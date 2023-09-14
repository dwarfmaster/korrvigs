#include "datalog.hpp"
#include "json.hpp"
#include "parser.hpp"
#include "souffle.hpp"
#include "typer.hpp"
#include <boost/asio.hpp>
#include <boost/phoenix/bind/bind_function.hpp>
#include <boost/program_options.hpp>
#include <boost/spirit/include/support_multi_pass.hpp>
#include <deque>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <mutex>
#include <sys/inotify.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/un.h>
#include <thread>
#include <unistd.h>

namespace po = boost::program_options;
namespace fs = std::filesystem;
using unix_socket = boost::asio::local::stream_protocol;

constexpr size_t EVENT_SIZE = sizeof(struct inotify_event);
constexpr size_t BUF_LENGTH = 512 * (EVENT_SIZE + 32);

constexpr unsigned handlers = 4;

struct facts_data {
  fs::path facts_dir;
  std::unordered_set<std::string> input;
  size_t usage;
};
struct connection_data {
  connection_data() = delete;
  connection_data(const fs::path &socket_path);
  boost::asio::io_context io_context;
  unix_socket::endpoint endpoint;
  unix_socket::acceptor acceptor;
  Program program;
  fs::path runtime_dir;
  std::deque<facts_data> facts;
  std::mutex facts_mutex;
};
void handle_connection(connection_data &, unsigned);

connection_data::connection_data(const fs::path &socket_path)
    : endpoint(socket_path), acceptor(io_context, endpoint) {}

int main(int argc, char *argv[]) {
  // Handle CLI arguments
  po::options_description desc("Allowed options");
  desc.add_options()("help", "produce help message")(
      "socket", po::value<fs::path>(), "unix socket to bind (mandatory)")(
      "wiki", po::value<fs::path>(), "path to wiki root (mandatory)")(
      "cache", po::value<fs::path>(),
      "path to path to use as cache (mandatory)")(
      "rules", po::value<fs::path>(), "datalog rules to always include")(
      "types", po::value<fs::path>(), "types file to load");

  po::variables_map vm;
  po::store(po::parse_command_line(argc, argv, desc), vm);
  po::notify(vm);
  Program program;

  if (vm.count("help")) {
    std::cout << desc << std::endl;
    return 0;
  }

  fs::path socket_path;
  if (vm.count("socket")) {
    socket_path = fs::absolute(vm["socket"].as<fs::path>());
    if (fs::exists(socket_path)) {
      fs::remove(socket_path);
    }
    fs::path socket_dir = socket_path;
    socket_dir.remove_filename();
    if (!fs::exists(socket_dir)) {
      fs::create_directories(socket_dir);
    }
  } else {
    std::cerr << "Socket path was not set\n\n" << desc << std::endl;
    return 1;
  }

  fs::path wiki_path;
  if (vm.count("wiki")) {
    wiki_path = fs::absolute(vm["wiki"].as<fs::path>());
  } else {
    std::cerr << "Wiki path was not set\n\n" << desc << std::endl;
    return 1;
  }

  fs::path runtime_path;
  if (vm.count("cache")) {
    runtime_path = fs::absolute(vm["cache"].as<fs::path>());
    fs::create_directories(runtime_path);
    for (const auto &entry : fs::directory_iterator(runtime_path)) {
      fs::remove_all(entry.path());
    }
  } else {
    std::cerr << "Cache path was not set\n\n" << desc << std::endl;
    return 1;
  }

  std::vector<datalog::Predicate> predicates;
  if (vm.count("types")) {
    fs::path types_path = vm["types"].as<fs::path>();
    std::ifstream ifs(types_path);
    if (!ifs) {
      std::cerr << "Could not open " << types_path << std::endl;
      return 1;
    }
    std::optional<std::vector<datalog::Predicate>> parsed =
        parse_types(boost::spirit::make_default_multi_pass(
                        std::istreambuf_iterator<char>{ifs}),
                    boost::spirit::make_default_multi_pass(
                        std::istreambuf_iterator<char>()));
    if (!parsed.has_value()) {
      std::cerr << "Failed to parse " << types_path << std::endl;
      return 1;
    }
    for (const datalog::Predicate &pred : *parsed) {
      program.declare(pred);
    }
  }

  if (vm.count("rules")) {
    fs::path rules_path = vm["rules"].as<fs::path>();
    std::ifstream ifs(rules_path);
    if (!ifs) {
      std::cerr << "Could not open " << rules_path << std::endl;
      return 1;
    }
    std::optional<std::vector<datalog::Rule>> parsed =
        parse_rules(boost::spirit::make_default_multi_pass(
                        std::istreambuf_iterator<char>{ifs}),
                    boost::spirit::make_default_multi_pass(
                        std::istreambuf_iterator<char>()));
    if (!parsed.has_value()) {
      std::cerr << "Failed to parse " << rules_path << std::endl;
      return 1;
    }
    for (const datalog::Rule &rule : *parsed) {
      program.add_rule(rule);
    }
  }

  if (program.has_conflict(std::cerr)) {
    return 1;
  }

  // Init cache
  fs::path cache_path_00 = runtime_path / "facts" / "00";
  fs::path cache_path_01 = runtime_path / "facts" / "01";
  fs::create_directories(cache_path_01);
  int ino = inotify_init1(IN_CLOEXEC);
  std::unordered_set<std::string> input =
      extract_facts(wiki_path, cache_path_01, ino);
  fs::copy(cache_path_01, cache_path_00);

  // Synchronisation data
  connection_data data(socket_path);
  data.runtime_dir = runtime_path;
  facts_data facts00;
  facts00.facts_dir = cache_path_00;
  facts00.input = input;
  facts00.usage = 0;
  facts_data facts01;
  facts01.facts_dir = cache_path_01;
  facts01.input = input;
  facts01.usage = 0;
  data.facts.emplace_front(std::move(facts00));
  data.facts.emplace_front(std::move(facts01));
  data.program = std::move(program);

  // Create threads to handle incoming connections
  std::thread handler1(
      boost::phoenix::bind(&handle_connection, boost::phoenix::ref(data), 0));
  std::thread handler2(
      boost::phoenix::bind(&handle_connection, boost::phoenix::ref(data), 1));
  std::thread handler3(
      boost::phoenix::bind(&handle_connection, boost::phoenix::ref(data), 2));
  std::thread handler4(
      boost::phoenix::bind(&handle_connection, boost::phoenix::ref(data), 3));

  // Manage wiki state
  char buffer[BUF_LENGTH];
  for (;;) {
    // We use the fact that reading is blocking
    ssize_t result = read(ino, buffer, BUF_LENGTH);
    if (result < 0) {
      if (errno == EINTR) {
        continue;
      } else {
        perror("read");
        return 1;
      }
    }

    bool reload = false;
    for (size_t i = 0; i < result;) {
      struct inotify_event *event = (struct inotify_event *)(buffer + i);
      if (event->len) {
        if (event->mask & IN_DELETE_SELF || event->mask & IN_CLOSE_WRITE) {
          reload = true;
          break;
        }
        if (event->mask & IN_CREATE || event->mask & IN_DELETE) {
          fs::path file(event->name);
          if (event->mask & IN_ISDIR || file.extension() == ".meta") {
            reload = true;
            break;
          }
        }
      }
      i += EVENT_SIZE + event->len;
    }
    if (!reload)
      continue;

    facts_data facts;
    facts.usage = 0;
    data.facts_mutex.lock();
    if (data.facts.back().usage == 0) {
      facts = std::move(data.facts.back());
      data.facts.pop_back();
      data.facts_mutex.unlock();
      for (const auto &entry : fs::directory_iterator(facts.facts_dir)) {
        fs::remove_all(entry.path());
      }
    } else {
      data.facts_mutex.unlock();
      for (unsigned count = 2;; ++count) {
        std::ostringstream oss;
        oss << std::setw(2) << std::setfill('0') << count;
        facts.facts_dir = runtime_path / "facts" / oss.str();
        if (!fs::exists(facts.facts_dir)) {
          fs::create_directories(facts.facts_dir);
          break;
        }
      }
    }
    close(ino);
    ino = inotify_init1(IN_CLOEXEC);
    facts.input = extract_facts(wiki_path, facts.facts_dir, ino);
    data.facts_mutex.lock();
    data.facts.push_front(std::move(facts));
    data.facts_mutex.unlock();
  }

  return 0;
}

void handle_connection(connection_data &data, unsigned id) {
  // Compute working dir
  std::ostringstream oss;
  oss << data.runtime_dir.native() << "/threads/" << id;
  fs::path working_dir = oss.str();
  fs::create_directories(working_dir);

  // The main loop of the thread
  std::string first_line;
  facts_data *facts = nullptr;
  unix_socket::iostream stream;
  for (;;) {
    facts = nullptr;
    data.acceptor.accept(stream.socket());

    // Parse query
    std::getline(stream, first_line);
    if (first_line == "QUERY") {
      std::optional<std::vector<datalog::Rule>> query =
          parse_rules(boost::spirit::make_default_multi_pass(
                          std::istreambuf_iterator<char>{stream}),
                      boost::spirit::make_default_multi_pass(
                          std::istreambuf_iterator<char>()));
      if (!query.has_value()) {
        stream << "ERROR\nCouldn't parse the query" << std::endl;
        goto finish;
      }

      // Setup program
      Program prog = data.program;
      for (const datalog::Rule &rule : *query) {
        prog.add_rule(rule);
      }
      if (!prog.fully_typed()) {
        stream << "ERROR\nCouldn't fully type the query" << std::endl;
        goto finish;
      }

      // Find facts directory
      // Very important: insertion and deletion at either end of a deque never
      // invalidates pointers or references to the rest of the elements.
      data.facts_mutex.lock();
      facts = &data.facts.front();
      facts->usage += 1;
      data.facts_mutex.unlock();

      // Run query
      std::vector<std::vector<datalog::Value>> result =
          run_query(prog.predicates(), facts->input, prog.rules(), working_dir,
                    facts->facts_dir);
      stream << "SUCCESS\n";
      for (const std::vector<datalog::Value> &row : result) {
        write_json(stream, row) << '\n';
      }
    }

  finish:
    // Release facts usage is acquired
    if (facts) {
      data.facts_mutex.lock();
      facts->usage -= 1;
      data.facts_mutex.unlock();
    }
    stream.flush();
    stream.close();
  }
}

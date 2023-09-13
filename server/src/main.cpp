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
    socket_path = vm["socket"].as<fs::path>();
    if (fs::exists(socket_path)) {
      fs::remove(socket_path);
    }
  } else {
    std::cerr << "Socket path was not set\n\n" << desc << std::endl;
    return 1;
  }

  fs::path wiki_path;
  if (vm.count("wiki")) {
    wiki_path = vm["wiki"].as<fs::path>();
  } else {
    std::cerr << "Wiki path was not set\n\n" << desc << std::endl;
    return 1;
  }

  fs::path runtime_path;
  if (vm.count("cache")) {
    runtime_path = vm["cache"].as<fs::path>();
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
  // TODO
  for (;;) {
    sleep(1);
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

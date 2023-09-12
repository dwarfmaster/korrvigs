#include "datalog.hpp"
#include "parser.hpp"
#include "souffle.hpp"
#include "typer.hpp"
#include <boost/spirit/include/support_multi_pass.hpp>
#include <iostream>
#include <sys/inotify.h>
#include <unistd.h>

int main(int argc, char *argv[]) {
  int ino = inotify_init1(IN_CLOEXEC);
  auto input = extract_facts("/home/luc/downloads/wiki", "tmp/facts", ino);

  std::optional<std::vector<datalog::Rule>> rules = parse_rules(
      boost::spirit::make_default_multi_pass(
          std::istreambuf_iterator<char>{std::cin}),
      boost::spirit::make_default_multi_pass(std::istreambuf_iterator<char>()));
  if (!rules.has_value()) {
    std::cerr << "Failed to parse input datalog" << std::endl;
    return 1;
  }

  Program prog;
  for (const datalog::Rule &rule : *rules) {
    prog.add_rule(rule);
  }
  if (!prog.fully_typed(std::cerr)) {
    return 1;
  }

  char buf[4096] __attribute__((aligned(__alignof__(struct inotify_event))));
  const struct inotify_event *event;
  ssize_t len;
  while (true) {
    std::vector<std::vector<datalog::Value>> result =
        run_query(prog.predicates(), input, prog.rules(), "tmp", "tmp/facts");
    std::cout << "Got " << result.size() << " results" << std::endl;
    for (const std::vector<datalog::Value> &vals : result) {
      for (const datalog::Value &val : vals) {
        std::cout << val << " ";
      }
      std::cout << "\n";
    }
    std::cout.flush();

    len = read(ino, buf, sizeof(buf));
    if (len == -1 && errno != EAGAIN) {
      perror("read");
      return 1;
    }
    for (char *ptr = buf; ptr < buf + len;
         ptr += sizeof(struct inotify_event) + event->len) {
      event = (const struct inotify_event *)buf;
      std::cout << "Even catched, regenerating" << std::endl;
      input = extract_facts("/home/luc/downloads/wiki", "tmp/facts", ino);
    }
  }

  return 0;
}

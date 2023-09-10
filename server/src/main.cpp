#include "datalog.hpp"
#include "parser.hpp"
#include "typer.hpp"
#include <boost/spirit/version.hpp>
#include <iostream>

int main(int argc, char *argv[]) {
  std::string rule_str = "test(A, B) :- test(B, A)."
                         "test(5, X) :- test(X, X)."
                         "test(\"toto\", 5).";
  std::optional<std::vector<datalog::Rule>> rules =
      parse_rules(rule_str.begin(), rule_str.end());
  if (rules.has_value()) {
    Program prog;
    for (const datalog::Rule &rule : *rules) {
      std::cout << "Adding rule" << std::endl;
      prog.add_rule(rule);
    }
    std::cout << "Checking type" << std::endl;
    prog.fully_typed(std::cout);
  } else {
    std::cout << "Failed" << std::endl;
  }
  return 0;
}

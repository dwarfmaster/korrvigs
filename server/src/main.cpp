#include "parser.hpp"
#include <boost/spirit/version.hpp>
#include <iostream>

int main(int argc, char *argv[]) {
  std::string rule_str =
      "size-of('73f09745-6e36-00e5-af6b-0dbd80b35c64#query', N, 31415e-4) :- "
      "name-of(U, N), instance-of(U, C), name-of(C, \"Entity\\\"\").";
  std::cout << "Parsing rule " << rule_str << "\n";
  std::optional<datalog::Rule> rule =
      parse_rule(rule_str.begin(), rule_str.end());
  if (rule.has_value()) {
    std::cout << "Success: " << rule->head.pred << "[" << rule->head.args.size()
              << "]"
              << " :- " << rule->body.size() << std::endl;
  } else {
    std::cout << "Failed" << std::endl;
  }
  return 0;
}

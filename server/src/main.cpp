#include "parser.hpp"
#include <boost/spirit/version.hpp>
#include <iostream>

int main(int argc, char *argv[]) {
  std::string rule_str =
      "\n\nsize-of,  '73f09745-6e36-00e5-af6b-0dbd80b35c64#query', \"toto\", "
      "31415e-4"
      "\n\n\nname-of, \"you\", \"U\"\n";
  std::cout << "Parsing rule " << rule_str << "\n";
  std::optional<std::vector<datalog::Prop>> rules =
      parse_csv(rule_str.begin(), rule_str.end());
  if (rules.has_value()) {
    std::cout << "Success, found " << rules->size() << " rules" << std::endl;
  } else {
    std::cout << "Failed" << std::endl;
  }
  return 0;
}

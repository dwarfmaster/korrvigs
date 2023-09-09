#include "datalog.hpp"
#include "parser.hpp"
#include <boost/spirit/version.hpp>
#include <iostream>

int main(int argc, char *argv[]) {
  std::string rule_str =
      "\n\nsize-of,  '73f09745-6e36-00e5-af6b-0dbd80b35c64#query', \"toto\", "
      "31415e-4"
      "\n\n\nname-of, self#'toto', \"U\"\n";
  std::cout << "Parsing rule " << rule_str << "\n";
  std::string entry_str = "c7013603-fca6-4bb8-b1df-b156c5ab9c4d#page17";
  datalog::Entry entry =
      parse_entry(entry_str.begin(), entry_str.end()).value();
  std::optional<std::vector<datalog::GroundedProp>> rules =
      parse_csv(rule_str.begin(), rule_str.end(), entry);
  if (rules.has_value()) {
    std::cout << "Success, found " << rules->size() << " rules" << std::endl;
    std::cout << "First one " << rules->at(0).pred << "["
              << rules->at(0).args.size() << "]" << std::endl;
    std::cout << "Second one " << rules->at(1).pred << "["
              << rules->at(1).args.size() << "]" << std::endl;
    std::cout << "Self? " << std::get<datalog::Entry>((*rules)[1].args[0])
              << std::endl;
  } else {
    std::cout << "Failed" << std::endl;
  }
  return 0;
}

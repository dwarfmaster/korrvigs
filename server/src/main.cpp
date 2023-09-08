#include "parser.hpp"
#include <boost/spirit/version.hpp>
#include <iostream>

int main(int argc, char *argv[]) {
  std::string entry_str = "73f09745-6e36-00e5-af6b-0dbd80b35c64#query";
  std::cout << "Parsing entry " << entry_str << "\n";
  std::optional<datalog::Entry> entry =
      parse_entry(entry_str.begin(), entry_str.end());
  if (entry.has_value()) {
    auto prefix = entry->uuid_prefix();
    std::cout << "Value: " << std::get<0>(prefix) << std::get<1>(prefix) << "/"
              << entry.value() << std::endl;
  } else {
    std::cout << "Failed" << std::endl;
  }
  return 0;
}

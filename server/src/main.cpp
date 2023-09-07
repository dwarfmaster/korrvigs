#include "parser.hpp"
#include <boost/spirit/version.hpp>
#include <iostream>

int main(int argc, char *argv[]) {
  std::cout << "To parse: ";
  std::string s;
  std::cin >> s;
  std::cout << "Parse result: " << std::boolalpha << parse(s.begin(), s.end())
            << std::endl;
  return 0;
}

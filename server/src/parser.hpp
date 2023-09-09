#ifndef DEF_PARSER_HPP
#define DEF_PARSER_HPP

#include "parser_impl.hpp"
#include <boost/spirit/include/qi.hpp>
#include <optional>
#include <vector>

template <typename It>
std::optional<datalog::Entry> parse_entry(It first, It last) {
  using namespace boost::spirit;
  datalog::Entry result;
  entry_grammar<It> entry_parser;
  bool r = qi::parse(first, last, entry_parser, result);
  if (r && first == last) {
    return result;
  } else {
    return {};
  }
}

template <typename It>
std::optional<std::vector<datalog::Rule>> parse_rules(It first, It last) {
  using namespace boost::spirit;
  std::vector<datalog::Rule> result;
  program_grammar<It> program_parser;
  bool r = qi::phrase_parse(first, last, program_parser, ascii::space, result);
  if (r && first == last) {
    return result;
  } else {
    std::cerr << "Failed at " << std::string(first, last) << std::endl;
    return {};
  }
}

template <typename It>
std::optional<std::vector<datalog::GroundedProp>>
parse_csv(It first, It last, const datalog::Entry &self) {
  using namespace boost::spirit;
  std::vector<datalog::GroundedProp> result;
  csv_grammar<It> csv_parser(self);
  bool r = qi::phrase_parse(first, last, csv_parser, ascii::blank, result);
  if (r && first == last) {
    return result;
  } else {
    std::cerr << "Failed at " << std::string(first, last) << std::endl;
    return {};
  }
}

#endif

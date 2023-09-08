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
  bool r = qi::phrase_parse(first, last, entry_parser, ascii::space, result);
  if (r && first == last) {
    return result;
  } else {
    return {};
  }
}

template <typename It>
std::optional<datalog::Rule> parse_rule(It first, It last) {
  using namespace boost::spirit;
  datalog::Rule result;
  rule_grammar<It> rule_parser;
  bool r = qi::phrase_parse(first, last, rule_parser, ascii::space, result);
  if (r && first == last) {
    return result;
  } else {
    std::cerr << "Failed at " << std::string(first, last) << std::endl;
    return {};
  }
}

#endif

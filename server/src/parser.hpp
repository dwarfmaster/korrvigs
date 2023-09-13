#ifndef DEF_PARSER_HPP
#define DEF_PARSER_HPP

#include "parser_impl.hpp"
#include <boost/spirit/include/qi.hpp>
#include <optional>
#include <vector>

template <typename It> It advance_with_last(It it, size_t n, It last) {
  for (size_t i = 0; i < n && it != last; ++i) {
    ++it;
  }
  return it;
}

template <typename V, typename Parser, typename It, typename Skipper,
          typename... Args>
std::optional<V> generic_parser(It first, It last, Skipper skipper,
                                Args... args) {
  using namespace boost::spirit;
  V result;
  Parser parser(args...);
  bool r = qi::phrase_parse(first, last, parser, skipper, result);
  if (r && first == last) {
    return result;
  } else {
    std::cerr << "Failed at "
              << std::string(first, advance_with_last(first, 50, last))
              << std::endl;
    return {};
  }
}

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
  return generic_parser<std::vector<datalog::Rule>, program_grammar<It>>(
      first, last, ascii::space);
}

template <typename It>
std::optional<std::vector<datalog::GroundedProp>>
parse_csv(It first, It last, const datalog::Entry &self) {
  return generic_parser<std::vector<datalog::GroundedProp>, csv_grammar<It>>(
      first, last, ascii::blank, self);
}

template <typename It>
std::optional<std::vector<datalog::Predicate>> parse_types(It first, It last) {
  return generic_parser<std::vector<datalog::Predicate>, types_grammar<It>>(
      first, last, ascii::blank);
}

template <typename It>
std::optional<std::vector<std::vector<datalog::Value>>>
parse_souffle_csv(It first, It last, const std::vector<datalog::Type> &type) {
  return generic_parser<std::vector<std::vector<datalog::Value>>,
                        souffle_csv_grammar<It>>(first, last, qi::char_(' '),
                                                 type);
}

#endif

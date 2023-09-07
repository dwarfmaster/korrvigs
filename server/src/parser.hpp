#ifndef DEF_PARSER_HPP
#define DEF_PARSER_HPP

#include <boost/spirit/include/qi.hpp>

template <typename It> bool parse(It first, It last) {
  using namespace boost::spirit;
  using ascii::space;
  using qi::double_;
  using qi::phrase_parse;

  bool r = phrase_parse(first, last, double_ >> *(',' >> double_), space);
  // If we didn't get a full parse
  if (first != last) {
    return false;
  }
  return r;
}

#endif

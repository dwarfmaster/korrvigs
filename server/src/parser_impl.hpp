#ifndef DEF_PARSER_IMPL_HPP
#define DEF_PARSER_IMPL_HPP

#include "datalog.hpp"
#include <boost/phoenix/operator.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/qi_uint.hpp>

using namespace boost::spirit;

template <typename Iterator, unsigned N, unsigned Shift>
qi::rule<Iterator, uint128_t(), ascii::space_type>
    hexN = qi::uint_parser<uint128_t, 16, N, N>()[_val = _1 << Shift];

template <typename Iterator>
struct entry_grammar
    : qi::grammar<Iterator, datalog::Entry(), ascii::space_type> {
  entry_grammar() : entry_grammar::base_type(entry) {
    uuid = hexN<Iterator, 8, 96>[_val = _1] > '-' >
           hexN<Iterator, 4, 80>[_val += _1] > '-' >
           hexN<Iterator, 4, 64>[_val += _1] > '-' >
           hexN<Iterator, 4, 48>[_val += _1] > '-' >
           hexN<Iterator, 12, 0>[_val += _1];
    ident = qi::alpha > *(qi::alnum | qi::char_("-_.?!~&|@()[]{}^"));
    entry %= (uuid > -('/' > ident) > -('#' > ident));
  }

  qi::rule<Iterator, uint128_t(), ascii::space_type> uuid;
  qi::rule<Iterator, std::string(), ascii::space_type> ident;
  qi::rule<Iterator, datalog::Entry(), ascii::space_type> entry;
};

#endif

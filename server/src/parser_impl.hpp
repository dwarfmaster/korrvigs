#ifndef DEF_PARSER_IMPL_HPP
#define DEF_PARSER_IMPL_HPP

#include "datalog.hpp"
#include <boost/phoenix/bind/bind_member_variable.hpp>
#include <boost/phoenix/operator.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/qi_uint.hpp>

using namespace boost::spirit;
using boost::phoenix::bind;

template <typename Iterator, unsigned N, unsigned Shift>
qi::rule<Iterator, uint128_t()>
    hexN = qi::uint_parser<uint128_t, 16, N, N>()[_val = _1 << Shift];

template <typename Iterator>
struct entry_grammar : qi::grammar<Iterator, datalog::Entry()> {
  entry_grammar() : entry_grammar::base_type(entry) {
    uuid = hexN<Iterator, 8, 96>[_val = _1] > '-' >
           hexN<Iterator, 4, 80>[_val += _1] > '-' >
           hexN<Iterator, 4, 64>[_val += _1] > '-' >
           hexN<Iterator, 4, 48>[_val += _1] > '-' >
           hexN<Iterator, 12, 0>[_val += _1];
    ident = qi::alpha > *(qi::alnum | qi::char_("-_.?!~&|@()[]{}^"));
    entry %= uuid > -('/' > ident) > -('#' > ident);
  }

  qi::rule<Iterator, uint128_t()> uuid;
  qi::rule<Iterator, std::string()> ident;
  qi::rule<Iterator, datalog::Entry()> entry;
};

template <typename Iterator>
struct rule_grammar
    : qi::grammar<Iterator, datalog::Rule(), ascii::space_type> {
  rule_grammar() : rule_grammar::base_type(rule) {
    string %= '"' > *(lit("\\\"")[_val = '"'] | (qi::char_ - '"')) > '"';
    value %= ('\'' > entry > '\'') | qi::double_ | string;
    var %= qi::lexeme[qi::upper > *(qi::alnum | qi::char_("-_"))];
    atom %= var | value;
    pred_name %= qi::lexeme[qi::lower > *(qi::alnum | qi::char_("-_"))];
    prop %= lexeme[pred_name] > '(' > (lexeme[atom] % ',') > ')';
    rule %= prop > -(lit(":-") > (prop % ',')) > '.';
  }

  entry_grammar<Iterator> entry;
  qi::rule<Iterator, std::string()> string;
  qi::rule<Iterator, datalog::Value()> value;
  qi::rule<Iterator, datalog::Variable()> var;
  qi::rule<Iterator, datalog::Atom()> atom;
  qi::rule<Iterator, std::string()> pred_name;
  qi::rule<Iterator, datalog::Prop(), ascii::space_type> prop;
  qi::rule<Iterator, datalog::Rule(), ascii::space_type> rule;
};

template <typename Iterator>
struct program_grammar
    : qi::grammar<Iterator, std::vector<datalog::Rule>(), ascii::space_type> {
  program_grammar() : program_grammar::base_type(prog) { prog %= *rule; }

  rule_grammar<Iterator> rule;
  qi::rule<Iterator, std::vector<datalog::Rule>(), ascii::space_type> prog;
};

template <typename Iterator>
struct csv_grammar : qi::grammar<Iterator, std::vector<datalog::GroundedProp>(),
                                 ascii::blank_type> {
  csv_grammar(const datalog::Entry &_self)
      : csv_grammar::base_type(csv), self_value(_self) {
    self = lit("self")[_val = self_value] >
           -(lit("/'") >
             rule.entry.ident[bind(&datalog::Entry::sub, _val) = _1] > '\'') >
           -(lit("#'") >
             rule.entry.ident[bind(&datalog::Entry::query, _val) = _1] > '\'');
    entry %= lexeme[rule.pred_name] > ',' > ((self | rule.value) % ',');
    csv %= *qi::eol > (entry % +qi::eol) > *qi::eol;
  }

  rule_grammar<Iterator> rule;
  datalog::Entry self_value;
  qi::rule<Iterator, datalog::Entry> self;
  qi::rule<Iterator, datalog::GroundedProp(), ascii::blank_type> entry;
  qi::rule<Iterator, std::vector<datalog::GroundedProp>(), ascii::blank_type>
      csv;
};

#endif

#ifndef DEF_PARSER_IMPL_HPP
#define DEF_PARSER_IMPL_HPP

#include "datalog.hpp"
#include <boost/phoenix/bind/bind_function.hpp>
#include <boost/phoenix/bind/bind_member_function.hpp>
#include <boost/phoenix/bind/bind_member_variable.hpp>
#include <boost/phoenix/operator.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/qi_uint.hpp>

using namespace boost::spirit;
namespace phoenix = boost::phoenix;

std::string decode(const std::vector<char> &);

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
    var_name %= qi::upper > *(qi::alnum | qi::char_("-_"));
    var %= var_name;
    atom %= var | value;
    pred_name %= qi::lower > *(qi::alnum | qi::char_("-_"));
    prop %= pred_name > '(' > (atom % ',') > ')';
    rule %= prop > -(lit(":-") > (prop % ',')) > '.';
  }

  entry_grammar<Iterator> entry;
  qi::rule<Iterator, std::string()> string;
  qi::rule<Iterator, datalog::Value()> value;
  qi::rule<Iterator, std::string()> var_name;
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
    self =
        lit("self")[_val = self_value] >
        -(lit("/'") >
          rule.entry.ident[phoenix::bind(&datalog::Entry::sub, _val) = _1] >
          '\'') >
        -(lit("#'") >
          rule.entry.ident[phoenix::bind(&datalog::Entry::query, _val) = _1] >
          '\'');
    entry %= lexeme[rule.pred_name] > ',' > ((self | rule.value) % ',');
    csv %= *qi::eol > -(entry % +qi::eol) > *qi::eol;
  }

  rule_grammar<Iterator> rule;
  datalog::Entry self_value;
  qi::rule<Iterator, datalog::Entry> self;
  qi::rule<Iterator, datalog::GroundedProp(), ascii::blank_type> entry;
  qi::rule<Iterator, std::vector<datalog::GroundedProp>(), ascii::blank_type>
      csv;
};

template <typename Iterator>
struct types_grammar : qi::grammar<Iterator, std::vector<datalog::Predicate>(),
                                   ascii::blank_type> {
  types_grammar() : types_grammar::base_type(csv) {
    type.add("entry", datalog::Type::Entry)("string", datalog::Type::String)(
        "number", datalog::Type::Number);
    entry %= lexeme[rule.pred_name] > ',' > (type % ',');
    csv %= *qi::eol > -(entry % +qi::eol) > *qi::eol;
  }

  rule_grammar<Iterator> rule;
  qi::symbols<char, datalog::Type> type;
  qi::rule<Iterator, datalog::Predicate(), ascii::blank_type> entry;
  qi::rule<Iterator, std::vector<datalog::Predicate>(), ascii::blank_type> csv;
};

using space_skipper = qi::literal_char<char_encoding::standard, false, false>;

template <typename Iterator>
struct souffle_csv_grammar
    : qi::grammar<Iterator, std::vector<std::vector<datalog::Value>>(),
                  space_skipper> {
  souffle_csv_grammar(const std::vector<datalog::Type> &type_)
      : souffle_csv_grammar::base_type(csv), type(type_) {
    string = (+qi::char_("0-9A-Za-z_.~%"))[_val = phoenix::bind(decode, _1)];
    uint32 %= qi::uint_parser<uint128_t, 10, 1, -1>();
    mstring %= lit("nil") | ('[' > string > ']');

    uuid_part = uint32[_val = _1 << 96] > ',' > uint32[_val += _1 << 64] > ',' >
                uint32[_val += _1 << 32] > ',' > uint32[_val += _1];
    entry = '[' > uuid_part > ',' > mstring > ',' > mstring > ']';

    values %= qi::eps[_a = 0] >
              (qi::lazy(phoenix::bind(
                   &souffle_csv_grammar<Iterator>::parse_col_value, this, _a)) %
               '\t');
    csv %= *qi::eol > -(values % +qi::eol) > *qi::eol;
  }

  qi::rule<Iterator, datalog::Value(), space_skipper>
  parse_col_value(size_t &col) const {
    if (col >= type.size()) {
      return qi::eps(false);
    } else {
      col += 1;
      switch (type[col - 1]) {
      case datalog::Type::Entry:
        return entry;
      case datalog::Type::String:
        return string;
      case datalog::Type::Number:
        return double_;
      }
    }
  }

  std::vector<datalog::Type> type;
  qi::rule<Iterator, std::string()> string;
  qi::rule<Iterator, uint128_t> uint32;
  qi::rule<Iterator, std::optional<std::string>(), space_skipper> mstring;
  qi::rule<Iterator, uint128_t(), space_skipper> uuid_part;
  qi::rule<Iterator, datalog::Entry(), space_skipper> entry;
  // qi::rule<Iterator, datalog::Value(), qi::locals<size_t>, space_skipper>
  // value;
  qi::rule<Iterator, std::vector<datalog::Value>(), qi::locals<size_t>,
           space_skipper>
      values;
  qi::rule<Iterator, std::vector<std::vector<datalog::Value>>(), space_skipper>
      csv;
};

#endif

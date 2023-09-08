#ifndef DEF_DATALOG_HPP
#define DEF_DATALOG_HPP

#include <boost/fusion/include/adapt_struct.hpp>
#include <optional>
#include <ostream>
#include <string>
#include <tuple>
#include <variant>
#include <vector>

using uint128_t = unsigned __int128;

namespace datalog {
struct Entry {
  Entry();
  Entry(uint128_t);
  std::tuple<char, char> uuid_prefix() const;

  uint128_t uuid;
  std::optional<std::string> sub;
  std::optional<std::string> query;
};
std::ostream &operator<<(std::ostream &, const Entry &);

using Value = std::variant<Entry, double, std::string>;
struct Variable {
  std::string name;
};
using Atom = std::variant<Variable, Value>;

enum class Type { Entry, Number, String };

struct Predicate {
  std::string name;
  std::vector<Type> args;
};

struct Prop {
  std::string pred;
  std::vector<Atom> args;
};

struct Rule {
  Predicate head;
  std::vector<Prop> body;
};
} // namespace datalog

BOOST_FUSION_ADAPT_STRUCT(datalog::Entry,
                          (uint128_t, uuid)(std::optional<std::string>,
                                            sub)(std::optional<std::string>,
                                                 query))
BOOST_FUSION_ADAPT_STRUCT(datalog::Variable, (std::string, name))
BOOST_FUSION_ADAPT_STRUCT(datalog::Predicate,
                          (std::string, name)(std::vector<datalog::Type>, args))
BOOST_FUSION_ADAPT_STRUCT(datalog::Prop,
                          (std::string, pred)(std::vector<datalog::Atom>, args))
BOOST_FUSION_ADAPT_STRUCT(datalog::Rule,
                          (datalog::Predicate, head)(std::vector<datalog::Prop>,
                                                     body))

#endif

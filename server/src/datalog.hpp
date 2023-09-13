#ifndef DEF_DATALOG_HPP
#define DEF_DATALOG_HPP

#include <array>
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
  std::array<uint32_t, 4> uuid_parts() const;

  uint128_t uuid;
  std::optional<std::string> sub;
  std::optional<std::string> query;
};
std::ostream &print_uuid(std::ostream &, uint128_t);
std::ostream &operator<<(std::ostream &, const Entry &);

using Value = std::variant<Entry, double, std::string>;
std::ostream &operator<<(std::ostream &, const Value &);
struct Variable {
  std::string name;
};
using Atom = std::variant<Variable, Value>;

enum class Type { Entry, Number, String };
Type get_value_type(const Value &);
std::ostream &operator<<(std::ostream &, const Type &);

struct Predicate {
  std::string name;
  std::vector<Type> args;
};

struct Prop {
  std::string pred;
  std::vector<Atom> args;
};

struct GroundedProp {
  std::string pred;
  std::vector<Value> args;
};

struct Rule {
  Prop head;
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
BOOST_FUSION_ADAPT_STRUCT(datalog::GroundedProp,
                          (std::string, pred)(std::vector<datalog::Value>,
                                              args))
BOOST_FUSION_ADAPT_STRUCT(datalog::Rule,
                          (datalog::Prop, head)(std::vector<datalog::Prop>,
                                                body))

#endif

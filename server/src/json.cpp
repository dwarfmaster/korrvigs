#include "json.hpp"
#include <type_traits>

std::ostream &write_json(std::ostream &os,
                         const std::vector<datalog::Value> &row) {
  os << "[ ";
  for (size_t col = 0; col < row.size(); ++col) {
    if (col > 0) {
      os << ", ";
    }
    std::visit(
        [&os](auto &&arg) {
          using T = std::decay_t<decltype(arg)>;
          if constexpr (std::is_same_v<T, datalog::Entry>) {
            os << "{ \"uuid\": \"";
            datalog::print_uuid(os, arg.uuid);
            os << "\", \"sub\": ";
            if (arg.sub.has_value()) {
              os << '"' << *arg.sub << '"';
            } else {
              os << "null";
            }
            os << ", \"query\": ";
            if (arg.query.has_value()) {
              os << '"' << *arg.query << '"';
            } else {
              os << "null";
            }
            os << " }";
          } else if constexpr (std::is_same_v<T, std::string>) {
            os << '"' << arg << '"';
          } else {
            os << arg;
          }
        },
        row[col]);
  }
  os << " ]";
  return os;
}

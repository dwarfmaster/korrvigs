#include "datalog.hpp"
#include <boost/format.hpp>

#define LOW_TIME(uuid) (uint64_t((uuid >> 96) & 0xFFFFFFFF))
#define MID_TIME(uuid) (uint64_t((uuid >> 80) & 0xFFFF))
#define VERSION(uuid) (uint64_t((uuid >> 64) & 0xFFFF))
#define VARIANT(uuid) (uint64_t((uuid >> 48) & 0xFFFF))
#define NODE(uuid) (uint64_t(uuid & 0xFFFFFFFFFFFF))

template <typename Int> char int_to_hex(Int i) {
  switch (i & 0xF) {
  case 0:
    return '0';
  case 1:
    return '1';
  case 2:
    return '2';
  case 3:
    return '3';
  case 4:
    return '4';
  case 5:
    return '5';
  case 6:
    return '6';
  case 7:
    return '7';
  case 8:
    return '8';
  case 9:
    return '9';
  case 10:
    return 'a';
  case 11:
    return 'b';
  case 12:
    return 'c';
  case 13:
    return 'd';
  case 14:
    return 'e';
  case 15:
    return 'f';
  default:
    return '0'; // Will never happen
  }
}

namespace datalog {
Entry::Entry() : uuid(0), sub({}), query({}) {}
Entry::Entry(uint128_t uuid) : uuid(uuid), sub({}), query({}) {}

std::ostream &operator<<(std::ostream &os, const Entry &e) {
  os << boost::format("%08x-%04x-%04x-%04x-%012x") % LOW_TIME(e.uuid) %
            MID_TIME(e.uuid) % VERSION(e.uuid) % VARIANT(e.uuid) % NODE(e.uuid);
  if (e.sub.has_value()) {
    os << "/" << e.sub.value();
  }
  if (e.query.has_value()) {
    os << "#" << e.query.value();
  }
  return os;
}

std::tuple<char, char> Entry::uuid_prefix() const {
  return std::make_tuple(int_to_hex(uuid >> 124), int_to_hex(uuid >> 120));
}

} // namespace datalog
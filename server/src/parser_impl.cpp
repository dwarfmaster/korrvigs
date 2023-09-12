#include "parser_impl.hpp"
#include <boost/url.hpp>

std::string decode(const std::vector<char> &encoded) {
  using namespace boost::urls;
  pct_string_view view(encoded.data(), encoded.size());
  std::string buf;
  buf.resize(view.decoded_size());
  view.decode({}, string_token::assign_to(buf));
  return buf;
}

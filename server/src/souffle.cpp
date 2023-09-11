#include "souffle.hpp"
#include "datalog.hpp"
#include "parser.hpp"
#include <boost/spirit/include/support_multi_pass.hpp>
#include <boost/url.hpp>
#include <fstream>
#include <ios>
#include <souffle/SouffleInterface.h>

namespace fs = std::filesystem;

std::string souflify_pred_name(const std::string &pred) {
  std::string r = pred;
  for (char &c : r) {
    if (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') ||
        ('0' <= c && c <= '9') || c == '_') {
      continue;
    }
    c = '_';
  }
  return r;
}

std::unordered_set<std::string> extract_facts(const fs::path &wiki,
                                              const fs::path &dst) {
  std::unordered_set<std::string> inputs;
  for (const auto &prefix : fs::directory_iterator(wiki)) {
    for (const auto &entry : fs::directory_iterator(prefix.path())) {
      if (!entry.is_directory()) {
        continue;
      }
      std::string dir_name = entry.path().filename();
      datalog::Entry self =
          parse_entry(dir_name.begin(), dir_name.end()).value();

      for (const auto &sub : fs::directory_iterator(entry.path())) {
        fs::path path = sub.path();
        if (!path.filename().empty() && path.filename().native()[0] == '.') {
          continue;
        }
        if (path.extension() == ".meta" && path.filename() != "default.meta") {
          continue;
        }
        if (path.filename() != "default.meta") {
          self.sub = path.filename();
        } else {
          self.sub = {};
        }
        fs::path meta = sub.path();
        meta.replace_extension("meta");

        if (!fs::exists(meta)) {
          // Create empty meta file
          std::ofstream(meta).flush();
          continue;
        }

        std::ifstream stream(meta);
        std::optional<std::vector<datalog::GroundedProp>> props =
            parse_csv(boost::spirit::make_default_multi_pass(
                          std::istreambuf_iterator<char>{stream}),
                      boost::spirit::make_default_multi_pass(
                          std::istreambuf_iterator<char>()),
                      self);
        if (!props.has_value()) {
          std::cerr << "Couldn't parse " << meta << " file" << std::endl;
          continue;
        }

        for (const datalog::GroundedProp &prop : *props) {
          fs::path csv_path = dst;
          csv_path /= souflify_pred_name(prop.pred);
          inputs.insert(prop.pred);
          csv_path.replace_extension("facts");
          std::ofstream csv(csv_path, std::ios_base::app);
          for (size_t arg = 0; arg < prop.args.size(); ++arg) {
            if (arg != 0) {
              csv << '\t';
            }
            std::visit(
                [&csv](auto &&arg) {
                  using T = std::decay_t<decltype(arg)>;
                  if constexpr (std::is_same_v<T, datalog::Entry>) {
                    std::array<uint32_t, 4> uuid_parts = arg.uuid_parts();
                    csv << "[" << uuid_parts[0] << "," << uuid_parts[1] << ","
                        << uuid_parts[2] << "," << uuid_parts[3] << ",";
                    if (arg.sub.has_value()) {
                      csv << "["
                          << boost::urls::encode(*arg.sub,
                                                 boost::urls::unreserved_chars)
                          << "]";
                    } else {
                      csv << "nil";
                    }
                    csv << ",";
                    if (arg.query.has_value()) {
                      csv << "["
                          << boost::urls::encode(*arg.query,
                                                 boost::urls::unreserved_chars)
                          << "]";
                    } else {
                      csv << "nil";
                    }
                    csv << "]";
                  } else if constexpr (std::is_same_v<T, std::string>) {
                    csv << boost::urls::encode(arg,
                                               boost::urls::unreserved_chars);
                  } else {
                    csv << arg;
                  }
                },
                prop.args[arg]);
          }
          csv << '\n';
        }
      }
    }
  }
  return inputs;
}

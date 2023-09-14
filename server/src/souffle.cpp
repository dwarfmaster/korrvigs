#include "souffle.hpp"
#include "datalog.hpp"
#include "parser.hpp"
#include <boost/spirit/include/support_multi_pass.hpp>
#include <boost/url.hpp>
#include <cstdlib>
#include <fstream>
#include <ios>
#include <sys/inotify.h>

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

void write_string(std::ostream &os, const std::string &str, bool in_dtl) {
  if (in_dtl) {
    os << '"';
  }
  os << boost::urls::encode(str, boost::urls::unreserved_chars);
  if (in_dtl) {
    os << '"';
  }
}

void write_entry(std::ostream &os, const datalog::Entry &entry, bool in_dtl) {
  std::array<uint32_t, 4> uuid_parts = entry.uuid_parts();
  os << '[' << uuid_parts[0] << ',' << uuid_parts[1] << ',' << uuid_parts[2]
     << ',' << uuid_parts[3] << ',';
  if (entry.sub.has_value()) {
    os << '[';
    write_string(os, *entry.sub, in_dtl);
    os << ']';
  } else {
    os << "nil";
  }
  os << ',';
  if (entry.query.has_value()) {
    os << '[';
    write_string(os, *entry.query, in_dtl);
    os << ']';
  } else {
    os << "nil";
  }
  os << ']';
}

std::unordered_set<std::string>
extract_facts(const fs::path &wiki, const fs::path &dst, int inotify) {
  int inotify_flags = IN_MASK_CREATE;
  if (inotify) {
    inotify_add_watch(inotify, wiki.c_str(),
                      inotify_flags | IN_CREATE | IN_DELETE);
  }

  std::unordered_set<std::string> inputs;
  for (const auto &prefix : fs::directory_iterator(wiki)) {
    if (!prefix.is_directory() ||
        prefix.path().filename().native().size() != 2) {
      continue;
    }
    if (inotify) {
      inotify_add_watch(inotify, prefix.path().c_str(),
                        inotify_flags | IN_CREATE | IN_DELETE | IN_DELETE_SELF);
    }

    for (const auto &entry : fs::directory_iterator(prefix.path())) {
      if (!entry.is_directory()) {
        continue;
      }
      if (inotify) {
        inotify_add_watch(inotify, entry.path().c_str(),
                          inotify_flags | IN_CREATE | IN_DELETE |
                              IN_DELETE_SELF);
      }
      std::string dir_name = entry.path().filename();
      datalog::Entry self;
      {
        std::optional<datalog::Entry> parsed =
            parse_entry(dir_name.begin(), dir_name.end());
        if (!parsed.has_value()) {
          // Directory is not an UUID
          continue;
        }
        self = std::move(parsed.value());
      }

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

        bool meta_exists = fs::exists(meta);
        if (!meta_exists) {
          // Create empty meta file
          std::ofstream(meta).flush();
        }
        if (inotify) {
          inotify_add_watch(inotify, meta.c_str(),
                            inotify_flags | IN_CLOSE_WRITE | IN_DELETE_SELF);
        }
        if (!meta_exists) {
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
                    write_entry(csv, arg, false);
                  } else if constexpr (std::is_same_v<T, std::string>) {
                    write_string(csv, arg, false);
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

void write_value(std::ostream &os, const datalog::Value &val) {
  std::visit(
      [&os](auto &&arg) {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (std::is_same_v<T, datalog::Entry>) {
          write_entry(os, arg, true);
        } else if constexpr (std::is_same_v<T, std::string>) {
          write_string(os, arg, true);
        } else {
          os << arg;
        }
      },
      val);
}

void write_prop(std::ostream &os, const datalog::Prop &prop) {
  std::string souffle_name = souflify_pred_name(prop.pred);
  os << souffle_name << '(';
  for (size_t arg = 0; arg < prop.args.size(); ++arg) {
    if (arg != 0) {
      os << ", ";
    }
    std::visit(
        [&os](auto &&arg) {
          using T = std::decay_t<decltype(arg)>;
          if constexpr (std::is_same_v<T, datalog::Variable>) {
            std::string var_name = souflify_pred_name(arg.name);
            os << var_name;
          } else {
            write_value(os, arg);
          }
        },
        prop.args[arg]);
  }
  os << ')';
}

std::vector<std::vector<datalog::Value>>
run_query(const std::vector<datalog::Predicate> &preds,
          const std::unordered_set<std::string> &inputs,
          const std::vector<datalog::Rule> &rules, const fs::path &working_dir,
          const fs::path &facts) {
  fs::path datalog_path = working_dir / "query.dtl";
  std::ofstream datalog(datalog_path, std::ios_base::trunc);
  if (!datalog) {
    std::cerr << "Couldn't open " << datalog_path << " for writing"
              << std::endl;
    return {};
  }

  datalog << ".type ident = [ value: symbol ]\n"
          << ".type entry = [ uuid1: unsigned, uuid2: unsigned, uuid3: "
             "unsigned, uuid4: unsigned, sub: ident, query: ident ]\n";

  // Declare all predicates
  bool has_query = false;
  std::vector<datalog::Type> query_type;
  for (const datalog::Predicate &pred : preds) {
    std::string souffle_name = souflify_pred_name(pred.name);
    datalog << ".decl " << souffle_name << '(';
    for (size_t arg = 0; arg < pred.args.size(); ++arg) {
      if (arg != 0) {
        datalog << ',';
      }
      datalog << "x" << arg << ": ";
      switch (pred.args[arg]) {
      case datalog::Type::Entry:
        datalog << "entry";
        break;
      case datalog::Type::Number:
        datalog << "float";
        break;
      case datalog::Type::String:
        datalog << "symbol";
        break;
      }
    }
    datalog << ")\n";
    if (inputs.find(pred.name) != inputs.end()) {
      datalog << ".input " << souffle_name << "\n";
    }
    if (pred.name == "query") {
      datalog << ".output query\n";
      has_query = true;
      query_type = pred.args;
    }
  }
  if (!has_query) {
    std::cerr << "No query predicate." << std::endl;
    return {};
  }

  // Add all rules
  for (const datalog::Rule &rule : rules) {
    write_prop(datalog, rule.head);
    for (size_t prop = 0; prop < rule.body.size(); ++prop) {
      if (prop == 0) {
        datalog << " :- ";
      } else {
        datalog << ", ";
      }
      write_prop(datalog, rule.body[prop]);
    }
    datalog << ".\n";
  }
  datalog.close();

  // Run souffle
  std::ostringstream cmd;
  cmd << "souffle -F" << facts << " -D" << working_dir << " --no-warn "
      << datalog_path << " > /dev/null 2>&1";
  int return_value = system(cmd.str().c_str());
  if (return_value != 0) {
    perror("exec souffle");
    std::cerr << "Souffle failed with error code " << return_value << std::endl;
    return {};
  }

  // Parse result
  fs::path result_path = working_dir / "query.csv";
  std::ifstream result_stream(result_path);
  if (!result_stream) {
    std::cerr << "Couldn't open " << result_path << std::endl;
    return {};
  }
  std::optional<std::vector<std::vector<datalog::Value>>> result =
      parse_souffle_csv(boost::spirit::make_default_multi_pass(
                            std::istreambuf_iterator<char>{result_stream}),
                        boost::spirit::make_default_multi_pass(
                            std::istreambuf_iterator<char>()),
                        query_type);
  if (!result.has_value()) {
    std::cerr << "Failed to parse " << result_path << std::endl;
    return {};
  }
  return *result;
}

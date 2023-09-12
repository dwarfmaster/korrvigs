#ifndef DEF_SOUFFLE_HPP
#define DEF_SOUFFLE_HPP

#include "datalog.hpp"
#include <filesystem>
#include <unordered_set>

// Given the path of the root of the wiki, and the path to an existing
// directory, extract all the facts in the wiki and write them to the directory
// in a way souffle understands.
std::unordered_set<std::string> extract_facts(const std::filesystem::path &,
                                              const std::filesystem::path &);

// Rule souffle on a set of rules, assuming each is fully typed. Takes the
// types, the set of predicates which have input facts, and the rules
// themselves. Returns the set of tuples for the query predicate. It also takes
// a path indicating in which directory it must put the temporary files, and
// the path to the fact directory.
std::vector<std::vector<datalog::Value>>
run_query(const std::vector<datalog::Predicate> &,
          const std::unordered_set<std::string> &,
          const std::vector<datalog::Rule> &, const std::filesystem::path &,
          const std::filesystem::path &);

#endif

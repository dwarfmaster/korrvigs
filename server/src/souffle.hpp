#ifndef DEF_SOUFFLE_HPP
#define DEF_SOUFFLE_HPP

#include <filesystem>
#include <unordered_set>

// Given the path of the root of the wiki, and the path to an existing
// directory, extract all the facts in the wiki and write them to the directory
// in a way souffle understands.
std::unordered_set<std::string> extract_facts(const std::filesystem::path &,
                                              const std::filesystem::path &);

#endif

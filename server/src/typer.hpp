#ifndef DEF_TYPER_HPP
#define DEF_TYPER_HPP

#include "datalog.hpp"
#include <map>
#include <optional>
#include <ostream>
#include <vector>

class Program {
public:
  // Extend the program
  // Returns false if adding the declaration/rule cause the program to get
  // stuck
  bool declare(const datalog::Predicate &);
  bool add_rule(const datalog::Rule &);

  // Check if it is fully typed without conflict
  bool fully_typed() const;
  // Check if fully typed and if not print error on stream
  bool fully_typed(std::ostream &) const;
  // Get the rules
  // Must NOT be called if fully_typed returned false
  std::vector<datalog::Rule> rules() const;
  // Get the typed predicates
  // Must NOT be called if fully_typed returned false
  std::vector<datalog::Predicate> predicates() const;

private:
  void init();
  size_t newCell();
  std::vector<size_t> &get_pred(const std::string &, unsigned);
  size_t query(size_t);
  size_t const_query(size_t) const;
  // Return false if the merge failed
  bool merge(size_t, size_t);
  size_t type_cell(datalog::Type) const;
  bool add_prop(const datalog::Prop &, std::map<std::string, size_t> &);

  struct Cell {
    size_t parent;
    uint64_t rank;
    std::optional<datalog::Type> type;
  };
  std::map<std::string, std::vector<size_t>> _predicates;
  std::vector<datalog::Rule> _rules;
  std::vector<Cell> _uf;

  struct Conflict {
    std::string pred;
    size_t arg;
    datalog::Type type1;
    datalog::Type type2;
  };
  std::optional<Conflict> _conflict;
};

#endif

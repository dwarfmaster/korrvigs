#include "typer.hpp"
#include <algorithm>
#include <type_traits>

bool Program::declare(const datalog::Predicate &pred) {
  if (_conflict.has_value()) {
    return false;
  }

  if (_uf.size() == 0) {
    init();
  }

  std::vector<size_t> &args = get_pred(pred.name, pred.args.size());
  for (size_t arg = 0; arg < args.size(); ++arg) {
    size_t other = type_cell(pred.args[arg]);
    if (!merge(args[arg], other)) {
      Conflict c = {
          .pred = pred.name,
          .arg = arg,
          .type1 = _uf[query(args[arg])].type.value(),
          .type2 = pred.args[arg],
      };
      _conflict = c;
      return false;
    }
  }
  return true;
}

bool Program::add_rule(const datalog::Rule &rule) {
  _rules.push_back(rule);
  if (_conflict.has_value()) {
    return false;
  }

  if (_uf.size() == 0) {
    init();
  }

  // Map from variable name to the corresponding cell
  std::map<std::string, size_t> variables;
  if (!add_prop(rule.head, variables)) {
    return false;
  }
  for (const datalog::Prop &prop : rule.body) {
    if (!add_prop(prop, variables)) {
      return false;
    }
  }
  return true;
}

bool Program::has_conflict() const { return _conflict.has_value(); }

bool Program::fully_typed() const {
  if (_conflict.has_value()) {
    return false;
  }
  for (auto [_, args] : _predicates) {
    for (size_t arg : args) {
      if (!_uf[const_query(arg)].type.has_value()) {
        return false;
      }
    }
  }
  return true;
}

bool Program::has_conflict(std::ostream &os) const {
  if (_conflict.has_value()) {
    os << "Predicate's " << _conflict->pred << " " << _conflict->arg
       << "th argument has both " << _conflict->type1 << " and "
       << _conflict->type2 << " types\n";
    return true;
  }
  return false;
}

bool Program::fully_typed(std::ostream &os) const {
  if (has_conflict(os)) {
    return false;
  }
  for (auto [pred, args] : _predicates) {
    for (size_t arg = 0; arg < args.size(); ++arg) {
      if (!_uf[const_query(args[arg])].type.has_value()) {
        os << "Predicate's " << pred << " " << arg
           << "th argument is not constrained\n";
        return false;
      }
    }
  }
  return true;
}

std::vector<datalog::Rule> Program::rules() const { return _rules; }

std::vector<datalog::Predicate> Program::predicates() const {
  std::vector<datalog::Predicate> preds;

  for (auto [name, args] : _predicates) {
    datalog::Predicate pred;
    pred.name = std::move(name);
    std::transform(
        args.begin(), args.end(), std::back_inserter(pred.args),
        [this](size_t id) { return _uf[const_query(id)].type.value(); });
    preds.push_back(pred);
  }
  return preds;
}

void Program::init() {
  if (_uf.size() > 0)
    return;
  _uf.push_back(Cell{.parent = 0, .rank = 0, .type = datalog::Type::Entry});
  _uf.push_back(Cell{.parent = 1, .rank = 0, .type = datalog::Type::String});
  _uf.push_back(Cell{.parent = 2, .rank = 0, .type = datalog::Type::Number});
}

size_t Program::newCell() {
  size_t id = _uf.size();
  Cell cell = {.parent = id, .rank = 0, .type = {}};
  _uf.push_back(cell);
  return id;
}

size_t Program::query(size_t id) {
  while (_uf[id].parent != id) {
    _uf[id].parent = _uf[_uf[id].parent].parent;
    id = _uf[id].parent;
  }
  return id;
}

size_t Program::const_query(size_t id) const {
  while (_uf[id].parent != id) {
    id = _uf[id].parent;
  }
  return id;
}

bool Program::merge(size_t c1, size_t c2) {
  size_t p1 = query(c1);
  size_t p2 = query(c2);
  if (p1 == p2) {
    return true;
  }

  if (_uf[p1].rank < _uf[p2].rank) {
    std::swap(p1, p2);
  }

  // Handle types
  if (!_uf[p1].type.has_value()) {
    _uf[p1].type = _uf[p2].type;
  } else if (_uf[p1].type.has_value() && _uf[p2].type.has_value()) {
    if (*_uf[p1].type != *_uf[p2].type) {
      return false;
    }
  }

  _uf[p2].parent = p1;
  if (_uf[p1].rank == _uf[p2].rank) {
    _uf[p1].rank += 1;
  }

  return true;
}

std::vector<size_t> &Program::get_pred(const std::string &pred,
                                       unsigned arity) {
  auto it = _predicates.find(pred);
  if (it != _predicates.end()) {
    return it->second;
  }
  std::vector<size_t> args;
  for (unsigned arg = 0; arg < arity; ++arg) {
    args.push_back(newCell());
  }
  _predicates[pred] = std::move(args);
  return _predicates[pred];
}

size_t Program::type_cell(datalog::Type tp) const {
  switch (tp) {
  case datalog::Type::Entry:
    return 0;
  case datalog::Type::String:
    return 1;
  case datalog::Type::Number:
    return 2;
  }
}

template <class> inline constexpr bool always_false_v = false;

bool Program::add_prop(const datalog::Prop &prop,
                       std::map<std::string, size_t> &vars) {
  std::vector<size_t> &args = get_pred(prop.pred, prop.args.size());
  for (size_t argn = 0; argn < prop.args.size(); ++argn) {
    bool r = std::visit(
        [&vars, &args, &prop, argn, this](auto &&arg) {
          using T = std::decay_t<decltype(arg)>;
          if constexpr (std::is_same_v<T, datalog::Variable>) {
            auto it = vars.find(arg.name);
            if (it != vars.end()) {
              if (!merge(it->second, args[argn])) {
                Conflict c = {
                    .pred = prop.pred,
                    .arg = argn,
                    .type1 = _uf[query(it->second)].type.value(),
                    .type2 = _uf[query(args[argn])].type.value(),
                };
                this->_conflict = c;
                return false;
              }
            } else {
              vars[arg.name] = args[argn];
            }
            return true;
          } else if constexpr (std::is_same_v<T, datalog::Value>) {
            datalog::Type tp = datalog::get_value_type(arg);
            size_t other = type_cell(tp);
            if (!merge(other, args[argn])) {
              Conflict c = {
                  .pred = prop.pred,
                  .arg = argn,
                  .type1 = _uf[query(args[argn])].type.value(),
                  .type2 = tp,
              };
              this->_conflict = c;
              return false;
            }
            return true;
          } else {
            static_assert(always_false_v<T>, "non-exhaustive visitor");
          }
        },
        prop.args[argn]);
    if (!r) {
      return false;
    }
  }
  return true;
}

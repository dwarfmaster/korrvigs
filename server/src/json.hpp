#ifndef DEF_JSON_HPP
#define DEF_JSON_HPP

#include "datalog.hpp"
#include <iostream>

std::ostream &write_json(std::ostream &, const std::vector<datalog::Value> &);

#endif

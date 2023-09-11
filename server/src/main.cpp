#include "datalog.hpp"
#include "parser.hpp"
#include "souffle.hpp"
#include "typer.hpp"
#include <boost/spirit/version.hpp>
#include <iostream>

int main(int argc, char *argv[]) {
  extract_facts("/home/luc/downloads/wiki", "tmp/facts");
  return 0;
}

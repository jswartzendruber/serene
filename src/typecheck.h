#ifndef TYPECHECK_H
#define TYPECHECK_H

#include "parser.h"

class TypeChecker {
 public:
  TypeChecker(std::vector<Function> ast);
  ~TypeChecker();

  void check();
};

#endif
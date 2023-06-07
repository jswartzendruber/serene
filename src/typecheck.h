#ifndef TYPECHECK_H
#define TYPECHECK_H

#include "parser.h"

class TypeCheckVis : public ASTVisitor {
 public:
  void visitReturnStatement(ReturnStatement *elem);
};

class TypeChecker {
 public:
  TypeChecker(std::vector<Function> ast);
  ~TypeChecker();

  void check();

 private:
  std::vector<Function> m_ast;
};

#endif
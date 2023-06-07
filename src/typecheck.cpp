#include "typecheck.h"

void TypeCheckVis::visitReturnStatement(ReturnStatement *elem) {
  std::cout << "return : " << debugPrintExpr(elem->m_value) << "\n";
}

TypeChecker::TypeChecker(std::vector<Function> ast) : m_ast(ast) {}
TypeChecker::~TypeChecker() {}

void TypeChecker::check() {
  TypeCheckVis visitor;
  for (Function f : m_ast) {
    visitor.visitFunction(&f);
  }
}
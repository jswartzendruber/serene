#include "typecheck.h"

void TypeCheckVisitor::visitReturnStatement(ReturnStatement *elem) {
    this->visitExpression(&elem->m_value);
}

void TypeCheckVisitor::visitValueExpression(ValueExpression *expr) {
    
}

void TypeCheckVisitor::visitBinaryExpression(BinaryExpression *expr) {

}

TypeChecker::TypeChecker(std::vector<Function> ast) : m_ast(ast) {}
TypeChecker::~TypeChecker() {}

void TypeChecker::check() {
  TypeCheckVisitor visitor;
  for (Function f : m_ast) {
    visitor.m_currFnType = f.m_returnType;
    visitor.visitFunction(&f);
  }
}
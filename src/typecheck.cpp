#include "typecheck.h"

void TypeCheckVisitor::visitReturnStatement(ReturnStatement *elem) {
  Expression::Type type = elem->m_value.m_type;
  if (type == Expression::Type::Value) {
    ValueExpression *vexpr =
        static_cast<ValueExpression *>(elem->m_value.m_expression);

    std::string_view type = primitiveTypeToString(vexpr->m_type);
    if (type != m_currFn->m_returnType) {
      throw TypeCheckException(
          "Expected type " + std::string(m_currFn->m_returnType) +
          ", found (type: " + std::string(type) +
          ", value: " + std::string(vexpr->valueString()) + ") in function " +
          std::string(m_currFn->m_name) + ".");
    }
  }
}

TypeChecker::TypeChecker(
    std::vector<Function> ast,
    std::unordered_map<std::string_view, std::string_view> symbolTable)
    : m_ast(ast), m_symbolTable(symbolTable) {}
TypeChecker::~TypeChecker() {}

void TypeChecker::check() {
  TypeCheckVisitor visitor;
  for (Function f : m_ast) {
    visitor.m_currFn = &f;
    visitor.visitFunction(&f);
  }
}
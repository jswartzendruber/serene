#include "typecheck.h"

void TypeCheckVisitor::visitReturnStatement(ReturnStatement *elem) {
  checkExpr(&elem->m_value);
}

void TypeCheckVisitor::checkExpr(Expression *expr) {
  if (expr->m_type == Expression::Type::Value) {
    ValueExpression *vexpr = static_cast<ValueExpression *>(expr->m_expression);

    std::string_view type = primitiveTypeToString(vexpr->m_type);
    if (type != m_currFn->m_returnType) {
      throw TypeCheckException(
          "Expected type " + std::string(m_currFn->m_returnType) +
          ", found (type: " + std::string(type) +
          ", value: " + std::string(vexpr->valueString()) + ") in function " +
          std::string(m_currFn->m_name) + ".");
    }
  } else if (expr->m_type == Expression::Type::BinOp) {
    BinaryExpression *vexpr =
        static_cast<BinaryExpression *>(expr->m_expression);
    checkBinaryExpr(vexpr);
  } else {
    assert(!"Unreachable return expression check");
  }
}

void TypeCheckVisitor::checkBinaryExpr(BinaryExpression *expr) {
  checkExpr(&expr->m_left);
  checkExpr(&expr->m_right);
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
#include "typecheck.h"

TypeCheckVisitor::TypeCheckVisitor(
    std::unordered_map<std::string_view, std::string_view> *symbolTable)
    : m_symbolTable(symbolTable) {}
TypeCheckVisitor::~TypeCheckVisitor() {}

void TypeCheckVisitor::visitReturnStatement(ReturnStatement *elem) {
  checkExpr(elem->m_value.get(), m_currFn->m_returnType);
}

void TypeCheckVisitor::visitLetStatement(LetStatement *stmt) {
  checkLetStatement(stmt);
}

void TypeCheckVisitor::checkLetStatement(LetStatement *stmt) {
  checkExpr(stmt->m_initialValue.get(), stmt->m_type);
}

void TypeCheckVisitor::checkExpr(Expression *expr,
                                 std::string_view expectedType) {
  if (expr->m_type == Expression::Type::Value) {
    ValueExpression *vexpr =
        static_cast<ValueExpression *>(expr->m_expression.get());
    std::string_view type;

    if (vexpr->m_type == ValueExpressionType::Call) {
      type = (*m_symbolTable)[std::get<FunctionCall>(vexpr->m_value).m_name];
    } else if (vexpr->m_type == ValueExpressionType::Ident) {
      type = (*m_currFn->m_env)[std::get<std::string_view>(vexpr->m_value)];
    } else {
      type = valueExpressionTypeToString(vexpr->m_type);
    }

    if (type != expectedType) {
      throw TypeCheckException("Expected type " + std::string(expectedType) +
                               ", found (type: " + std::string(type) +
                               ", value: " + std::string(vexpr->valueString()) +
                               ").");
    }
  } else if (expr->m_type == Expression::Type::BinOp) {
    BinaryExpression *vexpr =
        static_cast<BinaryExpression *>(expr->m_expression.get());
    checkBinaryExpr(vexpr, expectedType);
  } else {
    assert(!"Unreachable return expression check");
  }
}

void TypeCheckVisitor::checkBinaryExpr(BinaryExpression *expr,
                                       std::string_view expectedType) {
  checkExpr(expr->m_left.get(), expectedType);
  checkExpr(expr->m_right.get(), expectedType);
}

TypeChecker::TypeChecker(
    std::vector<Function> ast,
    std::unordered_map<std::string_view, std::string_view> symbolTable)
    : m_ast(std::move(ast)), m_symbolTable(symbolTable) {}
TypeChecker::~TypeChecker() {}

void TypeChecker::check() {
  TypeCheckVisitor visitor(&m_symbolTable);
  for (Function &f : m_ast) {
    visitor.m_currFn = &f;
    visitor.visitFunction(&f);
  }
}
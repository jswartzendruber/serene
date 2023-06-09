#include "typecheck.h"

TypeCheckVisitor::TypeCheckVisitor(
    std::unordered_map<std::string_view, std::shared_ptr<Function>>
        *symbolTable)
    : m_symbolTable(symbolTable) {}

void TypeCheckVisitor::visitReturnStatement(ReturnStatement *elem) {
  this->visitExpression(elem->m_value.get());
  checkExpr(elem->m_value.get(), m_currFn->m_returnType);
}

void TypeCheckVisitor::visitLetStatement(LetStatement *stmt) {
  checkLetStatement(stmt);
}

void TypeCheckVisitor::checkLetStatement(LetStatement *stmt) {
  checkExpr(stmt->m_initialValue.get(), stmt->m_type);
}

void TypeCheckVisitor::visitCallExpression(FunctionCall *call) {
  for (int i = 0; i < call->m_args.size(); i++) {
    std::shared_ptr<Function> callingFunction = (*m_symbolTable)[call->m_name];

    if (callingFunction->m_args.size() != call->m_args.size()) {
      throw TypeCheckException(
          "Function " + std::string(call->m_name) + " expects " +
          std::to_string(callingFunction->m_args.size()) + " args, got " +
          std::to_string(call->m_args.size()) + ".");
    }

    for (int i = 0; i < call->m_args.size(); i++) {
      Expression *callExpr = call->m_args[i].get();
      std::string_view expectedType = callingFunction->m_args[i].m_type;
      checkExpr(callExpr, expectedType);
    }
  }
}

void TypeCheckVisitor::checkExpr(Expression *expr,
                                 std::string_view expectedType) {
  if (expr->m_type == Expression::Type::Value) {
    ValueExpression *vexpr =
        static_cast<ValueExpression *>(expr->m_expression.get());
    std::string_view type;

    if (vexpr->m_type == ValueExpressionType::Call) {
      type = (*m_symbolTable)[std::get<FunctionCall>(vexpr->m_value).m_name]
                 .get()
                 ->m_returnType;
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
    std::vector<std::shared_ptr<Function>> ast,
    std::unordered_map<std::string_view, std::shared_ptr<Function>>
        *symbolTable)
    : m_ast(std::move(ast)), m_symbolTable(std::move(symbolTable)) {}

void TypeChecker::check() {
  TypeCheckVisitor visitor(m_symbolTable);
  for (int i = 0; i < m_ast.size(); i++) {
    Function *f = m_ast[i].get();
    visitor.m_currFn = f;
    visitor.visitFunction(f);
  }
}
#ifndef TYPECHECK_H
#define TYPECHECK_H

#include "parser.h"

class TypeCheckVisitor : public ASTVisitor {
 public:
  void visitReturnStatement(ReturnStatement *elem);
  void visitValueExpression(ValueExpression *expr);
  void visitBinaryExpression(BinaryExpression *expr);

  std::string_view m_currFnType;
};

class TypeChecker {
 public:
  TypeChecker(std::vector<Function> ast);
  ~TypeChecker();

  void check();

 private:
  std::vector<Function> m_ast;
};

class TypeCheckException : public std::exception {
 public:
  explicit TypeCheckException(const std::string &msg) : m_msg(msg) {}
  const char *what() const noexcept override { return m_msg.c_str(); }

 private:
  std::string m_msg;
};

#endif
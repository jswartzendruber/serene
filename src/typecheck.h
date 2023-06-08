#ifndef TYPECHECK_H
#define TYPECHECK_H

#include "parser.h"

class TypeCheckVisitor : public ASTVisitor {
 public:
  TypeCheckVisitor(std::unordered_map<std::string_view, std::string_view> *m_symbolTable);
  ~TypeCheckVisitor();

  void visitReturnStatement(ReturnStatement *elem);
  void checkExpr(Expression *expr);
  void checkBinaryExpr(BinaryExpression *expr);

  Function *m_currFn;
  std::unordered_map<std::string_view, std::string_view> *m_symbolTable;
};

class TypeChecker {
 public:
  TypeChecker(
      std::vector<Function> ast,
      std::unordered_map<std::string_view, std::string_view> symbolTable);
  ~TypeChecker();

  void check();

 private:
  std::vector<Function> m_ast;
  std::unordered_map<std::string_view, std::string_view> m_symbolTable;
};

class TypeCheckException : public std::exception {
 public:
  explicit TypeCheckException(const std::string &msg) : m_msg(msg) {}
  const char *what() const noexcept override { return m_msg.c_str(); }

 private:
  std::string m_msg;
};

#endif
#ifndef TYPECHECK_H
#define TYPECHECK_H

#include "parser.h"

class TypeCheckVisitor : public ASTVisitor {
 public:
  TypeCheckVisitor(
      std::unordered_map<std::string_view, std::shared_ptr<Function>>
          *m_symbolTable);
  ~TypeCheckVisitor();

  void visitReturnStatement(ReturnStatement *elem) override;
  void visitLetStatement(LetStatement *stmt) override;
  void visitCallExpression(FunctionCall *call) override;

  void checkExpr(Expression *expr, std::string_view expectedType);
  void checkBinaryExpr(BinaryExpression *expr, std::string_view expectedType);
  void checkLetStatement(LetStatement *stmt);

  Function *m_currFn;
  std::unordered_map<std::string_view, std::shared_ptr<Function>>
      *m_symbolTable;
};

class TypeChecker {
 public:
  TypeChecker(std::vector<std::shared_ptr<Function>> ast,
              std::unordered_map<std::string_view, std::shared_ptr<Function>>
                  *symbolTable);
  ~TypeChecker();

  void check();

 private:
  std::vector<std::shared_ptr<Function>> m_ast;
  std::unordered_map<std::string_view, std::shared_ptr<Function>>
      *m_symbolTable;
};

class TypeCheckException : public std::exception {
 public:
  explicit TypeCheckException(const std::string &msg) : m_msg(msg) {}
  const char *what() const noexcept override { return m_msg.c_str(); }

 private:
  std::string m_msg;
};

#endif
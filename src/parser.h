#ifndef PARSER_H
#define PARSER_H

#include <cassert>
#include <exception>
#include <iostream>
#include <optional>
#include <string>
#include <vector>

#include "token.h"

class Parser;
class ParseException;

class Function;
class TypedValue;

class Expression;
class BaseExpression;

template <typename T>
class ValueExpression;

class BinaryExpression;

class Statement;
class IfStatement;
class BaseStatement;
class ReturnStatement;

class ASTVisitor;
class TypeCheckVis;

template <typename T>
std::string debugPrintExpr(Expression expr);

void walkFunction(ASTVisitor *visitor, Function *function);
void walkExpression(ASTVisitor *visitor, Expression *expr);
void walkStatement(ASTVisitor *visitor, Statement *stmt);
void walkIfStatement(ASTVisitor *visitor, IfStatement *stmt);
void walkReturnStatement(ASTVisitor *visitor, ReturnStatement *stmt);

class ASTVisitor {
 public:
  virtual void visitFunction(Function *function);
  virtual void visitExpression(Expression *expr);
  virtual void visitStatement(Statement *stmt);
  virtual void visitIfStatement(IfStatement *stmt);
  virtual void visitReturnStatement(ReturnStatement *stmt);
};

class Parser {
 public:
  Parser(std::vector<Token> tokens);
  ~Parser();

  std::vector<Function> parse();

 private:
  std::vector<Token> m_tokens;
  int m_idx;

  Token peek();
  bool at(TokenType type);
  bool atIdentifier(std::string expected);
  Token expect(TokenType type);
  std::string_view expectIdentifier();
  std::string_view expectIdentifier(std::string expected);
  Function parseFunction();
  Statement parseStatement();

  Expression parseExpression();
  Expression parseExpressionBP(int bp);

  TypedValue parseFunctionArgument();
};

class ParseException : public std::exception {
 public:
  explicit ParseException(const std::string &msg) : m_msg(msg) {}
  const char *what() const noexcept override { return m_msg.c_str(); }

 private:
  std::string m_msg;
};

class TypedValue {
 public:
  TypedValue(std::string_view name, std::string_view type);
  ~TypedValue();

  std::string_view m_name;
  std::string_view m_type;
};

class BaseExpression {};

class Expression {
 public:
  enum Type {
    Value,
    BinOp,
  };

  Expression();
  Expression(Type type, BaseExpression *expression);
  ~Expression();

  Type m_type;
  BaseExpression *m_expression;
};

class BinaryExpression : public BaseExpression {
 public:
  enum Type {
    Add,
    Sub,
    Mul,
    Div,
    Compare,
  };

  BinaryExpression(Type type, Expression left, Expression right);
  ~BinaryExpression();

  static int infixBP(Type op);

  Type m_type;
  Expression m_left;
  Expression m_right;
};

template <typename T>
class ValueExpression : public BaseExpression {
 public:
  ValueExpression(T value);
  ~ValueExpression();

  T m_value;
};

class BaseStatement {};

class Statement {
 public:
  enum Type {
    Return,
    If,
  };

  Statement(Type type, BaseStatement *statement);
  ~Statement();

  Type m_type;
  BaseStatement *m_statement;
};

class IfStatement : public BaseStatement {
 public:
  IfStatement(Expression condition, std::vector<Statement> bodyIfTrue,
              std::optional<std::vector<Statement>> bodyIfFalse);
  ~IfStatement();

  Expression m_condition;
  std::vector<Statement> m_bodyIfTrue;
  std::optional<std::vector<Statement>> m_bodyIfFalse;
};

class ReturnStatement : public BaseStatement {
 public:
  ReturnStatement(Expression value);
  ~ReturnStatement();

  Expression m_value;
};

class Function {
 public:
  Function(std::string_view name, std::vector<TypedValue> args,
           std::string_view returnType, std::vector<Statement> statements);
  ~Function();

  std::string_view m_name;
  std::vector<TypedValue> m_args;
  std::string_view m_returnType;
  std::vector<Statement> m_statements;
};

template <typename T>
std::string debugPrintExpr(Expression expr) {
  std::string s;

  if (expr.m_type == Expression::Type::BinOp) {
    BinaryExpression *opExpr =
        static_cast<BinaryExpression *>(expr.m_expression);
    BinaryExpression::Type op = opExpr->m_type;
    std::string opstr;

    if (op == BinaryExpression::Add) {
      opstr = " + ";
    } else if (op == BinaryExpression::Sub) {
      opstr = " - ";
    } else if (op == BinaryExpression::Mul) {
      opstr = " * ";
    } else if (op == BinaryExpression::Div) {
      opstr = " / ";
    } else if (op == BinaryExpression::Compare) {
      opstr = " == ";
    } else {
      opstr = " ? ";
    }

    s += "( " + debugPrintExpr<T>(opExpr->m_left) + opstr +
         debugPrintExpr<T>(opExpr->m_right) + " )";

  } else if (expr.m_type == Expression::Type::Value) {
    ValueExpression<T> *vExpr =
        static_cast<ValueExpression<T> *>(expr.m_expression);
    s += std::to_string(vExpr->m_value);
  }

  return s;
}

#endif
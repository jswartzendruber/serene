#ifndef PARSER_H
#define PARSER_H

#include <exception>
#include <iostream>
#include <optional>
#include <string>
#include <vector>

#include "token.h"

class TypedValue {
 public:
  TypedValue(std::string_view name, std::string_view type)
      : m_name(name), m_type(type) {}
  ~TypedValue() {}

  std::string_view m_name;
  std::string_view m_type;
};

class BaseExpression {};

class Expression {
 public:
  enum Type {
    Value,
    Op,
  };

  Expression() {}
  Expression(Type type, BaseExpression* expression)
      : m_type(type), m_expression(expression) {}
  ~Expression() {}

  Type m_type;
  BaseExpression* m_expression;
};

class ValueExpression : public BaseExpression {
 public:
  ValueExpression(int value) : m_value(value) {}
  ~ValueExpression() {}

  int m_value;
};

class OpExpression : public BaseExpression {
 public:
  enum Type {
    Add,
    Sub,
    Mul,
    Div,
    Compare,
  };

  OpExpression(Type type, Expression left, Expression right)
      : m_type(type), m_left(left), m_right(right) {}
  ~OpExpression() {}

  static int infixBP(Type op) {
    switch (op) {
      case Compare:
        return 10;

      case Add:
      case Sub:
        return 20;

      case Mul:
      case Div:
        return 30;

      default:
        return 0;
    }
  }

  Type m_type;
  Expression m_left;
  Expression m_right;
};

class BaseStatement {};

class Statement {
 public:
  enum Type {
    Return,
    If,
  };

  Statement(Type type, BaseStatement* statement)
      : m_type(type), m_statement(statement) {}
  ~Statement() {}

  Type m_type;
  BaseStatement* m_statement;
};

class ReturnStatement : public BaseStatement {
 public:
  ReturnStatement(Expression value) : m_value(value) {}
  ~ReturnStatement() {}

  Expression m_value;
};

class IfStatement : public BaseStatement {
 public:
  IfStatement(Expression condition, std::vector<Statement> bodyIfTrue,
              std::optional<std::vector<Statement>> bodyIfFalse)
      : m_condition(condition),
        m_bodyIfTrue(bodyIfTrue),
        m_bodyIfFalse(bodyIfFalse) {}
  ~IfStatement() {}

  Expression m_condition;
  std::vector<Statement> m_bodyIfTrue;
  std::optional<std::vector<Statement>> m_bodyIfFalse;
};

class Function {
 public:
  Function(std::string_view name, std::vector<TypedValue> args,
           std::string_view returnType, std::vector<Statement> statements)
      : m_name(name),
        m_args(args),
        m_returnType(returnType),
        m_statements(statements) {}
  ~Function() {}

  std::string_view m_name;
  std::vector<TypedValue> m_args;
  std::string_view m_returnType;
  std::vector<Statement> m_statements;
};

class Parser {
 public:
  Parser(std::vector<Token> tokens);
  ~Parser(){};

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
  explicit ParseException(const std::string& msg) : m_msg(msg) {}

  const char* what() const noexcept override { return m_msg.c_str(); }

 private:
  std::string m_msg;
};

std::string debugPrintExpr(Expression expr);

#endif
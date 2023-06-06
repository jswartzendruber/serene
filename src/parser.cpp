#include "parser.h"

Parser::Parser(std::vector<Token> tokens) : m_tokens(tokens) { m_idx = 0; }

Token Parser::peek() {
  if (m_idx + 1 < m_tokens.size()) {
    return m_tokens[m_idx];
  } else {
    return m_tokens[m_idx];
  }
}

Token Parser::expect(TokenType type) {
  Token t = m_tokens[m_idx];
  if (m_idx < m_tokens.size() && t.m_type == type) {
    m_idx++;
    return t;
  } else {
    throw ParseException("Expected " + tokenTypeToString(type) + ", got " +
                         tokenTypeToString(t.m_type) + " on line " +
                         std::to_string(t.m_line) + ".");
  }
}

std::string_view Parser::expectIdentifier() {
  return expect(TokenType::Identifier).m_src;
}

std::string_view Parser::expectIdentifier(std::string expected) {
  Token token = expect(TokenType::Identifier);

  if (token.m_src.compare(expected) == 0) {
    return token.m_src;
  } else {
    throw ParseException("Expected identifier '" + expected + "', got '" +
                         std::string(token.m_src) + "' on line " +
                         std::to_string(token.m_line) + ".");
  }
}

bool Parser::at(TokenType type) {
  return m_idx < m_tokens.size() && m_tokens[m_idx].m_type == type;
}

bool Parser::atIdentifier(std::string expected) {
  if (at(TokenType::Identifier)) {
    return peek().m_src.compare(expected) == 0;
  } else {
    return false;
  }
}

std::vector<Function> Parser::parse() {
  std::vector<Function> fns;
  while (!at(TokenType::Eof)) {
    fns.push_back(parseFunction());
  }
  return fns;
}

TypedValue Parser::parseFunctionArgument() {
  std::string_view argName = expectIdentifier();
  expect(TokenType::Colon);
  std::string_view argType = expectIdentifier();
  if (peek().m_type != TokenType::RParen) {
    expect(TokenType::Comma);
  }
  return TypedValue(argName, argType);
}

std::string debugPrintExpr(Expression expr) {
  std::string s;

  if (expr.m_type == Expression::Type::Op) {
    OpExpression *opExpr = static_cast<OpExpression *>(expr.m_expression);
    OpExpression::Type op = opExpr->m_type;
    std::string opstr;

    if (op == OpExpression::Add) {
      opstr = " + ";
    } else if (op == OpExpression::Sub) {
      opstr = " - ";
    } else if (op == OpExpression::Mul) {
      opstr = " * ";
    } else if (op == OpExpression::Div) {
      opstr = " / ";
    } else if (op == OpExpression::Compare) {
      opstr = " == ";
    } else {
      opstr = " ? ";
    }

    s += "( " + debugPrintExpr(opExpr->m_left) + opstr +
         debugPrintExpr(opExpr->m_right) + " )";

  } else if (expr.m_type == Expression::Type::Value) {
    ValueExpression *vExpr = static_cast<ValueExpression *>(expr.m_expression);
    s += std::to_string(vExpr->m_value);
  }

  return s;
}

Statement Parser::parseStatement() {
  if (atIdentifier("return")) {
    expectIdentifier("return");
    Expression expr = parseExpression();
    expect(TokenType::Semicolon);

    return Statement(Statement::Type::Return, new ReturnStatement(expr));
  } else if (atIdentifier("if")) {
    expectIdentifier("if");
    Expression condition = parseExpression();

    expect(TokenType::LCurly);
    std::vector<Statement> bodyIfTrue;
    while (!at(TokenType::RCurly)) {
      bodyIfTrue.push_back(parseStatement());
    }
    expect(TokenType::RCurly);

    std::optional<std::vector<Statement>> bodyIfFalse = std::nullopt;
    if (atIdentifier("else")) {
      expectIdentifier("else");

      expect(TokenType::LCurly);
      std::vector<Statement> tmpBody;
      while (!at(TokenType::RCurly)) {
        tmpBody.push_back(parseStatement());
      }
      expect(TokenType::RCurly);

      bodyIfFalse = tmpBody;
    }

    return Statement(Statement::Type::If,
                     new IfStatement(condition, bodyIfTrue, bodyIfFalse));
  } else {
    Token curr = peek();
    throw ParseException("Expected statement, got " + std::string(curr.m_src) +
                         " on line " + std::to_string(curr.m_line));
  }
}

Function Parser::parseFunction() {
  expectIdentifier("fn");
  std::string_view functionName = expectIdentifier();

  expect(TokenType::LParen);
  std::vector<TypedValue> args;
  while (!at(TokenType::RParen)) {
    args.push_back(parseFunctionArgument());
  }
  expect(TokenType::RParen);

  // TODO: optional return type
  expect(TokenType::Arrow);
  std::string_view returnType = expectIdentifier();

  expect(TokenType::LCurly);
  std::vector<Statement> statements;
  while (!at(TokenType::RCurly)) {
    statements.push_back(parseStatement());
  }
  expect(TokenType::RCurly);

  return Function(functionName, args, returnType, statements);
}

Expression Parser::parseExpression() { return parseExpressionBP(0); }

Expression Parser::parseExpressionBP(int minBP) {
  Expression lhs;

  if (at(TokenType::Integer)) {
    Token t = expect(TokenType::Integer);
    int value = std::stoi(std::string(t.m_src));
    lhs = Expression(Expression::Type::Value, new ValueExpression(value));
  } else if (at(TokenType::LParen)) {
    expect(TokenType::LParen);
    lhs = parseExpression();
    expect(TokenType::RParen);
  } else {
    throw ParseException("Expected part of expression, got '" +
                         std::string(peek().m_src) + "'.");
  }

  while (true) {
    OpExpression::Type op;
    Token curr = peek();
    if (curr.m_type == TokenType::Plus) {
      op = OpExpression::Type::Add;
    } else if (curr.m_type == TokenType::Minus) {
      op = OpExpression::Type::Sub;
    } else if (curr.m_type == TokenType::Star) {
      op = OpExpression::Type::Mul;
    } else if (curr.m_type == TokenType::Slash) {
      op = OpExpression::Type::Div;
    } else if (curr.m_type == TokenType::EqEq) {
      op = OpExpression::Type::Compare;
    } else {
      break;
    }

    int lbp = OpExpression::infixBP(op);

    if (lbp > 0) {
      if (lbp < minBP) {
        break;
      }

      m_idx++;  // Skip over op
      Expression rhs = parseExpressionBP(lbp + 1);
      lhs = Expression(Expression::Type::Op, new OpExpression(op, lhs, rhs));
      continue;
    }
  }

  return lhs;
}

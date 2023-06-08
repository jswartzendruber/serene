#ifndef TOKEN_H
#define TOKEN_H

#include <iostream>
#include <string>
#include <string_view>

enum class TokenType {
  Identifier,
  Semicolon,
  PipePipe,
  Integer,
  String,
  LParen,
  RParen,
  LCurly,
  RCurly,
  Minus,
  Slash,
  Arrow,
  Colon,
  Comma,
  Float,
  Plus,
  Star,
  EqEq,
  Eof,
  Eq,
  Or,
};

class Token {
 public:
  Token(TokenType type, std::string_view src, int line);
  ~Token();

  void debug_display();

  std::string_view m_src;
  TokenType m_type;
  int m_line;
};

std::string tokenTypeToString(TokenType type);

#endif
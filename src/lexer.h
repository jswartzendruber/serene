#ifndef LEXER_H
#define LEXER_H

#include <iostream>
#include <string_view>
#include <vector>

#include "token.h"

class Lexer {
 public:
  Lexer(std::string_view fileSrc);
  ~Lexer();

  std::vector<Token> lex();

  std::string_view m_src;

 private:
  std::vector<Token> m_tokens;
  int m_line;
  int m_idx;

  void addToken(TokenType type, int length);
};

#endif
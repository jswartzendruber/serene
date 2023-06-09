#include <fstream>
#include <iostream>
#include <sstream>

#include "lexer.h"
#include "parser.h"
#include "token.h"
#include "typecheck.h"

std::string readFile(const std::string &filename) {
  std::ifstream file(filename);
  if (!file) {
    std::cerr << "File could not be opened.\n";
    std::exit(1);
    return {};
  }

  std::stringstream buffer;
  buffer << file.rdbuf();

  return buffer.str();
}

int main(int argc, char **argv) {
  if (argc < 2) {
    std::cerr << "File name expected.\n";
    std::exit(1);
  }

  std::string fileContents = readFile(argv[1]);
  std::string_view fileSrc(fileContents);

  Lexer lexer(fileSrc);
  std::vector<Token> tokens = lexer.lex();

  Parser parser(tokens);
  std::vector<std::shared_ptr<Function>> ast;
  try {
    ast = std::move(parser.parse());
  } catch (ParseException &e) {
    std::cout << e.what() << "\n";
    std::exit(1);
  }

  TypeChecker tc(ast, &parser.m_symbolTable);
  try {
    tc.check();
  } catch (TypeCheckException &e) {
    std::cout << e.what() << "\n";
    std::exit(1);
  }

  return 0;
}
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

#include "lexer.h"
#include "parser.h"
#include "token.h"

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

  // for (Token t : tokens) {
  //   t.debug_display();
  // }

  Parser parser(tokens);
  std::vector<Function> ast;
  try {
    ast = parser.parse();
  } catch (ParseException &e) {
    std::cout << e.what() << "\n";
    std::exit(1);
  }

  for (Function f : ast) {
    std::cout << "Name: " << f.m_name << ", Type: " << f.m_returnType << "\n";
  }

  BaseStatement *baseStmt = ast[0].m_statements[0].m_statement;
  if (ast[0].m_statements[0].m_type == Statement::Type::If) {
    IfStatement *stmt = static_cast<IfStatement *>(baseStmt);
    std::cout << debugPrintExpr(stmt->m_condition) << "\n";
  }

  return 0;
}
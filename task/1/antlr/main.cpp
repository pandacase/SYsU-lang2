#include "SYsU_lang.h" // make sure the name is same as .g4
#include <fstream>
#include <iostream>
#include <unordered_map>

// definition mapping，ANTLR tokenTypeName -> clang format
std::unordered_map<std::string, std::string> tokenTypeMapping = {
  { "Int", "int" },
  { "Identifier", "identifier" },
  { "LeftParen", "l_paren" },
  { "RightParen", "r_paren" },
  { "RightBrace", "r_brace" },
  { "LeftBrace", "l_brace" },
  { "LeftBracket", "l_square" },
  { "RightBracket", "r_square" },
  { "Constant", "numeric_constant" },
  { "Return", "return" },
  { "Semi", "semi" },
  { "EOF", "eof" },
  { "Equal", "equal" },
  { "Plus", "plus" },
  { "Comma", "comma" },

  // more...
};

void print_token(
      const antlr4::Token* token,
      const antlr4::CommonTokenStream& tokens,
      std::ofstream& outFile,
      const antlr4::Lexer& lexer) {
  auto& vocabulary = lexer.getVocabulary();

  auto tokenTypeName =
    std::string(vocabulary.getSymbolicName(token->getType()));

  if (tokenTypeName.empty())
    tokenTypeName = "<UNKNOWN>"; // empty string

  if (tokenTypeMapping.find(tokenTypeName) != tokenTypeMapping.end()) {
    tokenTypeName = tokenTypeMapping[tokenTypeName];
  }
  std::string locInfo = " Loc=<0:0>";

  bool startOfLine = false;
  bool leadingSpace = false;

  if (token->getText() != "<EOF>")
    outFile << tokenTypeName << " '" << token->getText() << "'";
  else
    outFile << tokenTypeName << " '"
            << "'";
  if (startOfLine)
    outFile << "\t [StartOfLine]";
  if (leadingSpace)
    outFile << " [LeadingSpace]";
  outFile << locInfo << std::endl;
}

int main(int argc, char* argv[]) {
  if (argc != 3) {
    std::cout << "Usage: " << argv[0] << " <input> <output>\n";
    return -1;
  }

  std::ifstream inFile(argv[1]);
  if (!inFile) {
    std::cout << "Error: unable to open input file: " << argv[1] << '\n';
    return -2;
  }

  std::ofstream outFile(argv[2]);
  if (!outFile) {
    std::cout << "Error: unable to open output file: " << argv[2] << '\n';
    return -3;
  }

  std::cout << "Program '" << argv[0] << std::endl;
  std::cout << "Input   '" << argv[1] << std::endl;
  std::cout << "Output  '" << argv[2] << std::endl;

  antlr4::ANTLRInputStream input(inFile);
  SYsU_lang lexer(&input);

  antlr4::CommonTokenStream tokens(&lexer);
  tokens.fill();

  for (auto&& token : tokens.getTokens()) {
    print_token(token, tokens, outFile, lexer);
  }
}

#include "SYsU_lang.h" // make sure the name is same as `.g4`
#include <fstream>
#include <iostream>
#include <unordered_map>
#include <regex>

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

//! @brief 
//! 
//! @param token 
//! @param tokens 
//! @param outFile 
//! @param lexer 
//! @param lineBias 
//! @param fileLoc 
//! @param gotSpace 
//! @param withStart 
void print_token(
  const antlr4::Token* token,
  const antlr4::CommonTokenStream& tokens,
  std::ofstream& outFile,
  const antlr4::Lexer& lexer,
  const int lineBias,
  const std::string& fileLoc,
  const bool gotSpace,
  const bool withStart
) {
  
  auto& vocabulary = lexer.getVocabulary();

  auto tokenTypeName =
    std::string(vocabulary.getSymbolicName(token->getType()));

  if (tokenTypeName.empty())
    tokenTypeName = "<UNKNOWN>"; // empty string

  if (tokenTypeMapping.find(tokenTypeName) != tokenTypeMapping.end()) {
    tokenTypeName = tokenTypeMapping[tokenTypeName];
  }

  // check if [StartOfLine]
  const int lineNumber = token->getLine() - lineBias;
  const int indexNumber = token->getCharPositionInLine() + 1;
  bool startOfLine = false;
  if (indexNumber == 1 || withStart) {
    startOfLine = true;
  }

  // check if [LeadingSpace]
  bool leadingSpace = gotSpace;

  // load location info
  std::string locInfo = " Loc=<./";
  locInfo.append(fileLoc);
  locInfo.append(":");
  locInfo.append(std::to_string(lineNumber));
  locInfo.append(":");
  locInfo.append(std::to_string(indexNumber));
  locInfo.append(">");

  // token info output
  if (token->getText() != "<EOF>") {
    outFile << tokenTypeName << " '" << token->getText() << "'";
    if (startOfLine) {
      outFile << "\t [StartOfLine]";
    }
    
    if (leadingSpace) {
      outFile << " [LeadingSpace]";
    }
  } else {
    outFile << tokenTypeName << " '"
            << "'";
  }

  outFile << locInfo << std::endl;
}

//! @brief 
//! 
//! @param lexer 
//! @param tokens 
//! @param outFile 
void print_tokens(
  SYsU_lang& lexer,
  antlr4::CommonTokenStream& tokens,
  std::ofstream& outFile
) {
  int LinesOfPreprocessing = 0;
  std::string fileLoc;
  bool gotSpace = false, withStart = false;
  for (auto&& token : tokens.getTokens()) {
    if (token->getChannel() == lexer.HIDDEN) {
      if (token->getType() == lexer.LineAfterPreprocessing) {
        LinesOfPreprocessing += 1;
        // only at the first time can read the file location
        if (LinesOfPreprocessing == 1) {
          std::string input = token->getText();
          std::regex pattern("\"\\/workspaces\\/SYsU-lang2\\/test\\/cases\\/(.+?)\"");
          std::smatch match;
          if (std::regex_search(input, match, pattern)) {
            fileLoc = match[1];
          }
        }
      } else if (token->getType() == lexer.Whitespace) {
        gotSpace = true;
        if (token->getCharPositionInLine() == 0) {
          withStart = true;
        }
      } else if (token->getType() == lexer.Newline) {
        gotSpace = false;
        withStart = false;
      }
    } else {
      print_token(
        token, tokens, outFile, lexer, 
        LinesOfPreprocessing, fileLoc, 
        gotSpace, withStart
      );
      if (gotSpace) {
        gotSpace = false;
        if (withStart) {
          withStart = false;
        }
      }
    }
  }
}


//! @brief 
//! 
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

  print_tokens(lexer, tokens, outFile);
}

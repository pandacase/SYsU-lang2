#include "SYsULexer.h" // make sure the name is as same as `.g4`
#include <fstream>
#include <iostream>
#include <unordered_map>
#include <regex>

// definition mapping，ANTLR tokenTypeName -> clang format
std::unordered_map<std::string, std::string> tokenTypeMapping = {
  { "Int", "int" },
  { "Void" , "void" },
  { "Float" , "float" },
  { "Double" , "double" },
  { "Char" , "char" },
  
  { "Const", "const" },
  { "Return", "return" },
  { "If", "if" },
  { "Else", "else" },
  { "While", "while" },
  { "Break" , "break" },
  { "Continue", "continue" },
  { "Switch" , "switch" },
  
  { "LeftParen", "l_paren" },
  { "RightParen", "r_paren" },
  { "RightBrace", "r_brace" },
  { "LeftBrace", "l_brace" },
  { "LeftBracket", "l_square" },
  { "RightBracket", "r_square" },
  
  { "Semi", "semi" },
  { "Comma", "comma" },

  { "Equal", "equal" },

  { "Plus", "plus" },
  { "Plusequal" , "plusequal" },
  { "Minus", "minus"},
  { "Minusequal" , "minusequal" },
  { "Star", "star"},
  { "Starequal" , "starequal" },
  { "Slash", "slash"},
  { "Slashequal" , "slashequal" },
  { "Percent", "percent"},
  { "Percentequal" , "percentequal" },
  
  { "Greater", "greater" },
  { "Greaterequal", "greaterequal" },
  { "Less", "less" },
  { "Lessequal", "lessequal" }, 
  
  { "Exclaim", "exclaim", },
  { "Equalequal", "equalequal" },
  { "Exclaimequal" , "exclaimequal" },
  
  { "Pipe" , "pipe" },
  { "Pipepipe", "pipepipe" },
  { "Amp" , "amp" },
  { "Ampamp", "ampamp" },
  
  { "Identifier", "identifier" },
  { "Constant", "numeric_constant" },
  
  { "EOF", "eof" },
};


//! @brief print a single token's results of lexical analysis
//! to output file, having the Info below:
//! - type : the type of this token.
//! - text : the token's raw content.
//! - [StartOfLine]?  : if is the first non-whitespace token
//!                     of this line.
//! - [LeadingSpace]? : if there is some whitespace before
//!                     this token.
//! - location Info   : <(file location):(line number):(index)>
//! 
//! @param token a token received: process target.
//! @param tokens all tokens.
//! @param outFile out stream to output file.
//! @param lexer SYsU_lang lexer.
//! @param currLine current line number.
//! @param fileLoc the source code file path.
//! @param gotSpace if there are some space before this token,
//! this param is true, else false.
//! @param withStart if the space before this token begins at
//! `Start Of Line`, this param is true, else false.
void print_token(
  const antlr4::Token* token,
  const antlr4::CommonTokenStream& tokens,
  std::ofstream& outFile,
  const antlr4::Lexer& lexer,
  const int currLine,
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
  const int lineNumber = currLine;
  const int indexNumber = token->getCharPositionInLine() + 1;
  bool startOfLine = false;
  if (indexNumber == 1 || withStart) {
    startOfLine = true;
  }

  // check if [LeadingSpace]
  bool leadingSpace = gotSpace;

  // load location info
  std::string locInfo = " Loc=<";
  locInfo.append(fileLoc);
  locInfo.append(":");
  locInfo.append(std::to_string(lineNumber));
  locInfo.append(":");
  locInfo.append(std::to_string(indexNumber));
  locInfo.append(">");

  // token info output to file
  if (token->getText() != "<EOF>") {
    outFile << tokenTypeName << " '" << token->getText() << "'";
    if (startOfLine) {
      outFile << "\t [StartOfLine]";
    }
    
    if (leadingSpace) {
      outFile << " [LeadingSpace]";
    }
  } else {
    outFile << tokenTypeName << " '" << "'";
  }

  outFile << locInfo << std::endl;
}


//! @brief print all tokens one by one: call print_token to 
//! print a single token for N times.
//! 
//! @param lexer SYsU_lang lexer.
//! @param tokens all tokens.
//! @param outFile out stream to output file.
void print_tokens(
  SYsU_lang& lexer,
  antlr4::CommonTokenStream& tokens,
  std::ofstream& outFile
) {
  std::string fileLoc;
  int currLine = 0;
  bool gotSpace = false, withStart = false;
  for (auto&& token : tokens.getTokens()) {
    if (token->getChannel() == lexer.HIDDEN) {
      if (token->getType() == lexer.LineAfterPreprocessing) {
        std::string input = token->getText();

        // match the first line
        std::regex patternOfNum("# ([0-9]+) \"");
        std::smatch matchOfNum;
        if (std::regex_search(input, matchOfNum, patternOfNum)) {
          currLine = std::stoi(matchOfNum[1]) - 1;
        }

        // match the file location
        std::regex patternOfLoc("\"(.+?)\"");
        std::smatch matchOfLoc;
        if (std::regex_search(input, matchOfLoc, patternOfLoc)) {
          fileLoc = matchOfLoc[1];
        }
      } else if (token->getType() == lexer.Whitespace) {
        // tag the whitespace.
        gotSpace = true;
        if (token->getCharPositionInLine() == 0) {
          withStart = true;
        }
      } else if (token->getType() == lexer.Newline) {
        // when get a newline, reset the start flag.
        gotSpace = false;
        withStart = false;
        // then current line ++
        currLine += 1;
      }
    } else {
      print_token(
        token, tokens, outFile, lexer, 
        currLine, fileLoc,
        gotSpace, withStart
      );
      // a token consume a space / start flag.
      if (gotSpace) {
        gotSpace = false;
        if (withStart) {
          withStart = false;
        }
      }
    }
  }
}


//! @brief The client: get the source code after preprocessing
//! from input file, and put the lexer's result to output file.
//! 
int main(int argc, char* argv[]) {
  if (argc != 3) {
    std::cout << "Usage: " << argv[0] << " <input> <output>\n";
    return -1;
  }

  std::ifstream inFile(argv[1]);
  if (!inFile) {
    std::cout << "Error: unable to open input file: " 
      << argv[1] << '\n';
    return -2;
  }

  std::ofstream outFile(argv[2]);
  if (!outFile) {
    std::cout << "Error: unable to open output file: " 
      << argv[2] << '\n';
    return -3;
  }

  std::cout << "Program '" << argv[0] << std::endl;
  std::cout << "Input   '" << argv[1] << std::endl;
  std::cout << "Output  '" << argv[2] << std::endl;

  antlr4::ANTLRInputStream input(inFile);
  SYsULexer lexer(&input);

  antlr4::CommonTokenStream tokens(&lexer);
  tokens.fill();

  print_tokens(lexer, tokens, outFile);
}

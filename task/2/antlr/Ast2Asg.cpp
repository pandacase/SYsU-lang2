#include "Ast2Asg.hpp"
#include <unordered_map>

#define self (*this)

namespace asg {

//! @brief Symbol table, which holds all declarations for the current scope.
//!
struct Ast2Asg::Symtbl : public std::unordered_map<std::string, Decl*> {
  Ast2Asg& m;
  Symtbl* mPrev;

  Symtbl(Ast2Asg& m)
  : m(m)
  , mPrev(m.mSymtbl) {
    m.mSymtbl = this;
  }

  ~Symtbl() { m.mSymtbl = mPrev; }

  Decl* resolve(const std::string& name);
};

//! @brief find out a Decl whose name is `name` in the symbol table.
//! 
Decl* Ast2Asg::Symtbl::resolve(const std::string& name) {
  auto iter = find(name);
  if (iter != end())
    return iter->second;
  ASSERT(mPrev != nullptr); // Undefined ID
  return mPrev->resolve(name);
}

//! @brief calculating a expression's literal value.
//! 
//! @param expr the expression.
//! @return int the literal result of the expression.
static int eval_arrlen(Expr* expr) {
  if (auto p = expr->dcst<IntegerLiteral>())
    return p->val;

  if (auto p = expr->dcst<DeclRefExpr>()) {
    if (p->decl == nullptr)
      ABORT();

    auto var = p->decl->dcst<VarDecl>();
    if (!var || !var->type->qual.const_)
      ABORT(); // Array length must be a compile time constant

    switch (var->type->spec) {
      case Type::Spec::kChar:
      case Type::Spec::kInt:
      case Type::Spec::kLong:
      case Type::Spec::kLongLong:
        return eval_arrlen(var->init);

      default:
        ABORT(); // Length expression must be of numerical type
    }
  }

  if (auto p = expr->dcst<UnaryExpr>()) {
    auto sub = eval_arrlen(p->sub);

    switch (p->op) {
      case UnaryExpr::kPos:
        return sub;

      case UnaryExpr::kNeg:
        return -sub;

      default:
        ABORT();
    }
  }

  if (auto p = expr->dcst<BinaryExpr>()) {
    auto lft = eval_arrlen(p->lft);
    auto rht = eval_arrlen(p->rht);

    switch (p->op) {
      case BinaryExpr::kAdd:
        return lft + rht;

      case BinaryExpr::kSub:
        return lft - rht;

      default:
        ABORT();
    }
  }

  if (auto p = expr->dcst<InitListExpr>()) {
    if (p->list.empty())
      return 0;
    return eval_arrlen(p->list[0]);
  }

  ABORT();
}

//////////////////////////////////////////////////////////////////////////////
//! Top
//////////////////////////////////////////////////////////////////////////////

TranslationUnit* Ast2Asg::operator()(ast::TranslationUnitContext* ctx) {
  auto ret = make<asg::TranslationUnit>();
  if (ctx == nullptr)
    return ret;

  Symtbl localDecls(self);

  for (auto&& i : ctx->externalDeclaration()) {
    if (auto p = i->declaration()) {
      auto decls = self(p);
      ret->decls.insert(ret->decls.end(),
      std::make_move_iterator(decls.begin()),
      std::make_move_iterator(decls.end()));
    }

    else if (auto p = i->functionDefinition()) {
      auto funcDecl = self(p);
      ret->decls.push_back(funcDecl);

      // Add to declaration table
      localDecls[funcDecl->name] = funcDecl;
    }

    else
      ABORT();
  }

  return ret;
}

//////////////////////////////////////////////////////////////////////////////
//! Type
//////////////////////////////////////////////////////////////////////////////

Ast2Asg::SpecQual
Ast2Asg::operator()(ast::DeclarationSpecifiersContext* ctx) {
  SpecQual ret = { Type::Spec::kINVALID, Type::Qual() };

  for (auto&& i : ctx->declarationSpecifier()) {
    if (auto p = i->typeSpecifier()) {
      if (ret.first == Type::Spec::kINVALID) {
        if (p->Void())
          ret.first = Type::Spec::kVoid;
        else if (p->Int())
          ret.first = Type::Spec::kInt;
        else if (p->Char())
          ret.first = Type::Spec::kChar;
        else if (p->Long())
          ret.first = Type::Spec::kLong;
        else
          ABORT(); // Unknown type descriptor
      } else {
        ABORT(); // Unknown type descriptor
      }
    }

    else if (auto p = i->typeQualifier()) {
      if (ret.second == Type::Qual{}) {
        if (p->Const())
          ret.second.const_ = true;
        else
          ABORT(); // Unknown type descriptor
      } else {
        ABORT(); // Unknown type descriptor
      }
    }

    else
      ABORT();
    }
  return ret;
}

std::pair<TypeExpr*, std::string>
Ast2Asg::operator()(ast::DeclaratorContext* ctx, TypeExpr* sub) {
  return self(ctx->directDeclarator(), sub);
}



std::pair<TypeExpr*, std::string>
Ast2Asg::operator()(ast::DirectDeclaratorContext* ctx, TypeExpr* sub) {
  if (auto p = ctx->Identifier())
    return { sub, p->getText() };

  if (ctx->LeftBracket()) { // array decl
    auto arrayType = make<ArrayType>();
    arrayType->sub = sub;

    if (auto p = ctx->assignmentExpression())
      arrayType->len = eval_arrlen(self(p));
    else
      arrayType->len = ArrayType::kUnLen;

    return self(ctx->directDeclarator(), arrayType);
  }

  if (ctx->LeftParen()) {
    auto funcType = make<FunctionType>();
    funcType->sub = sub;
    if (auto paramList = ctx->parameterList()) {
      for (auto param : paramList->parameterDeclaration()) {
        auto specQual = self(param->declarationSpecifiers());
        auto paramType = make<Type>();
        paramType->spec = specQual.first;
        paramType->qual = specQual.second;
        paramType->texp = self(param->directDeclarator(), nullptr).first;
        funcType->params.push_back(paramType);
      }
    }
    return self(ctx->directDeclarator(), funcType);
  }

  ABORT();
}

//////////////////////////////////////////////////////////////////////////////
//! Expression
//////////////////////////////////////////////////////////////////////////////

Expr* Ast2Asg::operator()(ast::ExpressionContext* ctx) {
  auto assignExprList = ctx->assignmentExpression();
  Expr* ret = self(assignExprList[0]);

  for (unsigned i = 1; i < assignExprList.size(); ++i) {
    auto node = make<BinaryExpr>();
    node->op = node->kComma;
    node->lft = ret;
    node->rht = self(assignExprList[i]);
    ret = node;
  }

  return ret;
}

Expr* Ast2Asg::operator()(ast::AssignmentExpressionContext* ctx) {
  if (auto p = ctx->logicalOrExpression())
    return self(p);

  auto ret = make<BinaryExpr>();
  ret->op = ret->kAssign;
  ret->lft = self(ctx->unaryExpression());
  ret->rht = self(ctx->assignmentExpression());
  return ret;
}

Expr* Ast2Asg::operator()(ast::LogicalOrExpressionContext* ctx) {
  auto children = ctx->children;
  Expr* ret = self(dynamic_cast<ast::LogicalAndExpressionContext*>(children[0]));

  for (unsigned i = 1; i < children.size(); ++i) {
    auto node = make<BinaryExpr>();

    auto op = dynamic_cast<antlr4::tree::TerminalNode*>(children[i])
                    ->getSymbol()
                    ->getType();
    switch (op) {
      case ast::Pipepipe:
        node->op = node->kOr;
        break;

      default:
        ABORT();
    }

    node->lft = ret;
    node->rht = self(dynamic_cast<ast::LogicalAndExpressionContext*>(children[++i]));
    ret = node;
  }

  return ret;
}

Expr* Ast2Asg::operator()(ast::LogicalAndExpressionContext* ctx) {
  auto children = ctx->children;
  Expr* ret = self(dynamic_cast<ast::EqualityExpressionContext*>(children[0]));

  for (unsigned i = 1; i < children.size(); ++i) {
    auto node = make<BinaryExpr>();

    auto op = dynamic_cast<antlr4::tree::TerminalNode*>(children[i])
                    ->getSymbol()
                    ->getType();
    switch (op) {
      case ast::Ampamp:
        node->op = node->kAnd;
        break;

      default:
        ABORT();
    }

    node->lft = ret;
    node->rht = self(dynamic_cast<ast::EqualityExpressionContext*>(children[++i]));
    ret = node;
  }

  return ret;
}

Expr* Ast2Asg::operator()(ast::EqualityExpressionContext* ctx) {
  auto children = ctx->children;
  Expr* ret = self(dynamic_cast<ast::RelationalExpressionContext*>(children[0]));

  for (unsigned i = 1; i < children.size(); ++i) {
    auto node = make<BinaryExpr>();

    auto op = dynamic_cast<antlr4::tree::TerminalNode*>(children[i])
                    ->getSymbol()
                    ->getType();
    switch (op) {
      case ast::Equalequal:
        node->op = node->kEq;
        break;

      case ast::Exclaimequal:
        node->op = node->kNe;
        break;

      case ast::Percent:
        node->op = node->kMod;
        break;

      default:
        ABORT();
    }

    node->lft = ret;
    node->rht = self(dynamic_cast<ast::RelationalExpressionContext*>(children[++i]));
    ret = node;
  }

  return ret;
}

Expr* Ast2Asg::operator()(ast::RelationalExpressionContext* ctx) {
  auto children = ctx->children;
  Expr* ret = 
    self(dynamic_cast<ast::AdditiveExpressionContext*>(children[0]));

  for (unsigned i = 1; i < children.size(); ++i) {
    auto node = make<BinaryExpr>();

    auto op = dynamic_cast<antlr4::tree::TerminalNode*>(children[i])
                  ->getSymbol()
                  ->getType();
    switch (op) {
      case ast::Less:
        node->op = node->kLt;
        break;
      
      case ast::Greater:
        node->op = node->kGt;
        break;

      case ast::Lessequal:
        node->op = node->kLe;
        break;
      
      case ast::Greaterequal:
        node->op = node->kGe;
        break;

      default:
        ABORT();
    }

    node->lft = ret;
    node->rht =
      self(dynamic_cast<ast::AdditiveExpressionContext*>(children[++i]));
    ret = node;
  }

  return ret;
}

Expr* Ast2Asg::operator()(ast::AdditiveExpressionContext* ctx) {
  auto children = ctx->children;
  Expr* ret = 
    self(dynamic_cast<ast::MultiplicativeExpressionContext*>(children[0]));

  for (unsigned i = 1; i < children.size(); ++i) {
    auto node = make<BinaryExpr>();

    auto op = dynamic_cast<antlr4::tree::TerminalNode*>(children[i])
                  ->getSymbol()
                  ->getType();
    switch (op) {
      case ast::Plus:
        node->op = node->kAdd;
        break;

      case ast::Minus:
        node->op = node->kSub;
        break;

      default:
        ABORT();
    }

    node->lft = ret;
    node->rht =
      self(dynamic_cast<ast::MultiplicativeExpressionContext*>(children[++i]));
    ret = node;
  }

  return ret;
}

Expr* Ast2Asg::operator()(ast::MultiplicativeExpressionContext* ctx) {
  auto children = ctx->children;
  Expr* ret = self(dynamic_cast<ast::UnaryExpressionContext*>(children[0]));

  for (unsigned i = 1; i < children.size(); ++i) {
    auto node = make<BinaryExpr>();

    auto op = dynamic_cast<antlr4::tree::TerminalNode*>(children[i])
                    ->getSymbol()
                    ->getType();
    switch (op) {
      case ast::Star:
        node->op = node->kMul;
        break;

      case ast::Slash:
        node->op = node->kDiv;
        break;

      case ast::Percent:
        node->op = node->kMod;
        break;

      default:
        ABORT();
    }

    node->lft = ret;
    node->rht = self(dynamic_cast<ast::UnaryExpressionContext*>(children[++i]));
    ret = node;
  }

  return ret;
}

Expr* Ast2Asg::operator()(ast::UnaryExpressionContext* ctx) {
  if (auto p = ctx->postfixExpression())
    return self(p);

  auto ret = make<UnaryExpr>();

  auto op = dynamic_cast<antlr4::tree::TerminalNode*>(
              ctx->unaryOperator()->children[0]
            )->getSymbol()->getType();
  switch (op) {
    case ast::Plus:
      ret->op = ret->kPos;
      break;

    case ast::Minus:
      ret->op = ret->kNeg;
      break;
    
    case ast::Exclaim:
      ret->op = ret->kNot;
      break;

    default:
      ABORT();
  }

  ret->sub = self(ctx->unaryExpression());

  return ret;
}

Expr* Ast2Asg::operator() (ast::PostfixExpressionContext* ctx) {
  if (auto p = ctx->primaryExpression()) {
    return self(p);
  }

  if (ctx->LeftBracket()) {
    auto ret = make<BinaryExpr>();
    ret->op = ret->kIndex;
    ret->lft = self(ctx->postfixExpression());
    ret->rht = self(ctx->expression());
    return ret;
  }

  if (ctx->LeftParen()) {
    auto ret = make<CallExpr>();
    ret->head = self(ctx->postfixExpression());
    if (auto p = ctx->expression()) {
      for (auto&& arg : p->assignmentExpression())
        ret->args.push_back(self(arg));
    }
    return ret;
  }

  ABORT();
}


Expr* Ast2Asg::operator()(ast::PrimaryExpressionContext* ctx) {
  if (auto p = ctx->Identifier()) {
    auto name = p->getText();
    auto ret = make<DeclRefExpr>();
    ret->decl = mSymtbl->resolve(name);
    return ret;
  }

  if (auto p = ctx->Constant()) {
    auto text = p->getText();

    auto ret = make<IntegerLiteral>();

    ASSERT(!text.empty());
    if (text[0] != '0')
      ret->val = std::stoll(text);

    else if (text.size() == 1)
      ret->val = 0;

    else if (text[1] == 'x' || text[1] == 'X')
      ret->val = std::stoll(text.substr(2), nullptr, 16);

    else
      ret->val = std::stoll(text.substr(1), nullptr, 8);

    return ret;
  }

  if (auto p = ctx->expression()) {
    auto ret = make<ParenExpr>();
    ret->sub = self(p);
    return ret;
  }

  ABORT();
}

//! @details The `initializer` and `initializerList` is indirect
//! mutual recursive
//!
Expr* Ast2Asg::operator()(ast::InitializerContext* ctx) {
  if (auto p = ctx->assignmentExpression())
    return self(p);

  auto ret = make<InitListExpr>();

  if (auto p = ctx->initializerList()) {
    for (auto&& i : p->initializer()) {
      // Flatten the initialization list
      auto expr = self(i);
      if (auto p = expr->dcst<InitListExpr>()) {
        for (auto&& sub : p->list)
          ret->list.push_back(sub);
      } else {
        ret->list.push_back(expr);
      }
    }
  }

  return ret;
}

//////////////////////////////////////////////////////////////////////////////
//! Statement
//////////////////////////////////////////////////////////////////////////////

Stmt* Ast2Asg::operator()(ast::StatementContext* ctx) {
  if (auto p = ctx->compoundStatement())
    return self(p);

  if (auto p = ctx->expressionStatement())
    return self(p);

  if (auto p = ctx->ifStatement())
    return self(p);

  if (auto p = ctx->iterationStatement())
    return self(p);

  if (auto p = ctx->jumpStatement())
    return self(p);
  
  ABORT();
}

CompoundStmt* Ast2Asg::operator()(ast::CompoundStatementContext* ctx) {
  auto ret = make<CompoundStmt>();

  if (auto p = ctx->blockItemList()) {
    Symtbl localDecls(self);

    for (auto&& i : p->blockItem()) {
      if (auto q = i->declaration()) {
        auto sub = make<DeclStmt>();
        sub->decls = self(q);
        ret->subs.push_back(sub);
      }

      else if (auto q = i->statement())
        ret->subs.push_back(self(q));

      else
        ABORT();
    }
  }

  return ret;
}

Stmt* Ast2Asg::operator()(ast::ExpressionStatementContext* ctx) {
  if (auto p = ctx->expression()) {
    auto ret = make<ExprStmt>();
    ret->expr = self(p);
    return ret;
  }

  return make<NullStmt>();
}

Stmt* Ast2Asg::operator()(ast::IfStatementContext* ctx) {
  auto ret = make<IfStmt>();
  ret->cond = self(ctx->expression());
  auto stmtList = ctx->statement();
  ret->then = self(stmtList[0]);
  if (ctx->Else()) {
    ret->else_ = self(stmtList[1]);
  }
  return ret;
}

Stmt* Ast2Asg::operator()(ast::IterationStatementContext* ctx) {
  auto ret = make<WhileStmt>();
  ret->cond = self(ctx->expression());
  ret->body = self(ctx->statement());
  return ret;
}

Stmt* Ast2Asg::operator()(ast::JumpStatementContext* ctx) {
  if (ctx->Break()) {
    auto ret = make<BreakStmt>();
    return ret;
  }

  if (ctx->Continue()) {
    auto ret = make<ContinueStmt>();
    return ret;
  }

  if (ctx->Return()) {
    auto ret = make<ReturnStmt>();
    ret->func = mCurrentFunc;
    if (auto p = ctx->expression())
      ret->expr = self(p);
    return ret;
  }

  ABORT();
}

//////////////////////////////////////////////////////////////////////////////
//! Declaration
//////////////////////////////////////////////////////////////////////////////

std::vector<Decl*> Ast2Asg::operator()(ast::DeclarationContext* ctx) {
  std::vector<Decl*> ret;

  auto specs = self(ctx->declarationSpecifiers());

  if (auto initDeclList = ctx->initDeclaratorList()) {
    for (auto&& p : initDeclList->initDeclarator())
      ret.push_back(self(p, specs));
  }

  // If `initDeclaratorList` is empty, this line of declaration 
  // statement is meaningless
  return ret;
}

FunctionDecl* Ast2Asg::operator()(ast::FunctionDefinitionContext* ctx) {
  auto specQual = self(ctx->declarationSpecifiers());
  
  auto ret = dynamic_cast<FunctionDecl*>(self(ctx->initDeclarator(), specQual));
  // funcDecl has been fully parsed in `self(InitDeclaratorContext, specQual)`
  mCurrentFunc = ret;
  // Symtbl localDecls(self);
  for (auto&& paramDecl: ret->params) {
    (*mSymtbl)[paramDecl->name] = paramDecl;
  }

  ret->body = self(ctx->compoundStatement());
  return ret;
}

Decl* Ast2Asg::operator()(ast::InitDeclaratorContext* ctx, SpecQual sq) {
  Decl* ret;
  
  // declarator --> directDeclarator
  auto [texp, name] = self(ctx->declarator(), nullptr);

  // Type.texp -> FunctionType / ArrayType
  auto type = make<Type>();
  type->spec = sq.first;
  type->qual = sq.second;

  // funcType has been fully parsed in `self(DirectDeclaratorContext, nullptr)`
  if (auto funcType = texp->dcst<FunctionType>()) {
    auto funcDecl = make<FunctionDecl>();
    funcDecl->type = type;
    funcDecl->name = std::move(name);

    type->texp = funcType;

    if (auto paramList = ctx->declarator()->directDeclarator()->parameterList()) {
      for (auto&& param : paramList->parameterDeclaration()) {
        auto paramSpecQual = self(param->declarationSpecifiers());
        auto [paramTexp, paramName] = self(param->directDeclarator(), nullptr);
        
        auto paramType = make<Type>();
        paramType->spec = paramSpecQual.first;
        paramType->qual = paramSpecQual.second;
        paramType->texp = paramTexp;
        
        auto paramDecl = make<VarDecl>();
        paramDecl->type = paramType;
        paramDecl->name = std::move(paramName);

        funcDecl->params.push_back(paramDecl);
      }
    }

    if (ctx->initializer())
      ABORT();
    funcDecl->body = nullptr;

    ret = funcDecl;
  }

  else { // arrayType or NULL
    auto vdecl = make<VarDecl>();
    vdecl->type = type;

    type->texp = texp;
    vdecl->name = std::move(name);

    if (auto p = ctx->initializer())
      vdecl->init = self(p);
    else
      vdecl->init = nullptr;

    ret = vdecl;
  }

  // This implementation allows for repeated definitions of symbols,
  // and the new definition will replace the old one
  (*mSymtbl)[ret->name] = ret;
  return ret;
}

} // namespace asg

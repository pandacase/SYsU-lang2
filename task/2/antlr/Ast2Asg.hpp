#pragma once

#include "SYsUParser.h"
#include "asg.hpp"

namespace asg {

using ast = SYsUParser;

//! @brief Convert ast to asg
//! 
//! @details
//! - operator()
//!   is overloaded multiple times, and each overload corresponds
//!   to the conversion logic of different node types in the AST.
//!
class Ast2Asg {
public:
  Obj::Mgr& mMgr;

  Ast2Asg(Obj::Mgr& mgr) : mMgr(mgr) { }

  //////////////////////////////////////////////////////////////////////////////
  //! Top
  //////////////////////////////////////////////////////////////////////////////

  TranslationUnit* operator()(ast::TranslationUnitContext* ctx);

  //////////////////////////////////////////////////////////////////////////////
  //! Type
  //////////////////////////////////////////////////////////////////////////////

  using SpecQual = std::pair<Type::Spec, Type::Qual>;

  SpecQual operator()(ast::DeclarationSpecQualsContext* ctx);

  //! @brief This method handles declarators, which may contain 
  //! more complex type information such as arrays and functions.
  //! 
  //! @param ctx a declaration context
  //! @param sub possible subtype expressions (such as the element
  //! type of an array)
  //! @return std::pair<TypeExpr*, std::string> :
  //! containing the type expression and variable name.
  std::pair<TypeExpr*, std::string> operator()(
    ast::DeclaratorContext* ctx,
    TypeExpr* sub
  );

  std::pair<TypeExpr*, std::string> operator()(
    ast::DirectDeclaratorContext* ctx,
    TypeExpr* sub
  );

  //////////////////////////////////////////////////////////////////////////////
  //! Expression
  //////////////////////////////////////////////////////////////////////////////

  Expr* operator()(ast::ExpressionContext* ctx);

  Expr* operator()(ast::AssignmentExpressionContext* ctx);

  Expr* operator()(ast::AdditiveExpressionContext* ctx);

  Expr* operator()(ast::MultiplicativeExpressionContext* ctx);

  Expr* operator()(ast::UnaryExpressionContext* ctx);

  Expr* operator()(ast::PostfixExpressionContext* ctx);

  Expr* operator()(ast::PrimaryExpressionContext* ctx);

  Expr* operator()(ast::InitializerContext* ctx);

  //////////////////////////////////////////////////////////////////////////////
  //! Statement
  //////////////////////////////////////////////////////////////////////////////

  Stmt* operator()(ast::StatementContext* ctx);

  CompoundStmt* operator()(ast::CompoundStatementContext* ctx);

  Stmt* operator()(ast::ExpressionStatementContext* ctx);

  Stmt* operator()(ast::JumpStatementContext* ctx);

  //////////////////////////////////////////////////////////////////////////////
  //! Declaration
  //////////////////////////////////////////////////////////////////////////////

  std::vector<Decl*> operator()(ast::DeclarationContext* ctx);

  FunctionDecl* operator()(ast::FunctionDefinitionContext* ctx);

  Decl* operator()(ast::InitDeclaratorContext* ctx, SpecQual sq);

private:
  //! @brief a symbol table used to manage symbol information
  //! within the scope during the conversion process.
  //! 
  struct Symtbl;
  Symtbl* mSymtbl{ nullptr };

  FunctionDecl* mCurrentFunc{ nullptr };

  //! @brief Used to create a new AST node object through the 
  //! object manager.
  //! 
  template<typename T, typename... Args>
  T* make(Args... args) {
    return mMgr.make<T>(args...);
  }
};

} // namespace asg

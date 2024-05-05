#include "asg.hpp"
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

class EmitIR
{
public:
  Obj::Mgr& mMgr;
  llvm::Module mMod;

  EmitIR(Obj::Mgr& mgr, llvm::LLVMContext& ctx, llvm::StringRef mid = "-");

  llvm::Module& operator()(asg::TranslationUnit* tu);

private:
  llvm::LLVMContext& mCtx;

  llvm::Type* mIntTy;
  llvm::FunctionType* mCtorTy;

  llvm::Function* mCurFunc{ nullptr };
  std::unique_ptr<llvm::IRBuilder<>> mCurIrb;

  void trans_init(llvm::Value* val, asg::Expr* obj);

  //////////////////////////////////////////////////////////////////////////////
  // Type
  //////////////////////////////////////////////////////////////////////////////

  llvm::Type* operator()(const asg::Type* type);

  //////////////////////////////////////////////////////////////////////////////
  // Expression
  //////////////////////////////////////////////////////////////////////////////

  llvm::Value* operator()(asg::Expr* obj);

  llvm::Constant* operator()(asg::IntegerLiteral* obj);

  llvm::Constant* operator()(asg::StringLiteral* obj);

  llvm::Value* operator()(asg::DeclRefExpr* obj);

  llvm::Value* operator()(asg::ParenExpr* obj);

  llvm::Value* operator()(asg::UnaryExpr* obj);

  llvm::Value* operator()(asg::BinaryExpr* obj);

  llvm::Value* operator()(asg::CallExpr* obj);

  llvm::Value* operator()(asg::InitListExpr* obj);

  llvm::Value* operator()(asg::ImplicitInitExpr* obj);
  
  llvm::Value* operator()(asg::ImplicitCastExpr* obj);


  //////////////////////////////////////////////////////////////////////////////
  // Statement
  //////////////////////////////////////////////////////////////////////////////

  void operator()(asg::Stmt* obj);

  void operator()(asg::DeclStmt* obj);

  void operator()(asg::ExprStmt* obj);

  void operator()(asg::CompoundStmt* obj);

  void operator()(asg::IfStmt* obj);

  void operator()(asg::WhileStmt* obj);

  void operator()(asg::DoStmt* obj);

  void operator()(asg::BreakStmt* obj);

  void operator()(asg::ContinueStmt* obj);

  void operator()(asg::ReturnStmt* obj);

  // TODO: 添加语句处理相关声明

  //////////////////////////////////////////////////////////////////////////////
  // Declaration
  //////////////////////////////////////////////////////////////////////////////

  void operator()(asg::Decl* obj);

  void operator()(asg::FunctionDecl* obj);

  // TODO: 添加声明处理相关声明

  void operator()(asg::VarDecl* obj);

};

// 声明( Decl ) -> 类型( Type ) -> 表达式( Expr ) -> 语句( Stmt )
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

  void trans_init(asg::Expr* obj, llvm::Value* val);

  void checkToBool(llvm::Value* &val);

  void checkToZExt32(llvm::Value* &val);

  void checkToSExt64(llvm::Value* &val);

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

  llvm::Value* operator()(asg::InitListExpr* obj, llvm::Value* var, llvm::Type* ty);

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

  //////////////////////////////////////////////////////////////////////////////
  // Declaration
  //////////////////////////////////////////////////////////////////////////////

  void operator()(asg::Decl* obj);

  void operator()(asg::FunctionDecl* obj);

  void operator()(asg::VarDecl* obj);

};

#include "EmitIR.hpp"
#include <llvm/Transforms/Utils/ModuleUtils.h>

#define self (*this)

using namespace asg;

EmitIR::EmitIR(Obj::Mgr& mgr, llvm::LLVMContext& ctx, llvm::StringRef mid)
  : mMgr(mgr)
  , mMod(mid, ctx)
  , mCtx(ctx)
  , mIntTy(llvm::Type::getInt32Ty(ctx))
  , mCurIrb(std::make_unique<llvm::IRBuilder<>>(ctx))
  , mCtorTy(llvm::FunctionType::get(llvm::Type::getVoidTy(ctx), false))
{
}

llvm::Module&
EmitIR::operator()(asg::TranslationUnit* tu)
{
  for (auto&& i : tu->decls)
    self(i);
  return mMod;
}

////////////////////////////////////////////////////////////////////////////////
// Type
////////////////////////////////////////////////////////////////////////////////

llvm::Type*
EmitIR::operator()(const Type* type)
{
  if (type->qual.const_) {

  }

  if (type->texp == nullptr) {
    switch (type->spec) {
      case Type::Spec::kVoid:
        return llvm::Type::getVoidTy(mCtx);
      case Type::Spec::kChar:
        return llvm::Type::getInt8Ty(mCtx);
      case Type::Spec::kInt:
        return llvm::Type::getInt32Ty(mCtx);
      case Type::Spec::kLong:
        return llvm::Type::getInt32Ty(mCtx);
      case Type::Spec::kLongLong:
        return llvm::Type::getInt64Ty(mCtx);
      default:
        ABORT();
    }
  }

  // TODO: 在此添加对指针类型、数组类型和函数类型的处理
  Type subt;
  subt.spec = type->spec;
  subt.qual = type->qual;
  subt.texp = type->texp->sub;
  if (auto p = type->texp->dcst<PointerType>()) {

  } else if (auto p = type->texp->dcst<ArrayType>()) {
    return llvm::ArrayType::get(self(&subt), p->len);
  } else if (auto p = type->texp->dcst<FunctionType>()) {
    std::vector<llvm::Type*> pty;
    for (auto&& param : p->params) {
      pty.push_back(self(param));
    }
    return llvm::FunctionType::get(self(&subt), std::move(pty), false);
  }

  ABORT();
}

////////////////////////////////////////////////////////////////////////////////
// Expression
////////////////////////////////////////////////////////////////////////////////

llvm::Value*
EmitIR::operator()(Expr* obj)
{
  // TODO: 在此添加对更多表达式处理的跳转
  if (auto p = obj->dcst<IntegerLiteral>())
    return self(p);

  // if (auto p = obj->dcst<StringLiteral>())
  //   return self(p);

  if (auto p = obj->dcst<DeclRefExpr>())
    return self(p);

  if (auto p = obj->dcst<ParenExpr>())
    return self(p);

  if (auto p = obj->dcst<UnaryExpr>())
    return self(p);

  if (auto p = obj->dcst<BinaryExpr>())
    return self(p);

  if (auto p = obj->dcst<CallExpr>())
    return self(p);
  
  if (auto p = obj->dcst<InitListExpr>())
    return self(p);
  
  if (auto p = obj->dcst<ImplicitInitExpr>())
    return self(p);

  if (auto p = obj->dcst<ImplicitCastExpr>())
    return self(p);

  ABORT();
}

llvm::Constant*
EmitIR::operator()(IntegerLiteral* obj)
{
  return llvm::ConstantInt::get(self(obj->type), obj->val);
}

llvm::Value*
EmitIR::operator()(DeclRefExpr* obj)
{
  // 在LLVM IR层面，左值体现为返回指向值的指针
  // 在ImplicitCastExpr::kLValueToRValue中发射load指令从而变成右值
  return reinterpret_cast<llvm::Value*>(obj->decl->any);
}

llvm::Value*
EmitIR::operator()(ParenExpr* obj)
{
  return self(obj->sub);
}

llvm::Value*
EmitIR::operator()(UnaryExpr* obj)
{
  llvm::Value *Val;

  Val = self(obj->sub);

  auto& irb = *mCurIrb;
  switch (obj->op) {
    case UnaryExpr::kPos:
      return Val;
    case UnaryExpr::kNeg:
      return irb.CreateNeg(Val);
    case UnaryExpr::kNot:
      return irb.CreateNot(Val);
    default:
      ABORT();
  }
}

llvm::Value*
EmitIR::operator()(BinaryExpr* obj)
{
  llvm::Value *lftVal, *rhtVal;

  lftVal = self(obj->lft);
  rhtVal = self(obj->rht);

  auto& irb = *mCurIrb;
  switch (obj->op) {
    case BinaryExpr::kMul:
      return irb.CreateMul(lftVal, rhtVal);
    case BinaryExpr::kDiv:
      return irb.CreateSDiv(lftVal, rhtVal);
    case BinaryExpr::kMod:
      return irb.CreateSRem(lftVal, rhtVal);
    case BinaryExpr::kAdd:
      return irb.CreateAdd(lftVal, rhtVal);
    case BinaryExpr::kSub:
      return irb.CreateSub(lftVal, rhtVal);
    case BinaryExpr::kGt:
      return irb.CreateICmpSGT(lftVal, rhtVal);
    case BinaryExpr::kLt:
      return irb.CreateICmpSLT(lftVal, rhtVal);
    case BinaryExpr::kGe:
      return irb.CreateICmpSGE(lftVal, rhtVal);
    case BinaryExpr::kLe:
      return irb.CreateICmpSLE(lftVal, rhtVal);
    case BinaryExpr::kEq:
      return irb.CreateICmpEQ(lftVal, rhtVal);
    case BinaryExpr::kNe:
      return irb.CreateICmpNE(lftVal, rhtVal);
    // case BinaryExpr::kAnd:
    //   return irb.Create
    // case BinaryExpr::kOr:
    //   return irb.Create
    case BinaryExpr::kAssign:
      irb.CreateStore(rhtVal, lftVal);
      return rhtVal;
    // case BinaryExpr::kComma:
    //   return irb.Create
    case BinaryExpr::kIndex:
      // return ;
      // irb.
    default:
      ABORT();
  }
}

llvm::Value*
EmitIR::operator()(CallExpr* obj)
{
  
}

llvm::Value*
EmitIR::operator()(InitListExpr* obj)
{
  
}

llvm::Value*
EmitIR::operator()(ImplicitInitExpr* obj)
{
  
}

llvm::Value*
EmitIR::operator()(ImplicitCastExpr* obj)
{
  auto sub = self(obj->sub);

  // auto& irb = *mCurIrb;
  switch (obj->kind) {
    case ImplicitCastExpr::kLValueToRValue: {
      auto ty = self(obj->sub->type);
      auto loadVal = mCurIrb->CreateLoad(ty, sub);
      return loadVal;
    }

    case ImplicitCastExpr::kArrayToPointerDecay: {
      
    }

    default:
      ABORT();
  }
}



// TODO: 在此添加对更多表达式类型的处理

////////////////////////////////////////////////////////////////////////////////
// Statement
////////////////////////////////////////////////////////////////////////////////

void
EmitIR::operator()(Stmt* obj)
{
  // TODO: 在此添加对更多Stmt类型的处理的跳转
  if (auto p = obj->dcst<NullStmt>())
    return;

  if (auto p = obj->dcst<DeclStmt>())
    return self(p);

  if (auto p = obj->dcst<ExprStmt>())
    return self(p);

  if (auto p = obj->dcst<CompoundStmt>())
    return self(p);

  if (auto p = obj->dcst<IfStmt>())
    return self(p);

  if (auto p = obj->dcst<WhileStmt>())
    return self(p);

  if (auto p = obj->dcst<DoStmt>())
    return self(p);

  if (auto p = obj->dcst<BreakStmt>())
    return self(p);

  if (auto p = obj->dcst<ContinueStmt>())
    return self(p);

  if (auto p = obj->dcst<ReturnStmt>())
    return self(p);

  ABORT();
}

void
EmitIR::operator()(DeclStmt* obj) {
  for (auto&& decl : obj->decls)
    self(decl);
}

void
EmitIR::operator()(ExprStmt* obj)
{
  if (auto p = obj->expr) {
    self(p);
  }
}

void
EmitIR::operator()(CompoundStmt* obj)
{
  // TODO: 可以在此添加对符号重名的处理
  for (auto&& stmt : obj->subs)
    self(stmt);
}

void
EmitIR::operator()(IfStmt* obj)
{

}

void
EmitIR::operator()(WhileStmt* obj)
{

}

void
EmitIR::operator()(DoStmt* obj)
{

}

void
EmitIR::operator()(BreakStmt* obj)
{

}

void
EmitIR::operator()(ContinueStmt* obj)
{

}

void
EmitIR::operator()(ReturnStmt* obj)
{
  auto& irb = *mCurIrb;

  llvm::Value* retVal;
  if (!obj->expr)
    retVal = nullptr;
  else
    retVal = self(obj->expr);

  mCurIrb->CreateRet(retVal);

  auto exitBb = llvm::BasicBlock::Create(mCtx, "return_exit", mCurFunc);
  mCurIrb = std::make_unique<llvm::IRBuilder<>>(exitBb);
}

////////////////////////////////////////////////////////////////////////////////
// Declaration
////////////////////////////////////////////////////////////////////////////////

void
EmitIR::operator()(Decl* obj)
{
  if (auto p = obj->dcst<VarDecl>())
    return self(p);

  if (auto p = obj->dcst<FunctionDecl>())
    return self(p);

  ABORT();
}

void
EmitIR::trans_init(llvm::Value* val, Expr* obj)
{
  auto& irb = *mCurIrb;

  // 仅处理整数字面量的初始化
  if (auto p = obj->dcst<IntegerLiteral>()) {
    auto initVal = llvm::ConstantInt::get(self(p->type), p->val);
    irb.CreateStore(initVal, val);
    return;
  }

  // 如果表达式不是整数字面量，则中断编译
  ABORT();
}

void
EmitIR::operator()(VarDecl* obj)
{
  auto& irb = *mCurIrb;

  //! @if GLOBAL var
  if (mCurFunc == nullptr) { 
    auto ty = self(obj->type);
    // auto ty = llvm::Type::getInt32Ty(mCtx);
    auto gvar = new llvm::GlobalVariable(
      mMod, ty, false, llvm::GlobalVariable::ExternalLinkage, nullptr, obj->name
    );

    obj->any = gvar;

    // default set to 0
    gvar->setInitializer(llvm::Constant::getNullValue(ty));

    if (obj->init == nullptr)
      return;

    // create ctor for init the global value
    mCurFunc = llvm::Function::Create(
      mCtorTy, llvm::GlobalVariable::PrivateLinkage, "ctor_" + obj->name, mMod
    );
    llvm::appendToGlobalCtors(mMod, mCurFunc, 65535);

    auto entryBb = llvm::BasicBlock::Create(mCtx, "entry", mCurFunc);
    mCurIrb = std::make_unique<llvm::IRBuilder<>>(entryBb);
    trans_init(gvar, obj->init);
    mCurIrb->CreateRet(nullptr);
    mCurFunc = nullptr;
  } 
  
  //! @else LOCAL var
  else {  
    llvm::AllocaInst *var = irb.CreateAlloca(
      self(obj->type), nullptr, std::move(obj->name)
    );

    obj->any = var;

    if (obj->init) {
      irb.CreateStore(self(obj->init), var);
    }
  }
}

void
EmitIR::operator()(FunctionDecl* obj)
{
  // create function
  auto fty = llvm::dyn_cast<llvm::FunctionType>(self(obj->type));
  auto func = llvm::Function::Create(
    fty, llvm::GlobalVariable::ExternalLinkage, obj->name, mMod);

  obj->any = func;

  if (obj->body == nullptr)
    return;
  
  // Function `define`
  auto entryBb = llvm::BasicBlock::Create(mCtx, "entry", func);
  mCurIrb = std::make_unique<llvm::IRBuilder<>>(entryBb);
  auto& entryIrb = *mCurIrb;

  // TODO: 添加对函数参数的处理

  // translate the function body
  mCurFunc = func;
  self(obj->body);
  auto& exitIrb = *mCurIrb;

  // add a terminator inst
  if (fty->getReturnType()->isVoidTy())
    exitIrb.CreateRetVoid();
  else
    exitIrb.CreateUnreachable();

  // function end: reset the mCurFunc
  mCurFunc = nullptr;
}

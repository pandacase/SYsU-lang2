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

  Type subt;
  subt.spec = type->spec;
  subt.qual = type->qual;
  subt.texp = type->texp->sub;
  if (auto p = type->texp->dcst<PointerType>()) {
    return self(&subt)->getPointerTo();
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
    case UnaryExpr::kNeg: {
      auto res = irb.CreateNeg(Val);
      res->setName(std::move("neg"));
      return res;
    }
    case UnaryExpr::kNot: {
      auto valTy = Val->getType(); 
      auto toBool = irb.CreateICmpNE(Val, llvm::ConstantInt::get(valTy, 0));
      toBool->setName(std::move("tobool"));
      auto res = irb.CreateNot(toBool);
      res->setName(std::move("not"));
      return res;
    }
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
    case BinaryExpr::kMul:  {
      auto res = irb.CreateMul(lftVal, rhtVal);
      res->setName(std::move("mul"));
      return res;
    }
    case BinaryExpr::kDiv:  {
      auto res = irb.CreateSDiv(lftVal, rhtVal);
      res->setName(std::move("div"));
      return res;
    }
    case BinaryExpr::kMod:{
      auto res = irb.CreateSRem(lftVal, rhtVal);
      res->setName(std::move("rem"));
      return res;
    }
    case BinaryExpr::kAdd: {
      auto res = irb.CreateAdd(lftVal, rhtVal);
      res->setName(std::move("add"));
      return res;
    }
    case BinaryExpr::kSub: {
      auto res = irb.CreateSub(lftVal, rhtVal);
      res->setName(std::move("sub"));
      return res;
    }
    case BinaryExpr::kGt: {
      auto res = irb.CreateICmpSGT(lftVal, rhtVal);
      res->setName(std::move("cmp"));
      return res;
    }
    case BinaryExpr::kLt: {
      auto res = irb.CreateICmpSLT(lftVal, rhtVal);
      res->setName(std::move("cmp"));
      return res;
    }
    case BinaryExpr::kGe: {
      auto res = irb.CreateICmpSGE(lftVal, rhtVal);
      res->setName(std::move("cmp"));
      return res;
    }
    case BinaryExpr::kLe: {
      auto res = irb.CreateICmpSLE(lftVal, rhtVal);
      res->setName(std::move("cmp"));
      return res;
    }
    case BinaryExpr::kEq: {
      auto res = irb.CreateICmpEQ(lftVal, rhtVal);
      res->setName(std::move("cmp"));
      return res;
    }
    case BinaryExpr::kNe: {
      auto res = irb.CreateICmpNE(lftVal, rhtVal);
      res->setName(std::move("cmp"));
      return res;
    }
    case BinaryExpr::kAnd: {
      auto curBb = mCurIrb->GetInsertBlock();
      auto lorRhsBb = llvm::BasicBlock::Create(mCtx, "lor.rhs", mCurFunc);
      auto lorEndBb = llvm::BasicBlock::Create(mCtx, "lor.end", mCurFunc);
      // in current block
      mCurIrb->CreateCondBr(lftVal, lorRhsBb, lorEndBb);
      
      // in RHS block
      mCurIrb = std::make_unique<llvm::IRBuilder<>>(lorRhsBb);
      mCurIrb->CreateBr(lorEndBb);
      
      // in END block
      mCurIrb = std::make_unique<llvm::IRBuilder<>>(lorEndBb);
      llvm::PHINode *phi = irb.CreatePHI(llvm::Type::getInt1Ty(mCtx), 2, "merge");
      phi->addIncoming(mCurIrb->getInt1(false), curBb);
      phi->addIncoming(rhtVal, lorRhsBb);

      return phi;
    }
    case BinaryExpr::kOr: {
      auto curBb = mCurIrb->GetInsertBlock();
      auto lorRhsBb = llvm::BasicBlock::Create(mCtx, "lor.rhs", mCurFunc);
      auto lorEndBb = llvm::BasicBlock::Create(mCtx, "lor.end", mCurFunc);
      // in current block
      mCurIrb->CreateCondBr(lftVal, lorEndBb, lorRhsBb);
      
      // in RHS block
      mCurIrb = std::make_unique<llvm::IRBuilder<>>(lorRhsBb);
      mCurIrb->CreateBr(lorEndBb);
      
      // in END block
      mCurIrb = std::make_unique<llvm::IRBuilder<>>(lorEndBb);
      llvm::PHINode *phi = irb.CreatePHI(llvm::Type::getInt1Ty(mCtx), 2, "merge");
      phi->addIncoming(mCurIrb->getInt1(true), curBb);
      phi->addIncoming(rhtVal, lorRhsBb);

      return phi;
    }
    case BinaryExpr::kAssign:
      irb.CreateStore(rhtVal, lftVal);
      return rhtVal;
    case BinaryExpr::kComma:
      break;
    case BinaryExpr::kIndex: {
      std::vector<llvm::Value*> idxList{
        irb.getInt64(0), rhtVal
      };
      auto res = irb.CreateInBoundsGEP(self(obj->lft->type), lftVal, idxList);
      return res;
    }
    default:
      ABORT();
  }
}

llvm::Value*
EmitIR::operator()(CallExpr* obj)
{
  auto& irb = *mCurIrb;
  
  auto calleeFunc = mMod.getFunction(self(obj->head)->getName());
  
  std::vector<llvm::Value*> argsVector;
  for (auto&& arg : obj->args) {
    argsVector.push_back(self(arg));
  }
  auto argsRef = llvm::ArrayRef<llvm::Value*>(argsVector);

  return irb.CreateCall(calleeFunc, argsRef);
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
      return sub;
    }

    case ImplicitCastExpr::kFunctionToPointerDecay: {
      return sub;
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
  if (auto p = obj->expr)
    self(p);
}

void
EmitIR::operator()(CompoundStmt* obj)
{
  for (auto&& stmt : obj->subs)
    self(stmt);
}

void
EmitIR::operator()(IfStmt* obj)
{
  llvm::BasicBlock* ifThenBb = llvm::BasicBlock::Create(
    mCtx, "if.then", mCurFunc
  );
  llvm::BasicBlock* ifElseBb = llvm::BasicBlock::Create(
    mCtx, "if.else", mCurFunc
  );
  llvm::BasicBlock* ifEndBb = llvm::BasicBlock::Create(
    mCtx, "if.end", mCurFunc
  );

  auto condVal = self(obj->cond);
  auto condValTy = condVal->getType();
  auto condBool = mCurIrb->CreateICmpNE(condVal, llvm::ConstantInt::get(condValTy, 0));
  condBool->setName(std::move("tobool"));
  mCurIrb->CreateCondBr(condBool, ifThenBb, ifElseBb);

  //! @c if.then block
  mCurIrb = std::make_unique<llvm::IRBuilder<>>(ifThenBb);
  self(obj->then);
  //! @note after self(), the mCurIrb may not be if_xxx_block
  if (mCurIrb->GetInsertBlock()->getTerminator() == nullptr)
    mCurIrb->CreateBr(ifEndBb);

  //! @c if.else block
  mCurIrb = std::make_unique<llvm::IRBuilder<>>(ifElseBb);
  if (obj->else_)
    self(obj->else_);
  //! @note after self(), the mCurIrb may not be if_xxx_block
  if (mCurIrb->GetInsertBlock()->getTerminator() == nullptr)
    mCurIrb->CreateBr(ifEndBb);

  //! @c if.end block
  mCurIrb = std::make_unique<llvm::IRBuilder<>>(ifEndBb);
}

void
EmitIR::operator()(WhileStmt* obj)
{
  llvm::BasicBlock* while_cond_block = llvm::BasicBlock::Create(
    mCtx, "while.cond", mCurFunc
  );
  llvm::BasicBlock* while_body_block = llvm::BasicBlock::Create(
    mCtx, "while.body", mCurFunc
  );
  llvm::BasicBlock* while_end_block = llvm::BasicBlock::Create(
    mCtx, "while.end", mCurFunc
  );

  obj->any = while_body_block;

  mCurIrb->CreateBr(while_cond_block);

  //! @c while.cond block
  mCurIrb = std::make_unique<llvm::IRBuilder<>>(while_cond_block);
  auto condVal = self(obj->cond);
  auto condValTy = condVal->getType();
  auto condBool = mCurIrb->CreateICmpNE(condVal, llvm::ConstantInt::get(condValTy, 0));
  condBool->setName(std::move("tobool"));
  mCurIrb->CreateCondBr(condBool, while_body_block, while_end_block);

  //! @c while.body block
  mCurIrb = std::make_unique<llvm::IRBuilder<>>(while_body_block);
  self(obj->body);
  //! @note after self(), the mCurIrb may not be while_xxx_block
  if (mCurIrb->GetInsertBlock()->getTerminator() == nullptr)
    mCurIrb->CreateBr(while_cond_block);

  //! @c while.end block
  mCurIrb = std::make_unique<llvm::IRBuilder<>>(while_end_block);
}

void
EmitIR::operator()(DoStmt* obj)
{

}

void
EmitIR::operator()(BreakStmt* obj)
{
  // currently in the while.body block
  auto whileBodyBlock = reinterpret_cast<llvm::BasicBlock*>(obj->loop->any);
  
  for (auto it = mCurFunc->begin(); it != mCurFunc->end(); ++it) {
    if (&(*it) == whileBodyBlock) {
      auto prevIt = ++it;
      // br to while.end block
      mCurIrb->CreateBr(&(*prevIt));
      break;
    }
  }
}

void
EmitIR::operator()(ContinueStmt* obj)
{
  // currently in the while.body block
  auto whileBodyBlock = reinterpret_cast<llvm::BasicBlock*>(obj->loop->any);
  
  for (auto it = mCurFunc->begin(); it != mCurFunc->end(); ++it) {
    if (&(*it) == whileBodyBlock) {
      auto prevIt = --it;
      // br to while.cond block
      mCurIrb->CreateBr(&(*prevIt));
      break;
    }
  }
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

  if (auto p = obj->dcst<IntegerLiteral>()) {
    auto initVal = llvm::ConstantInt::get(self(p->type), p->val);
    irb.CreateStore(initVal, val);
    return;
  } else if (auto p = obj->dcst<InitListExpr>()) {
    
  } else {
    irb.CreateStore(self(obj), val);
  }
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
    // move `alloca` to entryBlock
    auto entryBb = &mCurFunc->getEntryBlock();
    llvm::AllocaInst* var;
    if (entryBb->getTerminator() != nullptr) {
      var = llvm::IRBuilder<>(entryBb->getTerminator()).CreateAlloca(
        self(obj->type), nullptr, std::move(obj->name)
      );
    } else {
      var = llvm::IRBuilder<>(entryBb).CreateAlloca(
        self(obj->type), nullptr, std::move(obj->name)
      );
    }

    obj->any = var;

    if (obj->init) {
      // irb.CreateStore(self(obj->init), var);
      trans_init(var, obj->init);
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
  
  // Function entry
  auto entryBb = llvm::BasicBlock::Create(mCtx, "entry", func);
  mCurIrb = std::make_unique<llvm::IRBuilder<>>(entryBb);
  auto& entryIrb = *mCurIrb;

  // Function parmas
  auto argIt = func->arg_begin();
  for (auto&& paramDecl : obj->params) {
    // set name
    argIt->setName(paramDecl->name);

    // alloc for params
    auto *var = mCurIrb->CreateAlloca(
      self(paramDecl->type), nullptr, std::move(paramDecl->name + ".addr")
    );
    paramDecl->any = var;
    mCurIrb->CreateStore(&(*argIt), var);

    ++argIt;
  }

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

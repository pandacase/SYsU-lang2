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
  if (auto p = type->texp->dcst<ArrayType>()) {
    return llvm::ArrayType::get(self(&subt), p->len);
  } else if (auto p = type->texp->dcst<PointerType>()) {
    return self(&subt)->getPointerTo();
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
  llvm::Value *val;

  val = self(obj->sub);

  switch (obj->op) {
    case UnaryExpr::kPos:
      return val;
    case UnaryExpr::kNeg: {
      checkToExt(val);
      return mCurIrb->CreateNeg(val, std::move("sub"));
    }
    //! @note In C/CPP, Not operator is logical not, implicitly convert `int`
    //! to `bool`.
    case UnaryExpr::kNot: {
      checkToBool(val);
      return mCurIrb->CreateNot(val, std::move("lnot"));
    }
    default:
      ABORT();
  }
}

llvm::Value*
EmitIR::operator()(BinaryExpr* obj)
{
  //! @if Logical And & Or: short_circuit case.
  if (obj->op == BinaryExpr::kAnd) {
    auto lftVal = self(obj->lft);
    checkToBool(lftVal);

    auto curBb = mCurIrb->GetInsertBlock();
    auto lorRhsBb = llvm::BasicBlock::Create(mCtx, "land.rhs", mCurFunc);
    auto lorEndBb = llvm::BasicBlock::Create(mCtx, "land.end", mCurFunc);
    
    // in current block
    mCurIrb->CreateCondBr(lftVal, lorRhsBb, lorEndBb);
    
    // in RHS block
    mCurIrb = std::make_unique<llvm::IRBuilder<>>(lorRhsBb);
    auto rhtVal = self(obj->rht);
    //! @details after self(obj->rht), the curr block may not be lorRhsBb
    //! so we store the curr block here to merge for phi:
    auto rhtCurBb = mCurIrb->GetInsertBlock();
    checkToBool(rhtVal);
    mCurIrb->CreateBr(lorEndBb);
    
    // in END block
    mCurIrb = std::make_unique<llvm::IRBuilder<>>(lorEndBb);
    llvm::PHINode *phi = mCurIrb->CreatePHI(
      llvm::Type::getInt1Ty(mCtx), 2, "merge");
    phi->addIncoming(mCurIrb->getInt1(false), curBb);
    phi->addIncoming(rhtVal, rhtCurBb);

    return phi;
  } else if (obj->op == BinaryExpr::kOr) {
    auto lftVal = self(obj->lft);
    checkToBool(lftVal);

    auto curBb = mCurIrb->GetInsertBlock();
    auto lorRhsBb = llvm::BasicBlock::Create(mCtx, "lor.rhs", mCurFunc);
    auto lorEndBb = llvm::BasicBlock::Create(mCtx, "lor.end", mCurFunc);
    
    // in current block
    mCurIrb->CreateCondBr(lftVal, lorEndBb, lorRhsBb);
    
    // in RHS block
    mCurIrb = std::make_unique<llvm::IRBuilder<>>(lorRhsBb);
    auto rhtVal = self(obj->rht);
    //! @details after self(obj->rht), the curr block may not be lorRhsBb
    //! so we store the curr block here to merge for phi:
    auto rhtCurBb = mCurIrb->GetInsertBlock();
    checkToBool(rhtVal);
    mCurIrb->CreateBr(lorEndBb);
    
    // in END block
    mCurIrb = std::make_unique<llvm::IRBuilder<>>(lorEndBb);
    llvm::PHINode *phi = mCurIrb->CreatePHI(
      llvm::Type::getInt1Ty(mCtx), 2, "merge");
    phi->addIncoming(mCurIrb->getInt1(true), curBb);
    phi->addIncoming(rhtVal, rhtCurBb);

    return phi;
  }
  
  //! @else normal case.
  else {
    llvm::Value *lftVal, *rhtVal;

    lftVal = self(obj->lft);
    rhtVal = self(obj->rht);

    switch (obj->op) {
      case BinaryExpr::kMul:  {
        checkToExt(lftVal);
        checkToExt(rhtVal);
        return mCurIrb->CreateMul(lftVal, rhtVal, std::move("mul"));
      }
      case BinaryExpr::kDiv:  {
        checkToExt(lftVal);
        checkToExt(rhtVal);
        return mCurIrb->CreateSDiv(lftVal, rhtVal, std::move("div"));
      }
      case BinaryExpr::kMod:{
        checkToExt(lftVal);
        checkToExt(rhtVal);
        return mCurIrb->CreateSRem(lftVal, rhtVal, std::move("rem"));
      }
      case BinaryExpr::kAdd: {
        checkToExt(lftVal);
        checkToExt(rhtVal);
        return mCurIrb->CreateAdd(lftVal, rhtVal, std::move("add"));
      }
      case BinaryExpr::kSub: {
        checkToExt(lftVal);
        checkToExt(rhtVal);
        return mCurIrb->CreateSub(lftVal, rhtVal, std::move("sub"));
      }
      case BinaryExpr::kGt: {
        return mCurIrb->CreateICmpSGT(lftVal, rhtVal, std::move("cmp"));
      }
      case BinaryExpr::kLt: {
        return mCurIrb->CreateICmpSLT(lftVal, rhtVal, std::move("cmp"));
      }
      case BinaryExpr::kGe: {
        return mCurIrb->CreateICmpSGE(lftVal, rhtVal, std::move("cmp"));
      }
      case BinaryExpr::kLe: {
        return mCurIrb->CreateICmpSLE(lftVal, rhtVal, std::move("cmp"));
      }
      case BinaryExpr::kEq: {
        return mCurIrb->CreateICmpEQ(lftVal, rhtVal, std::move("cmp"));
      }
      case BinaryExpr::kNe: {
        return mCurIrb->CreateICmpNE(lftVal, rhtVal, std::move("cmp"));
      }
      case BinaryExpr::kAssign: {
        mCurIrb->CreateStore(rhtVal, lftVal);
        return rhtVal;
      }
      case BinaryExpr::kComma: {
        break;
      }
      case BinaryExpr::kIndex: {
        // Sigh ext to i64
        auto idxExt = mCurIrb->CreateSExt(
          rhtVal, mCurIrb->getInt64Ty(), std::move("idxprom"));
        // get array type
        auto arrayTy = self(obj->lft->dcst<ImplicitCastExpr>()->sub->type);
        // check if is array type, if not, adjust the idxList
        std::vector<llvm::Value*> idxList;
        if (llvm::dyn_cast<llvm::ArrayType>(arrayTy))
          idxList = { mCurIrb->getInt64(0), idxExt };
        else if (llvm::dyn_cast<llvm::PointerType>(arrayTy)) {
          idxList = { idxExt };
          arrayTy = self(obj->type);
        }
        
        return mCurIrb->CreateInBoundsGEP(
          arrayTy, lftVal, std::move(idxList), std::move("arrayidx"));
      }
      default:
        ABORT();
    }
  }

  return nullptr;
}

llvm::Value*
EmitIR::operator()(CallExpr* obj)
{ 
  auto calleeFunc = mMod.getFunction(self(obj->head)->getName());
  
  std::vector<llvm::Value*> argsVector;
  for (auto arg : obj->args) {
    auto argVal = self(arg);
    //! @note in c code:
    //! when pass a array to a function param, will decay the array.
    if (auto p = arg->dcst<ImplicitCastExpr>()) {
      if (p->kind == ImplicitCastExpr::kArrayToPointerDecay) {
        std::vector<llvm::Value*> idxList{ 
          mCurIrb->getInt64(0), mCurIrb->getInt64(0) };
        argVal = mCurIrb->CreateInBoundsGEP(
          self(p->sub->type), self(p->sub), idxList, "arraydecay");
      }
    }
    argsVector.push_back(argVal);
  }

  auto ret = mCurIrb->CreateCall(calleeFunc, std::move(argsVector));
  if (!calleeFunc->getFunctionType()->getReturnType()->isVoidTy())
    ret->setName(std::move("call"));
  return ret;
}

llvm::Value*
EmitIR::operator()(InitListExpr* obj, llvm::Value* var, llvm::Type* ty)
{  
  auto arrTy = llvm::dyn_cast<llvm::ArrayType>(ty);
  for (int i = 0; i < arrTy->getNumElements(); ++i) {
    //! temporary variable
    std::vector<llvm::Value*> idxList{ 
      mCurIrb->getInt64(0), mCurIrb->getInt64(i) };
    auto subVar = mCurIrb->CreateInBoundsGEP(arrTy, var, idxList);
    auto elementTy = arrTy->getElementType();

    //! @if element type is still ArrayType
    if (llvm::dyn_cast<llvm::ArrayType>(elementTy)) {
      //! @details if in C source code `size of initList` < `size of array`
      //! the ast will append a `array_filler` in the InitListExpr
      if (obj->list[0]->dcst<ImplicitInitExpr>()) {
        if (i < obj->list.size() - 1) {
          auto subObj = (obj->list[i + 1])->dcst<InitListExpr>();
          self(subObj, subVar, elementTy);
        } else {
          auto newInitList = new InitListExpr();
          newInitList->list.push_back(new ImplicitInitExpr());
          self(newInitList, subVar, elementTy);
        }
      } else {
        if (i < obj->list.size()) {
          auto subObj = (obj->list[i])->dcst<InitListExpr>();
          self(subObj, subVar, elementTy);
        } else {
          auto newInitList = new InitListExpr();
          newInitList->list.push_back(new ImplicitInitExpr());
          self(newInitList, subVar, elementTy);
        }
      }
    }
    
    //! @else element type is not ArrayType (such as i32)
    else {
      //! @details if in C source code `size of initList` < `size of array`
      //! the ast will append a `array_filler` in the InitListExpr
      if (obj->list[0]->dcst<ImplicitInitExpr>()) {
        if (i < obj->list.size() - 1) {
          mCurIrb->CreateStore(self(obj->list[i + 1]), subVar);
        } else {
          mCurIrb->CreateStore(llvm::ConstantInt::get(elementTy, 0), subVar);
        }
      } else {
        if (i < obj->list.size()) {
          mCurIrb->CreateStore(self(obj->list[i]), subVar);
        } else {
          mCurIrb->CreateStore(llvm::ConstantInt::get(elementTy, 0), subVar);
        }
      }
    }
  }
  return nullptr;
}

llvm::Value*
EmitIR::operator()(ImplicitInitExpr* obj)
{
  auto ty = reinterpret_cast<llvm::Type*>(obj->any);
  return llvm::Constant::getNullValue(ty);
}

llvm::Value*
EmitIR::operator()(ImplicitCastExpr* obj)
{
  auto sub = self(obj->sub);

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
  checkToBool(condVal);
  mCurIrb->CreateCondBr(condVal, ifThenBb, ifElseBb);

  //! @c if.then block
  mCurIrb = std::make_unique<llvm::IRBuilder<>>(ifThenBb);
  self(obj->then);
  //! @note after self(), the mCurIrb may not be if_then_block
  //! @details Continue/Break Statement will unconditionally add a terminator,
  //! which make the # of terminator in total be greater than the # of block
  //! in total. So, here would need to avoid creating redundant terminator.
  if (mCurIrb->GetInsertBlock()->getTerminator() == nullptr)
    mCurIrb->CreateBr(ifEndBb);

  //! @c if.else block
  mCurIrb = std::make_unique<llvm::IRBuilder<>>(ifElseBb);
  if (obj->else_)
    self(obj->else_);
  //! @note after self(), the mCurIrb may not be if_else_block
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
  checkToBool(condVal);
  mCurIrb->CreateCondBr(condVal, while_body_block, while_end_block);

  //! @c while.body block
  mCurIrb = std::make_unique<llvm::IRBuilder<>>(while_body_block);
  self(obj->body);
  //! @note after self(), the mCurIrb may not be while_body_block
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
EmitIR::operator()(VarDecl* obj)
{
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
    // in ctor's entry block now:
    auto entryBb = llvm::BasicBlock::Create(mCtx, "entry", mCurFunc);
    mCurIrb = std::make_unique<llvm::IRBuilder<>>(entryBb);
    obj->init->any = ty;
    trans_init(obj->init, gvar);
    mCurIrb->CreateRet(nullptr);
    mCurFunc = nullptr;
  }

  //! @else LOCAL var
  else {  
    // move `alloca` to entryBlock
    auto entryBb = &mCurFunc->getEntryBlock();
    auto varTy = self(obj->type);
    llvm::AllocaInst* var;
    if (entryBb->getTerminator() != nullptr) {
      var = llvm::IRBuilder<>(entryBb->getTerminator()).CreateAlloca(
        varTy, nullptr, std::move(obj->name)
      );
    } else {
      var = llvm::IRBuilder<>(entryBb).CreateAlloca(
        varTy, nullptr, std::move(obj->name)
      );
    }

    //! @details About ArrayType:
    //! when `varTy` is a llvm::ArrayType
    //! after alloca, `var->getType()` decay to PointerType

    obj->any = var;

    if (obj->init) {
      obj->init->any = varTy;
      // mCurIrb->CreateStore(self(obj->init), var);
      trans_init(obj->init, var);
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


////////////////////////////////////////////////////////////////////////////////
// Helper
////////////////////////////////////////////////////////////////////////////////

//! @param obj Expr* VarDecl::init
//! @param val llvm::Value* variable
void
EmitIR::trans_init(Expr* obj, llvm::Value* var)
{
  if (auto init = obj->dcst<InitListExpr>()) {
    self(init, var, reinterpret_cast<llvm::ArrayType*>(obj->any));
  } else {
    mCurIrb->CreateStore(self(obj), var);
  }
}

//! @brief check if the `val` is i1, if not, tobool
void 
EmitIR::checkToBool(llvm::Value* &val)
{
  // check rhtVal and tobool
  if (!val->getType()->isIntegerTy(1)) {
    auto valTy = val->getType();
    auto tobool = mCurIrb->CreateICmpNE(
      val, llvm::ConstantInt::get(valTy, 0));
    tobool->setName(std::move("tobool"));
    val = tobool;
  }
}

//! @brief check if the `val` is i32, if not, ext
void 
EmitIR::checkToExt(llvm::Value* &val)
{
  // check if val is i32
  if (!val->getType()->isIntegerTy(32)) {
    auto valExt = mCurIrb->CreateZExt(val, mCurIrb->getInt32Ty());
    valExt->setName(std::move(val->getName() + ".ext"));
    val = valExt;
  }
}

/*
	The Infinity Virtual Machine - Exact portable representation
	Copyright (C) 2020  Keith Hammond

	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU Lesser General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU Lesser General Public License for more details.

	You should have received a copy of the GNU Lesser General Public License
	along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

#include <cstdlib>
#include <cassert>
#include <cstdio>
#include <cmath>

#include <memory>
#include <vector>
#include <string_view>
#include <functional>
#include <map>
#include <bitset>
#include <list>

#include "sexi.h"

#include "ivm/IR.h"

struct IvmIRTypeT{
	virtual ~IvmIRTypeT() = default;
	virtual size_t numBits() const noexcept{ return 0; }
};

struct IvmIRVoidTypeT: IvmIRTypeT{};

struct IvmIRBoolTypeT: IvmIRTypeT{
	size_t numBits() const noexcept override{ return 1; }
};

struct IvmIRIntTypeT: IvmIRTypeT{
	explicit IvmIRIntTypeT(size_t numBits_) noexcept
		: m_numBits(numBits_){}

	size_t numBits() const noexcept override{ return m_numBits; }

	size_t m_numBits;
};

struct IvmIRNatTypeT: IvmIRTypeT{
	explicit IvmIRNatTypeT(size_t numBits_) noexcept
		: m_numBits(numBits_){}

	size_t numBits() const noexcept override{ return m_numBits; }

	size_t m_numBits;
};

static const auto voidType = IvmIRVoidTypeT();

static const auto boolType = IvmIRBoolTypeT();

static const auto int8Type = IvmIRIntTypeT(8);
static const auto int16Type = IvmIRIntTypeT(16);
static const auto int32Type = IvmIRIntTypeT(32);
static const auto int64Type = IvmIRIntTypeT(64);

static const auto nat8Type = IvmIRNatTypeT(8);
static const auto nat16Type = IvmIRNatTypeT(16);
static const auto nat32Type = IvmIRNatTypeT(32);
static const auto nat64Type = IvmIRNatTypeT(64);

size_t ivmIRTypeNumBits(IvmIRType ty){ return ty->numBits(); }

IvmIRVoidType ivmIRTypeVoid(){ return &voidType; }

IvmIRBoolType ivmIRTypeBool(){ return &boolType; }

IvmIRIntType ivmIRTypeInt(size_t numBits){
	switch(numBits){
		case 8: return &int8Type;
		case 16: return &int16Type;
		case 32: return &int32Type;
		case 64: return &int64Type;
		default: return nullptr;
	}
}

IvmIRNatType ivmIRTypeNat(size_t numBits){
	switch(numBits){
		case 8: return &nat8Type;
		case 16: return &nat16Type;
		case 32: return &nat32Type;
		case 64: return &nat64Type;
		default: return nullptr;
	}
}

struct IvmIRNodeT{
	explicit IvmIRNodeT(IvmIRType type_ = nullptr): type(type_){}
	virtual ~IvmIRNodeT() = default;

	IvmIRType type;
};

struct IvmIRStackMemT: IvmIRNodeT{
	using IvmIRNodeT::IvmIRNodeT;
};

struct IvmIRLoadT: IvmIRNodeT{
	using IvmIRNodeT::IvmIRNodeT;
	IvmIRLocalMem mem;
};

struct IvmIRStoreT: IvmIRNodeT{
	using IvmIRNodeT::IvmIRNodeT;
	IvmIRLocalMem mem;
	IvmIRNode val;
};

struct IvmIRLiteralT: IvmIRNodeT{
	using IvmIRNodeT::IvmIRNodeT;
	union {
		union {
			int8_t v8;
			int16_t v16;
			int32_t v32;
			int64_t v64;
		} i;

		union {
			uint8_t v8;
			uint16_t v16;
			uint32_t v32;
			uint64_t v64;
		} n;

		uint64_t bits;
	} val;
};

struct IvmIRParamT: IvmIRNodeT{
	using IvmIRNodeT::IvmIRNodeT;
	size_t idx;
};

struct IvmIRLambdaT: IvmIRNodeT{
	using IvmIRNodeT::IvmIRNodeT;
	IvmIRType resultTy;
	std::vector<IvmIRParamT> params;
	size_t bodyLen;
	const IvmIRNode *body;
};

IvmIRLocalMem ivmir_localN(IvmIRType type, uint64_t n){
	return {
		.base = { .nodeType = IVM_IR_LOCAL, .type = type },
		.len = n
	};
}

IvmIRLocalMem ivmir_local(IvmIRType type){ return ivmir_localN(type, 1); }

IvmIRLoad ivmir_load(IvmIRLocalMemPtr mem){
	return {
		.base = { .nodeType = IVM_IR_LOAD, .type = mem->base.type },
		.mem = mem
	};
}

IvmIRStore ivmir_store(IvmIRLocalMemPtr mem, IvmIRNodePtr val){
	return {
		.base = { .nodeType = IVM_IR_STORE, .type = mem->base.type },
		.mem = mem,
		.value = val
	};
}

IvmIRLiteral ivmir_litInt32(int32_t val){
	return {
		.base = { .nodeType = IVM_IR_LITERAL, .type = ivmIRTypeInt(32) },
		.value = { .i32 = val }
	};
}

IvmIRLiteral ivmir_litNat32(uint32_t val){
	return {
		.base = { .nodeType = IVM_IR_LITERAL, .type = ivmIRTypeNat(32) },
		.value = { .n32 = val }
	};
}

IvmIRBinaryOp ivmir_binaryOp(IvmIRBinaryOperator op, IvmIRNodePtr lhs, IvmIRNodePtr rhs) noexcept{
	return {
		.base = { .nodeType = IVM_IR_BINARYOP, .type = ivmBinaryOperatorIsLogic(op) ? &boolType : lhs->type },
		.op = op,
		.lhs = lhs, .rhs = rhs
	};
}

IvmIRBinaryOp ivmir_add(IvmIRNodePtr lhs, IvmIRNodePtr rhs){ return ivmir_binaryOp(IVM_IRBINARYOP_ADD, lhs, rhs); }
IvmIRBinaryOp ivmir_sub(IvmIRNodePtr lhs, IvmIRNodePtr rhs){ return ivmir_binaryOp(IVM_IRBINARYOP_SUB, lhs, rhs); }
IvmIRBinaryOp ivmir_mul(IvmIRNodePtr lhs, IvmIRNodePtr rhs){ return ivmir_binaryOp(IVM_IRBINARYOP_MUL, lhs, rhs); }
IvmIRBinaryOp ivmir_div(IvmIRNodePtr lhs, IvmIRNodePtr rhs){ return ivmir_binaryOp(IVM_IRBINARYOP_DIV, lhs, rhs); }

IvmIRBinaryOp ivmir_lt(IvmIRNodePtr lhs, IvmIRNodePtr rhs){ return ivmir_binaryOp(IVM_IRBINARYOP_LT, lhs, rhs); }
IvmIRBinaryOp ivmir_gt(IvmIRNodePtr lhs, IvmIRNodePtr rhs){ return ivmir_binaryOp(IVM_IRBINARYOP_GT, lhs, rhs); }
IvmIRBinaryOp ivmir_le(IvmIRNodePtr lhs, IvmIRNodePtr rhs){ return ivmir_binaryOp(IVM_IRBINARYOP_LE, lhs, rhs); }
IvmIRBinaryOp ivmir_ge(IvmIRNodePtr lhs, IvmIRNodePtr rhs){ return ivmir_binaryOp(IVM_IRBINARYOP_GE, lhs, rhs); }
IvmIRBinaryOp ivmir_eq(IvmIRNodePtr lhs, IvmIRNodePtr rhs){ return ivmir_binaryOp(IVM_IRBINARYOP_EQ, lhs, rhs); }
IvmIRBinaryOp ivmir_ne(IvmIRNodePtr lhs, IvmIRNodePtr rhs){ return ivmir_binaryOp(IVM_IRBINARYOP_NE, lhs, rhs); }

IvmIRUnaryOp ivmir_unaryOp(IvmIRUnaryOperator op, IvmIRNodePtr value) noexcept{
	return {
		.base = { .nodeType = IVM_IR_UNARYOP, .type = ivmUnaryOperatorIsLogic(op) ? &boolType : value->type },
		.op = op,
		.value = value
	};
}

IvmIRUnaryOp ivmir_inc(IvmIRNodePtr value){ return ivmir_unaryOp(IVM_IRUNARYOP_INC, value); }
IvmIRUnaryOp ivmir_dec(IvmIRNodePtr value){ return ivmir_unaryOp(IVM_IRUNARYOP_DEC, value); }
IvmIRUnaryOp ivmir_not(IvmIRNodePtr value){ return ivmir_unaryOp(IVM_IRUNARYOP_NOT, value); }

IvmIRParam ivmir_param(IvmIRType type){
	return {
		.base = { .base = { .nodeType = IVM_IR_PARAM, .type = type } }
	};
}

IvmIRFunction ivmir_fn(
	size_t nameLen, const char *name,
	IvmIRType resultTy, size_t numParams, const IvmIRParam *params,
	IvmIRBlockPtr body
){
	auto ret = IvmIRFunction{
		.base = ivmir_lambda(resultTy, numParams, params, body),
		.linkType = IVM_IRLINK_EXPORT,
		.nameLen = nameLen,
		.name = name
	};

	ret.base.base.nodeType = IVM_IR_FN;

	return ret;
}

IvmIRFunction ivmir_fnUndef(
	size_t nameLen, const char *name,
	IvmIRType resultTy, size_t numParams, const IvmIRParam *params
){
	return ivmir_fn(nameLen, name, resultTy, numParams, params, nullptr);
}

IvmIRLambda ivmir_lambdaUndef(IvmIRType resultTy, size_t numParams, const IvmIRParam *params){
	return ivmir_lambda(resultTy, numParams, params, nullptr);
}

IvmIRLambda ivmir_lambda(IvmIRType resultTy, size_t numParams, const IvmIRParam *params, IvmIRBlockPtr body){
	return {
		.base = { .nodeType = IVM_IR_LAMBDA, .type = nullptr }, // TODO: add lambda/function types
		.resultTy = resultTy,
		.numParams = numParams,
		.params = params,
		.body = body
	};
}

IvmIRType IvmIRLambdaResultType(IvmIRLambda lam){ return lam.resultTy; }

size_t ivmIRLambdaNumParams(IvmIRLambda lam){ return lam.numParams; }
IvmIRParamPtr ivmIRLambdaParam(IvmIRLambda lam, size_t idx){ return &lam.params[idx]; }

bool ivmIRLambdaDef(IvmIRLambda *undefLam, IvmIRBlockPtr body){
	if(undefLam->body) return false;
	undefLam->body = body;
	return true;
}

IvmIRBlock ivmir_block(size_t len, const IvmIRNodePtr *body){
	return {
		.base = { .nodeType = IVM_IR_BLOCK, .type = nullptr },
		.len = len,
		.body = body
	};
}

IvmIRBranch ivmir_branch(IvmIRNodePtr cond, IvmIRBlockPtr trueBlock, IvmIRBlockPtr falseBlock){
	return {
		.base = { .nodeType = IVM_IR_BRANCH, .type = nullptr },
		.cond = cond,
		.trueBlock = trueBlock,
		.falseBlock = falseBlock
	};
}

IvmIRCall ivmir_call(IvmIRLambdaPtr lam, size_t numArgs, const IvmIRNodePtr *args){
	return {
		.base = { .nodeType = IVM_IR_CALL, .type = lam->resultTy },
		.lam = lam,
		.numArgs = numArgs,
		.args = args
	};
}

IvmIRReturn ivmir_return(IvmIRNodePtr value){
	return {
		.base = { .nodeType = IVM_IR_RETURN, .type = value ? value->type : &voidType },
		.value = value
	};
}

uint64_t ivmIRLiteralValue(IvmIRLiteral lit){ return lit.value.n64; }

IvmIRType ivmIRNodeType(IvmIRNodePtr node){ return node->type; }

class StackManager;

class StackHandle{
	public:
		~StackHandle();

		size_t size() const noexcept{ return std::get<0>(*m_it); }

	private:
		StackHandle(StackManager *man, std::list<std::tuple<size_t, bool>>::iterator it) noexcept
			: m_man(man), m_it(it){}

		StackManager *m_man;
		std::list<std::tuple<size_t, bool>>::iterator m_it;

		friend class StackManager;
};

class StackManager{
	public:
		StackHandle alloc(size_t size){
			for(auto it = begin(m_nodes); it != end(m_nodes); ++it){
				if(!std::get<1>(*it) && std::get<0>(*it) >= size){
					auto ds = std::get<0>(*it) - size;
					std::get<0>(*it) = size;
					std::get<1>(*it) = true;

					auto handleIt = it;

					if(ds){
						m_nodes.insert(++it, std::make_tuple(ds, false));
					}

					return StackHandle(this, handleIt);
				}
			}

			auto it = m_nodes.insert(end(m_nodes), std::make_tuple(size, true));

			return StackHandle(this, it);
		}

		size_t totalSize() const noexcept{
			size_t ret = 0;
			for(auto it = cbegin(m_nodes); it != cend(m_nodes); ++it){
				ret += std::get<0>(*it);
			}
			return ret;
		}

	private:
		std::list<std::tuple<size_t, bool>> m_nodes;

		friend class StackHandle;
};

StackHandle::~StackHandle(){
	auto next = m_it;
	++next;

	if(next != end(m_man->m_nodes)){
		if(!std::get<1>(*next)){ // not used
			std::get<0>(*m_it) += std::get<0>(*next);
			m_man->m_nodes.erase(next);
		}
	}

	if(m_it != begin(m_man->m_nodes)){
		auto prev = m_it;
		--prev;

		if(!std::get<1>(*prev)){
			std::get<0>(*prev) += std::get<0>(*m_it);
			m_man->m_nodes.erase(m_it);
		}
	}
}

struct MemoryHandle{
	enum {
		register_, stack, param
	} type;

	std::uint64_t idx;
};

struct GenCtx{
	IvmBuffer buffer;
	uint16_t labelCounter;
	std::map<IvmIRBlockPtr, uint16_t> blockIndices;
	std::map<IvmIRLambdaPtr, size_t> lambdaOffsets;
	std::vector<std::function<void(GenCtx*)>> fillFns;
	std::bitset<IVM_NUM_REGS> usedRegs = 0;
	std::map<IvmIRNodePtr, std::uint8_t> regMap;
	std::map<IvmIRLocalMemPtr, MemoryHandle> memMap;
	std::map<IvmIRNodePtr, std::size_t> instrOffsets;
	const MemoryHandle *mem = nullptr;
	IvmIRLambdaPtr lam = nullptr;
};

using LambdaGen = std::function<int(GenCtx *ctx)>;

struct CompileCtx{
	explicit CompileCtx(const CompileCtx *parent_ = nullptr, IvmIRLambdaPtr lam_ = nullptr)
		: parent(parent_), lam(lam_){}

	std::optional<IvmOperand> getParam(IvmIRParamPtr param) const{
		auto res = paramMap.find(param);
		if(res != end(paramMap)){
			return res->second;
		}
		else if(parent){
			return parent->getParam(param);
		}
		else{
			return std::nullopt;
		}
	}

	const CompileCtx *parent;
	IvmIRLambdaPtr lam;
	std::map<IvmIRParamPtr, IvmOperand> paramMap;
	std::map<IvmIRNodePtr, LambdaGen> genMap;
};

LambdaGen ivmCompileIRLambda(CompileCtx *ctx, IvmIRLambdaPtr lam, bool fnBody = false);
LambdaGen ivmCompileIRBlock(CompileCtx *ctx, IvmIRBlockPtr block);

static LambdaGen compError(int code, std::string_view str){
	std::fprintf(stderr, "Error: %.*s\n", int(str.size()), str.data());
	return [code](auto...) -> int{ return code; };
}

static inline int retError(int code, std::string_view str){
	std::fprintf(stderr, "Error: %.*s\n", int(str.size()), str.data());
	return code;
}

size_t allocRegister(GenCtx *ctx){
	size_t regIdx = IVM_NUM_REGS;

	for(size_t i = IVM_NUM_REGS; i > 0; i--){
		if(!ctx->usedRegs.test(i-1)){
			regIdx = i-1;
		}
	}

	if(regIdx < IVM_NUM_REGS){
		ctx->usedRegs[regIdx] = 1;
	}
	else{
		std::fprintf(stderr, "Error: register allocation unimplemented, ran out of registers\n");
		return SIZE_MAX;
	}

	return regIdx;
}

static inline int memToOp(const MemoryHandle *m, IvmOperand *ret){
	switch(m->type){
		case MemoryHandle::register_: *ret = IVMREG(m->idx); return 0;
		case MemoryHandle::param: *ret = IVMPARAM(m->idx); return 0;
		default: return retError(69, "stack memory not implemented");
	}
};

LambdaGen ivmCompileIRValue(CompileCtx *ctx, IvmIRNodePtr value){
	auto mapRes = ctx->genMap.find(value);
	if(mapRes != end(ctx->genMap)) return [f{&mapRes->second}](auto ... xs){ return (*f)(xs...); };

	static auto error = +[](int code, std::string_view msg) -> LambdaGen{
		return [code, msg](auto...){
			return retError(code, msg);
		};
	};

	static auto skip = +[]() -> int{ std::printf("skipping stray value expression\n"); return 0; };

	switch(value->nodeType){
		case IVM_IR_LITERAL:{
			auto lit = reinterpret_cast<const IvmIRLiteral*>(value);

			IvmOperand operand;

			if(dynamic_cast<IvmIRIntType>(value->type)){ // Integral
				switch(ivmIRTypeNumBits(value->type)){
					case 8:  operand = IVMIMM(lit->value.i8); break;
					case 16: operand = IVMIMM(lit->value.i16); break;
					case 32: operand = IVMIMM(lit->value.i32); break;
					case 64: operand = IVMIMM(lit->value.i64); break;
					default: return compError(1, "Error: only 8, 16, 32 and 64 bit integer literals supported");
				}
			}
			else if(dynamic_cast<IvmIRNatType>(value->type)){ // Natural
				switch(ivmIRTypeNumBits(value->type)){
					case 8:  operand = IVMIMM(lit->value.n8); break;
					case 16: operand = IVMIMM(lit->value.n16); break;
					case 32: operand = IVMIMM(lit->value.n32); break;
					case 64: operand = IVMIMM(lit->value.n64); break;
					default: return compError(1, "only 8, 16, 32 and 64 bit natural literals supported");
				}
			}
			else{
				return compError(69, "only integral and natural literal types implemented");
			}

			return [operand](GenCtx *ctx){
				if(!ctx->mem){
					std::fprintf(stdout, "Skipping stray value expression\n");
					return 0;
				}

				IvmOperand dst;
				memToOp(ctx->mem, &dst);

				auto setInstr = ivm_set(dst, operand);

				if(!ivmBufferWrite(ctx->buffer, sizeof(setInstr), &setInstr)){
					return retError(420, "error in ivmBufferWrite");
				}

				return 0;
			};
		}

		case IVM_IR_PARAM:
		case IVM_IR_LOCAL:{
			auto stack = reinterpret_cast<IvmIRLocalMemPtr>(value);

			return [stack](GenCtx *ctx){
				if(!ctx->mem) return skip();

				IvmOperand dst, src;

				{
					auto res = memToOp(ctx->mem, &dst);
					if(res != 0) return res;
				}

				auto memRes = ctx->memMap.find(stack);
				if(memRes != end(ctx->memMap)){
					auto res = memToOp(&memRes->second, &src);
					if(res != 0) return res;
				}
				else{
					return retError(7, "could not infer memory location in parameter value");
				}

				auto instr = ivm_set(dst, src);
				if(!ivmBufferWrite(ctx->buffer, sizeof(instr), &instr)){
					return retError(420, "error in ivmBufferWrite");
				}

				return 0;
			};
		}

		case IVM_IR_UNARYOP:{
			auto unary = reinterpret_cast<IvmIRUnaryOpPtr>(value);

			auto valueGen = ivmCompileIRValue(ctx, unary->value);

			auto prelude = [vgen{std::move(valueGen)}](GenCtx *ctx, IvmInstr(*op)(IvmOperand)){
				if(!ctx->mem) return skip();

				auto valRes = vgen(ctx);
				if(valRes != 0) return valRes;

				IvmOperand valOp;

				auto opRes = memToOp(ctx->mem, &valOp);
				if(opRes != 0) return opRes;

				auto instr = op(valOp);
				if(!ivmBufferWrite(ctx->buffer, sizeof(instr), &instr)){
					return retError(420, "error in ivmBufferWrite");
				}

				return 0;
			};

			LambdaGen gen;

			switch(unary->op){
				case IVM_IRUNARYOP_INC:{
					gen = [pre{std::move(prelude)}](GenCtx *ctx){ return pre(ctx, ivm_inc); };
					break;
				}

				case IVM_IRUNARYOP_DEC:{
					gen = [pre{std::move(prelude)}](GenCtx *ctx){ return pre(ctx, ivm_dec); };
					break;
				}

				default:{
					return compError(69, "only inc and dec unary operators implemented");
				}
			}

			return gen;
		}

		case IVM_IR_BINARYOP:{
			auto binop = reinterpret_cast<IvmIRBinaryOpPtr>(value);

			IvmInstr(*opFn)(IvmOperand, IvmOperand);

			switch(binop->op){
				case IVM_IRBINARYOP_ADD:{
					if(binop->rhs->nodeType == IVM_IR_LITERAL){
						auto litRhs = reinterpret_cast<IvmIRLiteralPtr>(binop->rhs);
						if(litRhs->value.i32 == 1){
							auto unaryOp = ivmir_unaryOp(IVM_IRUNARYOP_INC, binop->lhs);
							return ivmCompileIRValue(ctx, &unaryOp.base);
						}
					}
					
					opFn = ivm_add;
					break;
				}
				
				case IVM_IRBINARYOP_SUB:{
					if(binop->rhs->nodeType == IVM_IR_LITERAL){
						auto litRhs = reinterpret_cast<IvmIRLiteralPtr>(binop->rhs);
						if(litRhs->value.i32 == 1){
							auto unaryOp = ivmir_unaryOp(IVM_IRUNARYOP_DEC, binop->lhs);
							return ivmCompileIRValue(ctx, &unaryOp.base);
						}
					}

					opFn = ivm_sub;
					break;
				}
				
				case IVM_IRBINARYOP_MUL: opFn = ivm_mul; break;
				//case IVM_IRBINARYOP_DIV: opFn = ivm_div; break;
				default: return compError(69, "only add, sub and mul binary operators implemented");
			}

			auto lhsGen = ivmCompileIRValue(ctx, binop->lhs);
			auto rhsGen = ivmCompileIRValue(ctx, binop->rhs);

			auto prelude = [lgen{std::move(lhsGen)}, rgen{std::move(rhsGen)}]
				(GenCtx *ctx, IvmInstr(*op)(IvmOperand, IvmOperand)){
					if(!ctx->mem) return skip();

					auto lhsRes = lgen(ctx);
					if(lhsRes != 0) return lhsRes;

					auto lhsMem = ctx->mem;

					static const MemoryHandle scratch = {
						.type = MemoryHandle::register_,
						.idx = 1
					};

					ctx->mem = &scratch;

					auto rhsRes = rgen(ctx);
					if(rhsRes != 0) return rhsRes;

					auto rhsMem = ctx->mem;

					ctx->mem = lhsMem;

					IvmOperand lhsOp, rhsOp;

					auto opRes = memToOp(lhsMem, &lhsOp);
					if(opRes != 0) return opRes;

					opRes = memToOp(rhsMem, &rhsOp);
					if(opRes != 0) return opRes;

					auto instr = op(lhsOp, rhsOp);
					if(!ivmBufferWrite(ctx->buffer, sizeof(instr), &instr)){
						return retError(420, "error in ivmBufferWrite");
					}

					return 0;
				};

			return [pre{std::move(prelude)}, opFn](GenCtx *ctx){
				return pre(ctx, opFn);
			};
		}

		default:{
			return compError(69, "only literals and binary operator values currently implemented");
		}
	}
}

LambdaGen ivmCompileIRBranch(CompileCtx *ctx, IvmIRBranchPtr branch){
	auto mapRes = ctx->genMap.find(&branch->base);
	if(mapRes != end(ctx->genMap)) return [f{&mapRes->second}](auto ... args){ return (*f)(args...); };

	auto cond = branch->cond;
	auto trueBlock = branch->trueBlock;
	auto falseBlock = branch->falseBlock;

	static auto error = +[](int code, std::string_view msg) -> LambdaGen{
		return [code, msg](auto...){
			return retError(code, msg);
		};
	};

	if(cond->type != &boolType){
		return error(1, "condition to branch must evaluate to boolean");
	}

	if(cond->nodeType != IVM_IR_BINARYOP){
		return error(1, "condition must be a logical/boolean binary operator");
	}
	
	auto binop = reinterpret_cast<IvmIRBinaryOpPtr>(cond);

	auto emitTrueJmp = ivm_jlt;

	switch(binop->op){
		case IVM_IRBINARYOP_LT: emitTrueJmp = ivm_jlt; break;
		case IVM_IRBINARYOP_GT: emitTrueJmp = ivm_jgt; break;
		case IVM_IRBINARYOP_LE: emitTrueJmp = ivm_jle; break;
		case IVM_IRBINARYOP_GE: emitTrueJmp = ivm_jge; break;
		case IVM_IRBINARYOP_EQ: emitTrueJmp = ivm_jeq; break;
		case IVM_IRBINARYOP_NE: emitTrueJmp = ivm_jne; break;
		default:
			return error(1, "binary op for condition must be logical/boolean");
	}

	// to fix infinite recursion we pre-fill the map spot
	ctx->genMap[&branch->base] = [](auto...){
		std::fprintf(stderr, "THIS IS A PLACEHOLDER FUNCTION AND SHOULD NOT BE CALLED\n");
		return 420;
	};

	auto lhsGen = ivmCompileIRValue(ctx, binop->lhs);
	auto rhsGen = ivmCompileIRValue(ctx, binop->rhs);
	auto trueGen = ivmCompileIRBlock(ctx, trueBlock);
	auto falseGen = ivmCompileIRBlock(ctx, falseBlock);

	auto gen = [
			branch,
			binop, trueBlock, falseBlock,
			emitTrueJmp,
			lhsGen{std::move(lhsGen)},
			rhsGen{std::move(rhsGen)},
			trueGen{std::move(trueGen)},
			falseGen{std::move(falseGen)}
		]
		(GenCtx *ctx) -> int{
			auto res = ctx->instrOffsets.find(&branch->base);
			if(res != end(ctx->instrOffsets)){
				//auto jmpInstr = ivm_jump(trueIdx->second); // TODO: simple jump is frought with danger
				//ivmBufferWrite(ctx->buffer, sizeof(jmpInstr), &jmpInstr);

				return 0;
			}

			static const MemoryHandle
				scratch0 = { .type = MemoryHandle::register_, .idx = 1 },
				scratch1 = { .type = MemoryHandle::register_, .idx = 2 };

			auto oldMem = ctx->mem;

			ctx->mem = &scratch0;
			auto lhsRes = lhsGen(ctx);
			if(lhsRes != 0) return lhsRes;

			ctx->mem = &scratch1;
			auto rhsRes = rhsGen(ctx);
			if(rhsRes != 0) return rhsRes;

			ctx->mem = oldMem;

			auto branchOff = ivmBufferLength(ctx->buffer);

			auto trueJumpInstr = emitTrueJmp(0, IVMREG(scratch0.idx), IVMREG(scratch1.idx));
			auto trueJumpOff = ivmBufferLength(ctx->buffer);
			ivmBufferWrite(ctx->buffer, sizeof(trueJumpInstr), &trueJumpInstr);

			auto falseJumpInstr = ivm_jump(0);
			auto falseJumpOff = ivmBufferLength(ctx->buffer);
			ivmBufferWrite(ctx->buffer, sizeof(falseJumpInstr), &falseJumpInstr);

			auto trueRes = trueGen(ctx);
			if(trueRes != 0) return trueRes;

			auto trueIdx = ctx->blockIndices.find(trueBlock);
			if(trueIdx == end(ctx->blockIndices)){
				return retError(69, "could not create block for branch");
			}

			auto falseRes = falseGen(ctx);
			if(falseRes != 0) return falseRes;

			auto falseIdx = ctx->blockIndices.find(falseBlock);
			if(falseIdx == end(ctx->blockIndices)){
				return retError(69, "could not create block for branch");
			}

			auto trueJumpPtr = reinterpret_cast<IvmInstr*>(reinterpret_cast<char*>(ivmBufferPtr(ctx->buffer)) + trueJumpOff);
			auto falseJumpPtr = reinterpret_cast<IvmInstr*>(reinterpret_cast<char*>(ivmBufferPtr(ctx->buffer)) + falseJumpOff);

			trueJumpPtr->op0 = IVMIMM(trueIdx->second).data;
			falseJumpPtr->op0 = IVMIMM(falseIdx->second).data;

			ctx->instrOffsets[&branch->base] = branchOff;

			return 0;
		};

	ctx->genMap[&branch->base] = std::move(gen);

	return [f{&ctx->genMap[&branch->base]}](auto ... args){ return (*f)(args...); };
}

LambdaGen ivmCompileIRBlock(CompileCtx *ctx, IvmIRBlockPtr block){
	auto mapRes = ctx->genMap.find(&block->base);
	if(mapRes != end(ctx->genMap)) return [f{&mapRes->second}](auto ... xs){ return (*f)(xs...); };

	std::vector<LambdaGen> innerGen;

	static auto result = +[](int i) -> LambdaGen{ return [i](auto...){ return i; }; };

	static auto error = +[](int code, const char *msg) -> LambdaGen{
		return [code, msg](auto...){
			std::fprintf(stderr, "Error: %s\n", msg);
			return code;
		};
	};

	for(size_t i = 0; i < block->len; i++){
		auto node = block->body[i];

		switch(node->nodeType){
			case IVM_IR_BINARYOP:
			case IVM_IR_LITERAL:{
				std::fprintf(stderr, "Warning: unused rvalue\n");
				break;
			}

			case IVM_IR_FN:
			case IVM_IR_LAMBDA:{
				if(ctx->parent){
					return error(69, "nested lambdas currently unimplemented");
				}

				auto fn = reinterpret_cast<IvmIRLambdaPtr>(node);

				innerGen.emplace_back(ivmCompileIRLambda(ctx, fn));

				break;
			}

			case IVM_IR_BRANCH:{
				auto branch = reinterpret_cast<IvmIRBranchPtr>(node);
				innerGen.emplace_back(ivmCompileIRBranch(ctx, branch));
				break;
			}

			case IVM_IR_LOCAL:{
				auto stack = reinterpret_cast<IvmIRLocalMemPtr>(node);

				innerGen.emplace_back([stack](GenCtx *ctx){
					auto memRes = ctx->memMap.find(stack);
					if(memRes != end(ctx->memMap)) return 0; // already allocated

					auto regIdx = allocRegister(ctx);
					if(regIdx == SIZE_MAX) return 69;

					ctx->memMap[stack] = MemoryHandle{
						.type = MemoryHandle::register_,
						.idx = regIdx
					};

					return 0;
				});

				break;
			}

			case IVM_IR_STORE:{
				auto store = reinterpret_cast<IvmIRStorePtr>(node);

				if(store->value->type != store->mem->base.type){
					return error(1, "store instruction has type mismatch");
				}

				auto valueGen = ivmCompileIRValue(ctx, store->value);

				innerGen.emplace_back([store, valueGen{std::move(valueGen)}](GenCtx *ctx){
					auto memRes = ctx->memMap.find(store->mem);
					if(memRes == end(ctx->memMap)){
						return retError(1, "could not infer memory location for store instruction");
					}

					auto oldMem = ctx->mem;
					ctx->mem = &memRes->second;

					auto res = valueGen(ctx);
					if(res != 0) return res;

					ctx->mem = oldMem;

					return 0;
				});

				break;
			}

			case IVM_IR_BINDING:{
				auto binding = reinterpret_cast<IvmIRBindingPtr>(node);
				auto rhs = binding->value;

				// TODO: implement lambda binding

				return error(69, "bindings currently unimplemented");
			}

			case IVM_IR_RETURN:{
				auto ret = reinterpret_cast<const IvmIRReturn*>(node);

				if(!ctx->lam){
					return error(5, "return instruction may only exist within a lambda expression");
				}
				else if(ctx->lam->resultTy != ret->base.type){
					return error(5, "returned value does not have same type as function result");
				}

				if(ret->value){
					auto numBits = ivmIRTypeNumBits(ret->value->type);
					if(numBits > 64){ // TODO: make arch-agnostic
						return error(69, "only return values that fit in 64 bits (8 bytes) are currently implemented");
					}

					auto retResult = [](IvmOperand operand){
						return [operand](GenCtx *ctx) -> int{
							const auto ret = ivm_ret(operand);
							if(!ivmBufferWrite(ctx->buffer, sizeof(ret), &ret)){
								std::fprintf(stderr, "Error: error in ivmBufferWrite\n");
								return 420;
							}
							return 0;
						};
					};

					switch(ret->value->nodeType){
						case IVM_IR_LITERAL:{
							auto lit = reinterpret_cast<const IvmIRLiteral*>(ret->value);

							if(dynamic_cast<IvmIRNatType>(lit->base.type)){
								switch(numBits){
									case 8:
										innerGen.emplace_back(retResult(IVMIMM(lit->value.n8)));
										break;

									case 16:
										innerGen.emplace_back(retResult(IVMIMM(lit->value.n16)));
										break;

									case 32:
										innerGen.emplace_back(retResult(IVMIMM(lit->value.n32)));
										break;

									case 64:
										innerGen.emplace_back(retResult(IVMIMM(lit->value.n64)));
										break;

									default:
										return error(69, "only 8, 16, 32 and 64 bit naturals implemented");
								}
							}
							else if(dynamic_cast<IvmIRIntType>(lit->base.type)){
								switch(numBits){
									case 8:
										innerGen.emplace_back(retResult(IVMIMM(lit->value.i8)));
										break;

									case 16:
										innerGen.emplace_back(retResult(IVMIMM(lit->value.i16)));
										break;

									case 32:
										innerGen.emplace_back(retResult(IVMIMM(lit->value.i32)));
										break;

									case 64:
										innerGen.emplace_back(retResult(IVMIMM(lit->value.i64)));
										break;

									default:
										return error(69, "only 8, 16, 32 and 64 bit integers implemented");
								}
							}
							else{
								return error(69, "only integral and natural result types currently implemented");
							}

							break;
						}

						case IVM_IR_PARAM:
						case IVM_IR_LOCAL:{
							auto stack = reinterpret_cast<IvmIRLocalMemPtr>(ret->value);

							innerGen.emplace_back([stack](GenCtx *ctx){
								IvmOperand src;

								auto memRes = ctx->memMap.find(stack);
								if(memRes != end(ctx->memMap)){
									memToOp(&memRes->second, &src);
								}
								else{
									return retError(7, "couldn't infer memory location in return expression");
								}

								const auto ret = ivm_ret(src);
								if(!ivmBufferWrite(ctx->buffer, sizeof(ret), &ret)){
									return retError(420, "error in ivmBufferWrite");
								}

								return 0;
							});

							break;
						}

						default:
							return error(69, "only literal return values currently supported");
					}
				}
				else{
					innerGen.emplace_back([](GenCtx *ctx) -> int{
						auto ret = ivm_ret(IVMVOID());
						if(!ivmBufferWrite(ctx->buffer, sizeof(ret), &ret)){
							std::fprintf(stderr, "Error: error in ivmBufferWrite\n");
							return 420;
						}

						return 0;
					});
				}

				break;
			}

			default:
				return error(69, "IR compilation mostly unimplemented");
		}
	}

	return [block, inner{std::move(innerGen)}](GenCtx *ctx) -> int{
		auto blockRes = ctx->blockIndices.find(block);
		if(blockRes != end(ctx->blockIndices)){
			return 0;
		}

		auto blockIdx = ctx->labelCounter++;

		auto labelInstr = ivm_label(blockIdx);

		if(!ivmBufferWrite(ctx->buffer, sizeof(labelInstr), &labelInstr)){
			std::fprintf(stderr, "Error: error in ivmBufferWrite\n");
			return 420;
		}

		ctx->blockIndices[block] = blockIdx;

		for(auto &&gen : inner){
			auto res = gen(ctx);
			if(res != 0){
				return res;
			}
		}

		return 0;
	};
}

LambdaGen ivmCompileIRLambda(CompileCtx *ctx, IvmIRLambdaPtr lam, bool fnBody){
	assert(lam && lam->body); // TODO: do actual error return

	auto mapRes = ctx->genMap.find(&lam->base);
	if(mapRes != end(ctx->genMap)) return [f{&mapRes->second}](auto ... xs){ return (*f)(xs...); };

	ctx->lam = lam;

	if(!fnBody && lam->base.nodeType != IVM_IR_FN){
		return compError(69, "only lambdas as function bodies currently supported");
	}

	for(size_t i = 0; i < lam->numParams; i++){
		ctx->paramMap[lam->params + i] = IVMPARAM(i);
	}

	return [lam, inner{ivmCompileIRBlock(ctx, ctx->lam->body)}](GenCtx *ctx){
		auto res = ctx->lambdaOffsets.find(lam);
		if(res != end(ctx->lambdaOffsets)){
			return 0; // lambda already compiled
		}

		// jump label for inline lambdas
		auto jumpLabelIdx = ctx->labelCounter++;

		auto jumpInstr = ivm_jump(jumpLabelIdx);

		if(!ivmBufferWrite(ctx->buffer, sizeof(jumpInstr), &jumpInstr)){
			std::fprintf(stderr, "Error: error in ivmBufferWrite\n");
			return 420;
		}

		if(lam->base.nodeType == IVM_IR_FN){
			auto fn = reinterpret_cast<IvmIRFunctionPtr>(lam);
			auto fnInstr = ivm_fn(fn->nameLen, fn->name);
			if(!ivmBufferWrite(ctx->buffer, sizeof(fnInstr), &fnInstr)){
				std::fprintf(stderr, "Error: error in ivmBufferWrite\n");
				return 420;
			}
		}

		auto off = ivmBufferLength(ctx->buffer);

		auto oldLam = ctx->lam;
		ctx->lam = lam;

		for(size_t i = 0; i < lam->numParams; i++){
			ctx->memMap[&lam->params[i].base] = MemoryHandle{
				.type = MemoryHandle::param,
				.idx = i
			};
		}

		{
			auto res = inner(ctx);
			if(res != 0) return res;
		}

		ctx->lam = oldLam;

		ctx->lambdaOffsets[lam] = off;

		auto labelInstr = ivm_label(jumpLabelIdx);
		if(!ivmBufferWrite(ctx->buffer, sizeof(labelInstr), &labelInstr)){
			std::fprintf(stderr, "Error: error in ivmBufferWrite\n");
			return 420;
		}

		return 0;
	};
}

struct IvmCompileResult{
	IvmBuffer buf;
};

IvmCompileResultPtr ivmCompileIR(size_t len, const IvmIRNodePtr *nodes){
	auto globalBlock = ivmir_block(len, nodes);

	auto globalLam = ivmir_lambda(ivmIRTypeVoid(), 0, nullptr, &globalBlock);

	auto ctx = CompileCtx(nullptr);

	auto gen = ivmCompileIRLambda(&ctx, &globalLam, true);

	auto mem = std::malloc(sizeof(IvmCompileResult));
	if(!mem) return nullptr;

	auto ret = new(mem) IvmCompileResult;

	auto startLabel = ivm_label(0);

	ret->buf = ivmCreatePageBuffer();

	ivmBufferWrite(ret->buf, sizeof(startLabel), &startLabel);

	GenCtx genCtx;
	genCtx.buffer = ret->buf;
	genCtx.labelCounter = 1;

	auto res = gen(&genCtx);

	for(auto &&fn : genCtx.fillFns){
		fn(&genCtx);
	}

	if(res != 0){
		ivmDestroyCompileResult(ret);
		return nullptr;
	}
	else{
		return ret;
	}
}

void ivmDestroyCompileResult(IvmCompileResultPtr res){
	if(res->buf) ivmDestroyBuffer(res->buf);
	std::destroy_at(res);
	std::free(res);
}

size_t ivmCompileResultNumInstrs(IvmCompileResultPtr res){
	return ivmBufferLength(res->buf) / sizeof(IvmInstr);
}

const IvmInstr *ivmCompileResultInstrs(IvmCompileResultPtr res){
	return reinterpret_cast<const IvmInstr*>(ivmBufferConstPtr(res->buf));
}

struct IvmParseResult{
	std::vector<IvmIRNode*> nodes;
};

using ParseFn = int(IvmParseResultPtr, const sexi::Expr&);

int ivmParseExpr(IvmParseResultPtr ctx, const sexi::Expr &expr);

int ivmParseBlock(IvmParseResultPtr ctx, const sexi::Expr &block){
	assert(block.isList());

	for(auto &&inner : block){

	}

	return 69;
}

int ivmParseFn(IvmParseResultPtr ctx, const sexi::Expr &fn){
	if(fn.length() != 4){
		return retError(3, "'fn' expects 3 arguments (id, signature, body)");
	}

	auto id = fn[1];
	auto sig = fn[2];
	auto body = fn[3];

	assert(id.isId());
	assert(sig.isList());
	assert(body.isList());

	auto blockRes = ivmParseBlock(ctx, body);
	if(blockRes != 0) return blockRes;

	return retError(69, "'fn' op unimplemented");
}

int ivmParseAlloc(IvmParseResultPtr ctx, const sexi::Expr &set){
	if(set.length() != 3){
		return retError(3, "'alloc' expects 2 arguments (id, type)");
	}
	return retError(69, "'alloc' op unimplemented");
}

int ivmParseStore(IvmParseResultPtr ctx, const sexi::Expr &store){
	if(store.length() != 3){
		return retError(3, "'store' expects 2 arguments (location, value)");
	}
	return retError(69, "'store' op unimplemented");
}

int ivmParseDef(IvmParseResultPtr ctx, const sexi::Expr &def){
	if(def.length() != 3){
		return retError(3, "'def' expects 2 arguments (id, body)");
	}
	return retError(69, "'def' op unimplemented");
}

int ivmParseBranch(IvmParseResultPtr ctx, const sexi::Expr &def){
	if(def.length() != 4){
		return retError(3, "'branch' expects 3 arguments (cond, true block, false block)");
	}
	return retError(69, "'branch' op unimplemented");
}

int ivmParseRootExpr(IvmParseResultPtr ctx, const sexi::Expr &expr){
	static std::map<std::string_view, ParseFn*> opMap = {
		{"fn", ivmParseFn}
	};

	if(!expr.isList()) return 1;

	auto head = expr[0];

	if(!head.isId()) return 2;

	auto opName = head.toStr();

	auto opRes = opMap.find(opName);
	if(opRes == end(opMap)){
		auto str = "unrecognized op '" + std::string(opName) + "' at top level";
		return retError(3, str);
	}

	return opRes->second(ctx, expr);
}

int ivmParseExpr(IvmParseResultPtr ctx, const sexi::Expr &expr){
	static auto parseFnErr = [](IvmParseResultPtr, const sexi::Expr&){
		return retError(1, "functions must be declared at top level");
	};

	static std::map<std::string_view, ParseFn*> opMap = {
		{"alloc", ivmParseAlloc},
		{"branch", ivmParseBranch},
		{"def", ivmParseDef},
		{"fn", parseFnErr},
		{"store", ivmParseStore}
	};

	if(!expr.isList()) return 1;

	auto head = expr[0];

	if(!head.isId()) return 2;

	auto opName = head.toStr();

	auto opRes = opMap.find(opName);
	if(opRes == end(opMap)){
		auto str = "unrecognized op '" + std::string(opName) + "'";
		return retError(3, str);
	}

	return opRes->second(ctx, expr);
}

IvmParseResultPtr ivmParseIR(size_t len, const char *str){
	if(len == 0 || !str) return nullptr;

	auto parsed = sexi::parse(std::string_view(str, len));
	if(parsed.hasError()){
		std::fprintf(stderr, "%.*s\n", int(parsed.error().size()), parsed.error().data());
		return nullptr;
	}

	auto mem = std::malloc(sizeof(IvmParseResult));
	if(!mem) return nullptr;

	auto ptr = new(mem) IvmParseResult;

	for(auto &&expr : parsed){
		auto res = ivmParseRootExpr(ptr, expr);
		if(res != 0) return nullptr;
	}

	return ptr;
}

void ivmDestroyParseResult(IvmParseResultPtr res){
	for(auto node : res->nodes){
		std::free(node);
	}
	std::destroy_at(res);
	std::free(res);
}

size_t ivmParseResultNumNodes(IvmParseResultPtr res){ return res->nodes.size(); }
const IvmIRNodePtr *ivmParseResultNodes(IvmParseResultPtr res){ return res->nodes.data(); }

template<typename ... Tags>
class SexiThang{
	public:
		SexiThang(){}

	private:
		
};


//
// Created by keithh on 11/10/20.
//

#include <cassert>
#include <cstring>
#include <cstdio>

#include <tuple>

#include "emit.hpp"

using namespace ivm::x64;

constexpr uint8_t rexPrefix = 0x48; // REX.W - Indicates 64 Bit Operand Size

#define REX_W_BIT (1u << 3u)
#define REX_R_BIT (1u << 2u)
#define REX_X_BIT (1u << 1u)
#define REX_B_BIT (1u)

// w: use 64-bit op0 sizes
// r: extend modrm reg field to r8-r15
// x: extend SIB.index field
// b: extend modrm rm or SIB base field
inline constexpr uint8_t encodeRexPrefix(bool w, bool r, bool x, bool b) noexcept{
	// 0b0100 == 0x4
	return (uint8_t(0x4u) << 4u) |
		   ((w & 0x1u) << 3u) |
		   ((r & 0x1u) << 2u) |
		   ((x & 0x1u) << 1u) |
		   (b & 0x1u);
}

enum class ModAddr{
		indirect = 0b00,
		direct = 0b11
};

inline uint8_t encodeModRegRM(ModAddr mod, uint8_t reg, uint8_t rm) noexcept{
	// TODO: remove these asserts
	assert(reg < IVM_X64REG_MODREG_COUNT);
	assert(rm < IVM_X64REG_MODREG_COUNT);
	return (((uint8_t)mod & 0b11u) << 6u) | ((reg & 0b111u) << 3u) | (rm & 0b111u);
}

std::tuple<bool, bool, uint8_t> calcModRegRm(
		uint8_t dst,
		uint8_t src = IVM_X64REG_RAX
){
	bool extendDst = dst >= IVM_X64REG_MODREG_COUNT;
	bool extendSrc = src >= IVM_X64REG_MODREG_COUNT;
	if(extendDst){
		dst -= IVM_X64REG_MODREG_COUNT;
		std::fprintf(stderr, "EXTENDED MOD RM\n");
	}
	if(extendSrc){
		src -= IVM_X64REG_MODREG_COUNT;
		std::fprintf(stderr, "EXTENDED MOD REG\n");
	}
	return std::make_tuple(extendDst, extendSrc, encodeModRegRM(ModAddr::direct, src, dst));
}

std::tuple<uint8_t, uint8_t> calcRexModRegRm(ModAddr addr, bool use64, uint8_t op0, uint8_t op1) noexcept{
	auto [extendRm, extendReg, mod] = calcModRegRm(op0, op1);
	auto prefix = encodeRexPrefix(use64, extendReg, false, extendRm);
	return std::make_tuple(prefix, mod);
}

void ivm::x64::emitPushReg(X64EmitCtx *ctx, bool use64, IvmX64Reg op0) noexcept{
	auto[extendRm, extendReg, mod] = calcModRegRm(op0, 0x6);

	if(extendRm || extendReg){
		auto prefix = encodeRexPrefix(use64, extendReg, false, extendRm);
		ivmBufferWrite8(ctx->buffer, prefix);
	} else if(use64){
		auto prefix = encodeRexPrefix(true, false, false, false);
		ivmBufferWrite8(ctx->buffer, prefix);
	}

	ivmBufferWrite8(ctx->buffer, 0xFF);
	ivmBufferWrite8(ctx->buffer, mod);
}

void ivm::x64::emitPushImm(X64EmitCtx *ctx, IvmImm imm) noexcept{
	switch(imm.type){
		case IVM_IMM_I8:
		case IVM_IMM_N8:{
			ivmBufferWrite8(ctx->buffer, 0x6A);
			ivmBufferWrite8(ctx->buffer, imm.n16);
			break;
		}
		case IVM_IMM_I16:
		case IVM_IMM_N16:{
			ivmBufferWrite8(ctx->buffer, 0x68);
			ivmBufferWrite8(ctx->buffer, imm.n16);
			break;
		}
		case IVM_IMM_I32:
		case IVM_IMM_N32:{
			auto prefix = encodeRexPrefix(true, false, false, false);
			ivmBufferWrite8(ctx->buffer, prefix);
			ivmBufferWrite8(ctx->buffer, 0x68);
			ivmBufferWrite32(ctx->buffer, imm.n32);
			break;
		}
		case IVM_IMM_I64:
		case IVM_IMM_N64:{
			// implement 64-bit push as two 32-bit pushs
			static uint32_t dwords[2];
			std::memcpy(dwords, &imm.n64, 8);
			for(int i = 0; i < 2; i++){
				auto prefix = encodeRexPrefix(true, false, false, false);
				ivmBufferWrite8(ctx->buffer, prefix);
				ivmBufferWrite8(ctx->buffer, 0x68);
				ivmBufferWrite32(ctx->buffer, dwords[i]);
			}
			break;
		}
		default:{
			assert(!"only 32-bit immediate values implemented");
			break;
		}
	}
}

void ivm::x64::emitPopReg(X64EmitCtx *ctx, PopSize size, IvmX64Reg op0) noexcept{
	auto[extendRm, extendReg, mod] = calcModRegRm(op0, 0x0);

	uint8_t prefix = 0;

	switch(size){
		case PopSize::_16:{
			prefix = (extendRm || extendReg) ? encodeRexPrefix(false, extendReg, false, extendRm) : prefix;
			break;
		}
		case PopSize::_32: // 32-bit operands can not be encoded in x64-64
		case PopSize::_64:{
			prefix = encodeRexPrefix(true, extendReg, false, extendRm);
			break;
		}
		default:
			break;
	}

	if(prefix) ivmBufferWrite8(ctx->buffer, prefix);
	ivmBufferWrite8(ctx->buffer, 0x8F);
	ivmBufferWrite8(ctx->buffer, mod);
}

int ivm::x64::emitCmpRegImm(X64EmitCtx *ctx, IvmX64Reg reg, IvmImm imm) noexcept{
	auto[extendRm, extendReg, mod] = calcModRegRm(reg, 0x7);

	auto prefix = encodeRexPrefix(true, extendReg, false, extendRm);

	ivmBufferWrite8(ctx->buffer, prefix);

	switch(imm.type){
		case IVM_IMM_I8:
		case IVM_IMM_N8:{
			ivmBufferWrite8(ctx->buffer, 0x80);
			ivmBufferWrite8(ctx->buffer, mod);
			ivmBufferWrite8(ctx->buffer, imm.n8);
			break;
		}
		case IVM_IMM_I16:
		case IVM_IMM_N16:
		case IVM_IMM_I32:
		case IVM_IMM_N32:{
			ivmBufferWrite8(ctx->buffer, 0x81);
			ivmBufferWrite8(ctx->buffer, mod);
			ivmBufferWrite32(ctx->buffer, imm.n32);
			break;
		}
		case IVM_IMM_I64:
		case IVM_IMM_N64:
		default:{
			std::fprintf(stderr, "Error: only 32-bit immediate values implemented\n");
			return 69;
		}
	}

	return 0;
}

int ivm::x64::emitCmpRegReg(X64EmitCtx *ctx, IvmX64Reg dst, IvmX64Reg src) noexcept{
	auto[extendReg, extendRm, mod] = calcModRegRm(dst, src);
	auto prefix = encodeRexPrefix(true, extendReg, false, extendRm);
	ivmBufferWrite8(ctx->buffer, prefix);
	ivmBufferWrite8(ctx->buffer, 0x39);
	ivmBufferWrite8(ctx->buffer, mod);
	return 0;
}

int ivm::x64::emitCmp(X64EmitCtx *ctx, IvmOperand dst, IvmOperand src) noexcept{
	uint8_t dstReg = 0;

	switch(dst.type){
		case IVM_OPTYPE_REG:{
			dstReg = dst.data.reg;
			break;
		}

		case IVM_OPTYPE_PARAM:{
			if(dst.data.param >= ctx->paramRegs.size()){
				std::fprintf(stderr, "Error: max %zu params currently supported\n", ctx->paramRegs.size());
				return 69;
			}

			dstReg = ctx->paramRegs[dst.data.param];
			break;
		}

		default:{
			std::fprintf(stderr, "Error: only register and param destination operands supported in CMP\n");
			return 1;
		}
	}

	switch(src.type){
		case IVM_OPTYPE_REG:
			return emitCmpRegReg(ctx, IvmX64Reg(dstReg), IvmX64Reg(src.data.reg));

		case IVM_OPTYPE_IMM:
			return emitCmpRegImm(ctx, IvmX64Reg(dstReg), src.data.imm);

		case IVM_OPTYPE_PARAM:{
			if(src.data.param >= ctx->paramRegs.size()){
				std::fprintf(stderr, "Error: max %zu params currently supported\n", ctx->paramRegs.size());
				return 69;
			}

			return emitCmpRegReg(ctx, IvmX64Reg(dstReg), IvmX64Reg(ctx->paramRegs[src.data.param]));
		}

		default:{
			std::fprintf(stderr, "Error: unexpected src operand in CMP\n");
			return 1;
		}
	}
}

// Jump near
void ivm::x64::emitJmpn32(X64EmitCtx *ctx, int32_t rel) noexcept{
	ivmBufferWrite8(ctx->buffer, 0xE9);
	ivmBufferWrite32(ctx->buffer, static_cast<uint32_t>(rel));
}

// Jump near if less than
void ivm::x64::emitJnlt32(X64EmitCtx *ctx, int32_t rel) noexcept{
	ivmBufferWrite8(ctx->buffer, 0x0F);
	ivmBufferWrite8(ctx->buffer, 0x8C);
	ivmBufferWrite32(ctx->buffer, static_cast<uint32_t>(rel));
}

// Jump near if below (less than unsigned)
void ivm::x64::emitJnb32(X64EmitCtx *ctx, int32_t rel) noexcept{
	ivmBufferWrite8(ctx->buffer, 0x0F);
	ivmBufferWrite8(ctx->buffer, 0x82);
	ivmBufferWrite32(ctx->buffer, static_cast<uint32_t>(rel));
}

// Jump near if greater than
void ivm::x64::emitJngt32(X64EmitCtx *ctx, int32_t rel) noexcept{
	ivmBufferWrite8(ctx->buffer, 0x0F);
	ivmBufferWrite8(ctx->buffer, 0x8F);
	ivmBufferWrite32(ctx->buffer, static_cast<uint32_t>(rel));
}

// Jump near if above (greater than unsigned)
void ivm::x64::emitJna32(X64EmitCtx *ctx, int32_t rel) noexcept{
	ivmBufferWrite8(ctx->buffer, 0x0F);
	ivmBufferWrite8(ctx->buffer, 0x87);
	ivmBufferWrite32(ctx->buffer, static_cast<uint32_t>(rel));
}

// Jump near if less than or equal
void ivm::x64::emitJnle32(X64EmitCtx *ctx, int32_t rel) noexcept{
	ivmBufferWrite8(ctx->buffer, 0x0F);
	ivmBufferWrite8(ctx->buffer, 0x8E);
	ivmBufferWrite32(ctx->buffer, static_cast<uint32_t>(rel));
}

// Jump near if below or equal (unsigned less or equal)
void ivm::x64::emitJnbe32(X64EmitCtx *ctx, int32_t rel) noexcept{
	ivmBufferWrite8(ctx->buffer, 0x0F);
	ivmBufferWrite8(ctx->buffer, 0x86);
	ivmBufferWrite32(ctx->buffer, static_cast<uint32_t>(rel));
}

// Jump near if greater than or equal
void ivm::x64::emitJnge32(X64EmitCtx *ctx, int32_t rel) noexcept{
	ivmBufferWrite8(ctx->buffer, 0x0F);
	ivmBufferWrite8(ctx->buffer, 0x8D);
	ivmBufferWrite32(ctx->buffer, static_cast<uint32_t>(rel));
}

// Jump near if above or equal (unsigned greater or equal)
void ivm::x64::emitJnae32(X64EmitCtx *ctx, int32_t rel) noexcept{
	ivmBufferWrite8(ctx->buffer, 0x0F);
	ivmBufferWrite8(ctx->buffer, 0x83);
	ivmBufferWrite32(ctx->buffer, static_cast<uint32_t>(rel));
}

// Jump near if equal
void ivm::x64::emitJneq32(X64EmitCtx *ctx, int32_t rel) noexcept{
	ivmBufferWrite8(ctx->buffer, 0x0F);
	ivmBufferWrite8(ctx->buffer, 0x84);
	ivmBufferWrite32(ctx->buffer, static_cast<uint32_t>(rel));
}

// Jump near if not equal
void ivm::x64::emitJnne32(X64EmitCtx *ctx, int32_t rel) noexcept{
	ivmBufferWrite8(ctx->buffer, 0x0F);
	ivmBufferWrite8(ctx->buffer, 0x85);
	ivmBufferWrite32(ctx->buffer, static_cast<uint32_t>(rel));
}

// Bitwise logical XOR
void ivm::x64::emitXorRegReg(X64EmitCtx *ctx, IvmX64Reg dst, IvmX64Reg src) noexcept{
	auto[extendRm, extendReg, mod] = calcModRegRm(dst, src);
	auto prefix = encodeRexPrefix(true, extendReg, false, extendRm);
	ivmBufferWrite8(ctx->buffer, prefix);
	ivmBufferWrite8(ctx->buffer, 0x33);
	ivmBufferWrite8(ctx->buffer, mod);
}

// Integer addition
int ivm::x64::emitAdd(X64EmitCtx *ctx, IvmOperand dst, IvmOperand src) noexcept{
	IvmX64Reg dstReg = IVM_X64REG_RAX;

	if(dst.type == IVM_OPTYPE_REG){
		dstReg = IvmX64Reg(dst.data.reg);
	} else if(dst.type == IVM_OPTYPE_PARAM){
		if(dst.data.param >= ctx->paramRegs.size()){
			std::fprintf(stderr, "Error: max %zu params currently supported\n", ctx->paramRegs.size());
			return 69;
		}

		dstReg = IvmX64Reg(ctx->paramRegs[dst.data.param]);
	} else{
		std::fprintf(stderr, "Error: only register and param destination operands are supported\n");
	}

	switch(src.type){
		case IVM_OPTYPE_IMM:{
			auto[extendRm, extendReg, mod] = calcModRegRm(dstReg, 0x0);
			uint8_t prefix = encodeRexPrefix(true, extendReg, false, extendRm);

			uint8_t code = 0x81;

			switch(src.data.imm.type){
				case IVM_IMM_N8:
				case IVM_IMM_I8:{
					code = 0x83;
					break;
				}

				case IVM_IMM_N64:
				case IVM_IMM_I64:{
					assert(!"only 32-bit immediate values implemented");
				}

				default:
					break;
			}

			ivmBufferWrite8(ctx->buffer, prefix);
			ivmBufferWrite8(ctx->buffer, code);

			switch(src.data.imm.type){
				case IVM_IMM_N8:
				case IVM_IMM_I8:{
					ivmBufferWrite8(ctx->buffer, src.data.imm.n8);
					break;
				}

				default:{
					ivmBufferWrite32(ctx->buffer, src.data.imm.n32);
					break;
				}
			}

			break;
		}

		case IVM_OPTYPE_REG:{
			auto[extendRm, extendReg, mod] = calcModRegRm(dstReg, src.data.reg);
			auto prefix = encodeRexPrefix(true, extendReg, false, extendRm);
			ivmBufferWrite8(ctx->buffer, prefix);
			ivmBufferWrite8(ctx->buffer, 0x01);
			ivmBufferWrite8(ctx->buffer, mod);
			break;
		}

		case IVM_OPTYPE_PARAM:{
			if(src.data.param >= ctx->paramRegs.size()){
				std::fprintf(stderr, "Error: max %zu params currently supported\n", ctx->paramRegs.size());
				return 69;
			}

			auto srcReg = IvmX64Reg(ctx->paramRegs[src.data.param]);

			auto[extendRm, extendReg, mod] = calcModRegRm(dstReg, srcReg);
			auto prefix = encodeRexPrefix(true, extendReg, false, extendRm);
			ivmBufferWrite8(ctx->buffer, prefix);
			ivmBufferWrite8(ctx->buffer, 0x01);
			ivmBufferWrite8(ctx->buffer, mod);
			break;
		}

		default:{
			std::fprintf(stderr, "Error: only immediate and register src operands allowed in ADD\n");
			return 1;
		}
	}

	return 0;
}

int ivm::x64::emitSub(X64EmitCtx *ctx, IvmOperand dst, IvmOperand src) noexcept{
	IvmX64Reg dstReg = IVM_X64REG_RAX;

	if(dst.type == IVM_OPTYPE_REG){
		dstReg = IvmX64Reg(dst.data.reg);
	} else if(dst.type == IVM_OPTYPE_PARAM){
		if(dst.data.param >= ctx->paramRegs.size()){
			std::fprintf(stderr, "Error: max %zu params currently supported\n", ctx->paramRegs.size());
			return 69;
		}

		dstReg = IvmX64Reg(ctx->paramRegs[dst.data.param]);
	} else{
		std::fprintf(stderr, "Error: only register and param destination operands are supported\n");
	}

	switch(src.type){
		case IVM_OPTYPE_IMM:{
			auto[extendRm, extendReg, mod] = calcModRegRm(dstReg, 0x5);
			auto prefix = encodeRexPrefix(true, extendReg, false, extendRm);
			ivmBufferWrite8(ctx->buffer, prefix);
			ivmBufferWrite8(ctx->buffer, 0x81);
			ivmBufferWrite8(ctx->buffer, mod);

			switch(src.data.imm.type){
				case IVM_IMM_N8:
				case IVM_IMM_N64:
				case IVM_IMM_I64:
					assert(!"only 32-bit immediate values implemented");

				default:{
					ivmBufferWrite32(ctx->buffer, src.data.imm.n32);
					break;
				}
			}

			break;
		}

		case IVM_OPTYPE_REG:{
			auto[extendRm, extendReg, mod] = calcModRegRm(dstReg, src.data.reg);
			auto prefix = encodeRexPrefix(true, extendReg, false, extendRm);
			ivmBufferWrite8(ctx->buffer, prefix);
			ivmBufferWrite8(ctx->buffer, 0x29);
			ivmBufferWrite8(ctx->buffer, mod);
			break;
		}

		case IVM_OPTYPE_PARAM:{
			if(src.data.param >= ctx->paramRegs.size()){
				std::fprintf(stderr, "Error: max %zu params currently supported\n", ctx->paramRegs.size());
				return 69;
			}

			auto srcReg = IvmX64Reg(ctx->paramRegs[src.data.param]);

			auto[extendRm, extendReg, mod] = calcModRegRm(dstReg, srcReg);
			auto prefix = encodeRexPrefix(true, extendReg, false, extendRm);
			ivmBufferWrite8(ctx->buffer, prefix);
			ivmBufferWrite8(ctx->buffer, 0x29);
			ivmBufferWrite8(ctx->buffer, mod);
			break;
		}

		default:
			assert(!"only immediate values and registers allowed in sub instructions");
	}

	return 0;
}

int ivm::x64::emitIMul(X64EmitCtx *ctx, IvmOperand dst, IvmOperand src) noexcept{
	IvmX64Reg dstReg = IVM_X64REG_RAX;

	if(dst.type == IVM_OPTYPE_REG){
		dstReg = IvmX64Reg(dst.data.reg);
	} else if(dst.type == IVM_OPTYPE_PARAM){
		if(dst.data.param >= ctx->paramRegs.size()){
			std::fprintf(stderr, "Error: max %zu params currently supported\n", ctx->paramRegs.size());
			return 69;
		}

		dstReg = IvmX64Reg(ctx->paramRegs[dst.data.param]);
	} else{
		std::fprintf(stderr, "Error: only register and param destination operands are supported\n");
	}

	switch(src.type){
		case IVM_OPTYPE_REG:{
			auto[extendRm, extendReg, mod] = calcModRegRm(src.data.reg, dstReg);
			auto prefix = encodeRexPrefix(true, extendReg, false, extendRm);
			ivmBufferWrite8(ctx->buffer, prefix);
			ivmBufferWrite8(ctx->buffer, 0x0F);
			ivmBufferWrite8(ctx->buffer, 0xAF);
			ivmBufferWrite8(ctx->buffer, mod);
			break;
		}

		case IVM_OPTYPE_IMM:{
			switch(src.data.imm.type){
				case IVM_IMM_N8:
				case IVM_IMM_I8:
				case IVM_IMM_N16:
				case IVM_IMM_I16:
				case IVM_IMM_N32:
				case IVM_IMM_I32:{
					auto[extendRm, extendReg, mod] = calcModRegRm(0x0, dstReg);
					auto prefix = encodeRexPrefix(true, extendReg, false, extendRm);
					ivmBufferWrite8(ctx->buffer, prefix);
					ivmBufferWrite8(ctx->buffer, 0x69);
					ivmBufferWrite8(ctx->buffer, mod);
					ivmBufferWrite32(ctx->buffer, src.data.imm.n32);
					break;
				}

				default:{
					std::fprintf(stderr, "Error: only 32-bit immediate values implemented\n");
					return 69;
				}
			}
		}

		case IVM_OPTYPE_PARAM:{
			if(src.data.param >= ctx->paramRegs.size()){
				std::fprintf(stderr, "Error: only %zu params currently implemented\n", ctx->paramRegs.size());
				return 69;
			}

			return emitIMul(ctx, dst, IVMREG(ctx->paramRegs[src.data.param]));
		}

		default:{
			std::fprintf(stderr, "Error: only register, param and immediate operands to IMUL supported\n");
			return 1;
		}
	}

	return 0;
}

void ivm::x64::emitIncReg(X64EmitCtx *ctx, IvmX64Reg dst) noexcept{
	auto[extendRm, extendReg, mod] = calcModRegRm(dst, 0x0);
	auto prefix = encodeRexPrefix(true, extendReg, false, extendRm);
	ivmBufferWrite8(ctx->buffer, prefix);
	ivmBufferWrite8(ctx->buffer, 0xFF);
	ivmBufferWrite8(ctx->buffer, mod);
}

int ivm::x64::emitDecReg(X64EmitCtx *ctx, IvmX64Reg dst) noexcept{
	auto[extendRm, extendReg, mod] = calcModRegRm(dst, 0x1);
	auto prefix = encodeRexPrefix(true, extendReg, false, extendRm);
#define TRYWRITE8(x) { if(!ivmBufferWrite8(ctx->buffer, x)){ std::fprintf(stderr, "Error: error in ivmBufferWrite\n"); return 420; } }
	TRYWRITE8(prefix);
	TRYWRITE8(0xFF);
	TRYWRITE8(mod);
#undef TRYWRITE8
	return 0;
}

int ivm::x64::emitDecParam(X64EmitCtx *ctx, uint8_t idx) noexcept{
	if(idx >= ctx->paramRegs.size()){
		std::fprintf(stderr, "Error: max %zu params currently implemented\n", ctx->paramRegs.size());
		return 69;
	}

	auto dst = ctx->paramRegs[idx];

	return emitDecReg(ctx, IvmX64Reg(dst));
}

int ivm::x64::emitDec(X64EmitCtx *ctx, IvmOperand op0) noexcept{
	switch(op0.type){
		case IVM_OPTYPE_REG:
			return emitDecReg(ctx, IvmX64Reg(op0.data.reg));
		case IVM_OPTYPE_PARAM:
			return emitDecParam(ctx, op0.data.param);

		default:{
			std::fprintf(stderr, "Error: only register and param operands supported in DEC\n");
			return 1;
		}
	}
}

// Set register
void ivm::x64::emitMovRegImm32(X64EmitCtx *ctx, IvmX64Reg dst, uint32_t src) noexcept{
	auto[extendRm, extendReg, mod] = calcModRegRm(dst, 0x0);
	auto prefix = encodeRexPrefix(true, extendReg, false, extendRm);
	ivmBufferWrite8(ctx->buffer, prefix);
	ivmBufferWrite8(ctx->buffer, 0xC7);
	ivmBufferWrite8(ctx->buffer, mod);
	ivmBufferWrite32(ctx->buffer, src);
}

void ivm::x64::emitMovRegReg(X64EmitCtx *ctx, IvmX64Reg dst, IvmX64Reg src) noexcept{
	auto[extendRm, extendReg, mod] = calcModRegRm(dst, src);
	auto prefix = encodeRexPrefix(true, extendReg, false, extendRm);
	ivmBufferWrite8(ctx->buffer, prefix);
	ivmBufferWrite8(ctx->buffer, 0x89);
	ivmBufferWrite8(ctx->buffer, mod);
}

void ivm::x64::emitRet(X64EmitCtx *ctx) noexcept{
	ivmBufferWrite8(ctx->buffer, 0xC3);
}

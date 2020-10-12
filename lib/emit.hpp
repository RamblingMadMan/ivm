//
// Created by keithh on 11/10/20.
//

#ifndef IVM_EMIT_HPP
#define IVM_EMIT_HPP 1

#include <vector>

#include "ivm/Buffer.h"
#include "ivm/Instr.h"

namespace ivm::x64{
	enum IvmX64Reg: uint8_t{
		IVM_X64REG_RAX = 0,
		IVM_X64REG_RCX,
		IVM_X64REG_RDX,
		IVM_X64REG_RBX,
		IVM_X64REG_RSP,
		IVM_X64REG_RBP,
		IVM_X64REG_RSI,
		IVM_X64REG_RDI,

		IVM_X64REG_R8,
		IVM_X64REG_R9,
		IVM_X64REG_R10,
		IVM_X64REG_R11,
		IVM_X64REG_R12,
		IVM_X64REG_R13,
		IVM_X64REG_R14,
		IVM_X64REG_R15,


		IVM_X64REG_COUNT,
		IVM_X64REG_MODREG_COUNT = IVM_X64REG_R8
	};

	struct X64EmitCtx{
		IvmBuffer buffer = nullptr;
		std::vector<IvmX64Reg> paramRegs = {};
	};

	void emitPushReg(X64EmitCtx *ctx, bool use64, IvmX64Reg op0) noexcept;

	void emitPushImm(X64EmitCtx *ctx, IvmImm imm) noexcept;

	enum class PopSize{
		_16 = 4, _32 = 5, _64 = 6
	};

	void emitPopReg(X64EmitCtx *ctx, PopSize size, IvmX64Reg op0) noexcept;

/*
inline void emitPushImm32(IvmBuffer buf, IvmX64Reg reg, uint32_t imm) noexcept{
	ivmBufferWrite8(buf, 0x68);
}
*/

	int emitCmpRegImm(X64EmitCtx *ctx, IvmX64Reg reg, IvmImm imm) noexcept;

	int emitCmpRegReg(X64EmitCtx *ctx, IvmX64Reg dst, IvmX64Reg src) noexcept;

	int emitCmp(X64EmitCtx *ctx, IvmOperand dst, IvmOperand src) noexcept;

// Jump near
	void emitJmpn32(X64EmitCtx *ctx, int32_t rel) noexcept;

// Jump near if less than
	void emitJnlt32(X64EmitCtx *ctx, int32_t rel) noexcept;

// Jump near if below (less than unsigned)
	void emitJnb32(X64EmitCtx *ctx, int32_t rel) noexcept;

// Jump near if greater than
	void emitJngt32(X64EmitCtx *ctx, int32_t rel) noexcept;

// Jump near if above (greater than unsigned)
	void emitJna32(X64EmitCtx *ctx, int32_t rel) noexcept;

// Jump near if less than or equal
	void emitJnle32(X64EmitCtx *ctx, int32_t rel) noexcept;

// Jump near if below or equal (unsigned less or equal)
	void emitJnbe32(X64EmitCtx *ctx, int32_t rel) noexcept;

// Jump near if greater than or equal
	void emitJnge32(X64EmitCtx *ctx, int32_t rel) noexcept;

// Jump near if above or equal (unsigned greater or equal)
	void emitJnae32(X64EmitCtx *ctx, int32_t rel) noexcept;

// Jump near if equal
	void emitJneq32(X64EmitCtx *ctx, int32_t rel) noexcept;

// Jump near if not equal
	void emitJnne32(X64EmitCtx *ctx, int32_t rel) noexcept;

// Bitwise logical XOR
	void emitXorRegReg(X64EmitCtx *ctx, IvmX64Reg dst, IvmX64Reg src) noexcept;

// Integer addition
	int emitAdd(X64EmitCtx *ctx, IvmOperand dst, IvmOperand src) noexcept;

	int emitSub(X64EmitCtx *ctx, IvmOperand dst, IvmOperand src) noexcept;

	int emitIMul(X64EmitCtx *ctx, IvmOperand dst, IvmOperand src) noexcept;

	void emitIncReg(X64EmitCtx *ctx, IvmX64Reg dst) noexcept;

	int emitDecReg(X64EmitCtx *ctx, IvmX64Reg dst) noexcept;

	int emitDecParam(X64EmitCtx *ctx, uint8_t idx) noexcept;

	int emitDec(X64EmitCtx *ctx, IvmOperand op0) noexcept;

// Set register
	void emitMovRegImm32(X64EmitCtx *ctx, IvmX64Reg dst, uint32_t src) noexcept;

	void emitMovRegReg(X64EmitCtx *ctx, IvmX64Reg dst, IvmX64Reg src) noexcept;

	void emitRet(X64EmitCtx *ctx) noexcept;
}

#endif // !IVM_EMIT_HPP

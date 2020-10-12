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

#ifndef IVM_INSTR_H
#define IVM_INSTR_H 1

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>
#include <stdint.h>

/**
 * @defgroup Instrs Machine instructions
 * @{
 */

typedef enum{
	IVM_NOP = 0x0, // null op

	IVM_SET = 0x01,

	// 0b001000 == start of integer arithmetic
	IVM_IADD = 0x8,
	IVM_UADD = 0x9, // alias of IADD
	IVM_ISUB = 0xa,
	IVM_USUB = 0xb, // alias of ISUB
	IVM_IMUL = 0xc,
	IVM_UMUL = 0xd,
	IVM_IDIV = 0xe,
	IVM_UDIV = 0xf,

	// 0b011000 == start of register in-place ops
	IVM_INC = 0x18,
	IVM_DEC = 0x19,

	// 0b100000 == start of label/jump/cmp operations
	IVM_LABEL = 0x20,

	IVM_JMP = 0x21,

	IVM_JLT = 0x22,
	IVM_JLTN = 0x23,
	IVM_JGT = 0x24,
	IVM_JGTN = 0x25,
	IVM_JLE = 0x26,
	IVM_JLEN = 0x27,
	IVM_JGE = 0x28,
	IVM_JGEN = 0x29,
	IVM_JEQ = 0x2a,
	IVM_JNE = 0x2b,

	IVM_LT = 0x2c,
	IVM_LTN = 0x2d,
	IVM_GT = 0x2e,
	IVM_GTN = 0x2f,
	IVM_LE = 0x30,
	IVM_LEN = 0x31,
	IVM_GE = 0x32,
	IVM_GEN = 0x33,
	IVM_EQ = 0x34,
	IVM_NE = 0x35,

	// 0b111000 == start of fn ops
	IVM_FN = 0x38,

	// 0x111111 == return instruction
	IVM_RET = 0x3F,

	IVM_INSTRTYPE_MASK = 0x3F
} IvmInstrType;

typedef enum {
	IVM_OP_NUL = 0x0,
	IVM_OP_REG = 0x1,
	IVM_OP_IMM = 0x2,
	IVM_OP_PAR = 0x3
} IvmInstrOpFlags;

typedef enum {
	IVM_OPSIZE_8 = 0x0,
	IVM_OPSIZE_16 = 0x1,
	IVM_OPSIZE_32 = 0x2,
	IVM_OPSIZE_64 = 0x3,
} IvmOpSize;

typedef enum {
	IVM_OPTYPE_VOID = 0x0,
	IVM_OPTYPE_REG = 0x1,
	IVM_OPTYPE_PARAM = 0x2,
	IVM_OPTYPE_MEMORY = 0x3,
	IVM_OPTYPE_IMM = 0x4,

	IVM_OPTYPE_COUNT,
	IVM_OPTYPE_MASK = 0x3
} IvmOpType;

static inline uint16_t ivmMakeInstrFlags(IvmInstrType type, IvmOpType op0, IvmOpType op1, IvmOpType op2){
	uint8_t opFlags = 0x0;

	if(op0 == IVM_OPTYPE_IMM) opFlags |= 0x1;
	if(op1 == IVM_OPTYPE_IMM) opFlags |= 0x2;
	if(op2 == IVM_OPTYPE_IMM) opFlags |= 0x4;

	return (type << 10) | (opFlags << 6) | (op2 << 4) | (op1 << 2) | op0;
}

static inline IvmInstrType ivmInstrFlagsType(uint16_t flags){
	return (IvmInstrType)((flags >> 10) & IVM_INSTRTYPE_MASK);
}

static inline IvmOpType ivmInstrFlagsOpType(uint16_t flags, uint8_t idx){
	uint8_t opInfo = (flags >> (idx * 2)) & IVM_OPTYPE_MASK;
	uint8_t opFlags = flags >> 6;
	bool isImm = opFlags & (1 << idx);
	return isImm ? IVM_OPTYPE_IMM : (IvmOpType)opInfo;
}

static inline bool ivmInstrFlagsOpSigned(uint16_t flags){ return flags & (1 << 9); }
static inline bool ivmInstrFlagsOpModifier(uint16_t flags){ return flags & (1 << 9); }

static inline IvmOpSize ivmInstrFlagsOpSize(uint16_t flags, uint8_t idx){
	uint8_t opInfo = (flags >> (idx * 2)) & IVM_OPTYPE_MASK;
	uint8_t opFlags = flags >> 6;
	bool isImm = opFlags & (1 << idx);
	return isImm ? (IvmOpSize)opInfo : IVM_OPSIZE_8;
}

// 0b111111 == 0x3F
#define IVM_OP_MASK (0x3F)

#define IVM_INSTR_MASK (~IVM_OP_MASK)

typedef enum{
	IVM_IMM_N8, IVM_IMM_N16, IVM_IMM_N32, IVM_IMM_N64,
	IVM_IMM_I8, IVM_IMM_I16, IVM_IMM_I32, IVM_IMM_I64,

	IVM_IMMTYPE_COUNT
} IvmImmType;

inline bool ivmImmTypeIsSigned(IvmImmType ty){
	switch(ty){
		case IVM_IMM_I8:
		case IVM_IMM_I16:
		case IVM_IMM_I32:
		case IVM_IMM_I64:
			return true;

		default: return false;
	}
}

typedef struct{
	IvmImmType type;
	union{
		uint8_t n8;
		uint16_t n16;
		uint32_t n32;
		uint64_t n64;
		int8_t i8;
		int16_t i16;
		int32_t i32;
		int64_t i64;
	};
} IvmImm;

typedef union {
	uint8_t reg;
	uint8_t param;
	IvmImm imm;
} IvmOpData;

typedef struct {
	IvmOpType type;
	IvmOpData data;
} IvmOperand;

typedef struct {
	size_t nameLen;
	const char *name;
} IvmFnDef;

typedef struct{
	uint16_t flags;
	IvmOpData op0, op1, op2;
} IvmInstr;

IvmOperand ivm_immN8(uint8_t v);
IvmOperand ivm_immN16(uint16_t v);
IvmOperand ivm_immN32(uint32_t v);
IvmOperand ivm_immN64(uint64_t v);

IvmOperand ivm_immI8(int8_t v);
IvmOperand ivm_immI16(int16_t v);
IvmOperand ivm_immI32(int32_t v);
IvmOperand ivm_immI64(int64_t v);

#ifndef __cplusplus
#define IVMIMM(x) _Generic((x), \
	uint8_t: ivm_immN8,            \
	uint16_t: ivm_immN16,          \
	uint32_t: ivm_immN32,          \
	uint64_t: ivm_immN64,          \
	int8_t: ivm_immI8,             \
	int16_t: ivm_immI16,           \
	int32_t: ivm_immI32,           \
	int64_t: ivm_immI64)(x)
#else
}

inline IvmOperand IVMIMM(uint8_t x){ return ivm_immN8(x); }
inline IvmOperand IVMIMM(uint16_t x){ return ivm_immN16(x); }
inline IvmOperand IVMIMM(uint32_t x){ return ivm_immN32(x); }
inline IvmOperand IVMIMM(uint64_t x){ return ivm_immN64(x); }
inline IvmOperand IVMIMM(int8_t x){ return ivm_immI8(x); }
inline IvmOperand IVMIMM(int16_t x){ return ivm_immI16(x); }
inline IvmOperand IVMIMM(int32_t x){ return ivm_immI32(x); }
inline IvmOperand IVMIMM(int64_t x){ return ivm_immI64(x); }

extern "C" {
#endif

inline IvmOperand IVMREG(uint8_t r){
	IvmOperand ret;
	ret.type = IVM_OPTYPE_REG;
	ret.data.reg = r;
	return ret;
}

inline IvmOperand IVMPARAM(uint8_t idx){
	IvmOperand ret;
	ret.type = IVM_OPTYPE_PARAM;
	ret.data.param = idx;
	return ret;
}

#define IVMVOID() ((IvmOperand){ .type = IVM_OPTYPE_VOID })

#define IVM_NUM_REGS 8

IvmInstr ivm_nop(void);

IvmInstr ivm_set(IvmOperand dst, IvmOperand operand);

IvmInstr ivm_add(IvmOperand dst, IvmOperand operand);
IvmInstr ivm_sub(IvmOperand dst, IvmOperand operand);
IvmInstr ivm_mul(IvmOperand dst, IvmOperand operand);
IvmInstr ivm_div(IvmOperand dst, IvmOperand operand);

IvmInstr ivm_inc(IvmOperand dst);
IvmInstr ivm_dec(IvmOperand dst);

IvmInstr ivm_label(uint16_t idx);
IvmInstr ivm_fn(size_t nameLen, const char *name);

IvmInstr ivm_jump(uint16_t labelIdx);

IvmInstr ivm_lt(IvmOperand dst, IvmOperand src);
IvmInstr ivm_gt(IvmOperand dst, IvmOperand src);
IvmInstr ivm_le(IvmOperand dst, IvmOperand src);
IvmInstr ivm_ge(IvmOperand dst, IvmOperand src);
IvmInstr ivm_eq(IvmOperand dst, IvmOperand src);
IvmInstr ivm_ne(IvmOperand dst, IvmOperand src);

IvmInstr ivm_jlt(uint16_t labelIdx, IvmOperand dst, IvmOperand src);
IvmInstr ivm_jgt(uint16_t labelIdx, IvmOperand dst, IvmOperand src);
IvmInstr ivm_jle(uint16_t labelIdx, IvmOperand dst, IvmOperand src);
IvmInstr ivm_jge(uint16_t labelIdx, IvmOperand dst, IvmOperand src);
IvmInstr ivm_jeq(uint16_t labelIdx, IvmOperand dst, IvmOperand src);
IvmInstr ivm_jne(uint16_t labelIdx, IvmOperand dst, IvmOperand src);

IvmInstr ivm_ret(IvmOperand operand);

int ivmReadBytecode(size_t len, const uint8_t *bytes, void *data, void(*writeFn)(void*, const IvmInstr*));

/**
 * @}
 */

/**
 * @defgroup Emitter Machine code emission
 * @{
 */

typedef enum {
	IVM_ARCH_X86,
	IVM_ARCH_X64,
	IVM_ARCH_BYTECODE,

	IVM_ARCH_COUNT
} IvmArch;

typedef enum {
	IVM_PLATFORM_LINUX,
	IVM_PLATFORM_WIN32,
	IVM_PLATFORM_VM,

	IVM_PLATFORM_COUNT
} IvmPlatform;

typedef struct IvmEmitterT *IvmEmitter;

IvmEmitter ivmCreateEmitter(IvmArch arch, IvmPlatform platform);
void ivmDestroyEmitter(IvmEmitter emitter);

typedef void(*IvmCaller)(void *data, size_t codeLen, void *code);

void ivmEmitterCall(IvmEmitter emitter, size_t nameLen, const char *name, IvmCaller caller, void *data);

int ivmEmitInstr(IvmEmitter emitter, const IvmInstr *instr);
int ivmEmitInstrs(IvmEmitter emitter, size_t n, const IvmInstr *instrs);

void ivmWriteElf(IvmEmitter emitter, size_t pathLen, const char *path);

/**
 * @}
 */

#ifdef __cplusplus
}
#endif

#endif // !IVM_INSTR_H

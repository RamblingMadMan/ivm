/*
	This file is part of InfinityVM.

    InfinityVM is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    InfinityVM is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser Public License for more details.

    You should have received a copy of the GNU Lesser Public License
    along with InfinityVM.  If not, see <https://www.gnu.org/licenses/>.
*/

#ifndef IVM_INSTR_H
#define IVM_INSTR_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>
#include <stdint.h>

/**
 * @defgroup Instrs Machine instructions
 * @{
 */

typedef enum {
	IVM_SET = 0x01,

	// 0b01000000 == start of integer arithmetic
	IVM_IADD = 0x40,
	IVM_UADD = 0x40, // alias of IADD
	IVM_ISUB = 0x41,
	IVM_USUB = 0x41, // alias of ISUB
	IVM_IMUL = 0x42,
	IVM_UMUL = 0x43,
	IVM_IDIV = 0x44,
	IVM_UDIV = 0x45,

	// 0b01100000 == start of register in-place ops
	IVM_INC = 0x60,
	IVM_DEC = 0x61,

	// 0b10000000 == start of label/jump operations
	IVM_LABEL = 0x80,
	IVM_JMP = 0x81,
	IVM_JE = 0x82,
	IVM_JG = 0x83,

	// 0x11111111 == return instructions
	IVM_RET = 0xFF,
} IvmInstrType;

typedef enum {
	IVM_IMM_I8, IVM_IMM_I16, IVM_IMM_I32,

	IVM_IMMTYPE_COUNT
} ImmType;

typedef struct {
	ImmType type;
	union {
		uint8_t i8;
		uint16_t i16;
		uint32_t i32;
	};
} IvmImm;

typedef enum {
	IVM_OPTYPE_IMM, IVM_OPTYPE_REG,

	IVM_OPTYPE_COUNT
} IvmOpType;

typedef struct {
	union {
		uint8_t reg;
		IvmImm imm;
	};
} IvmUnaryOp;

typedef struct {
	uint8_t dst;
	union {
		IvmImm imm;
		uint8_t src;
	};
} IvmBinaryOp;

typedef struct {
	uint8_t reg;
	union {
		IvmImm imm;
		uint8_t src;
	};
	const char *name;
} IvmLabelOp;

typedef struct {
	IvmInstrType type;
	IvmOpType opType;
	union {
		IvmUnaryOp unary;
		IvmBinaryOp binary;
		IvmLabelOp label;
	};
} IvmInstr;

IvmInstr ivm_set(uint8_t dst, uint32_t val);
IvmInstr ivm_setr(uint8_t dst, uint8_t src);

IvmInstr ivm_mul(uint8_t dst, uint32_t val);
IvmInstr ivm_mulr(uint8_t dst, uint8_t src);

IvmInstr ivm_inc(uint8_t dst);
IvmInstr ivm_dec(uint8_t dst);

IvmInstr ivm_label(const char *name);
IvmInstr ivm_jump(const char *name);
IvmInstr ivm_jg(const char *name, uint8_t dst, uint32_t val);
IvmInstr ivm_je(const char *name, uint8_t dst, uint32_t val);

IvmInstr ivm_ret(uint32_t val);
IvmInstr ivm_retr(uint8_t src);

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
	IVM_PLATFORM_VM,

	IVM_PLATFORM_COUNT
} IvmPlatform;

typedef struct IvmEmitterT *IvmEmitter;

IvmEmitter ivmCreateEmitter(IvmArch arch, IvmPlatform platform);
void ivmDestroyEmitter(IvmEmitter emitter);

typedef void(*IvmCaller)(void *data, size_t codeLen, void *code);

void ivmEmitterCall(IvmEmitter emitter, IvmCaller caller, void *data);

int ivmEmitInstr(IvmEmitter emitter, const IvmInstr *instr);
int ivmEmitInstrs(IvmEmitter emitter, size_t n, const IvmInstr *instrs);

/**
 * @}
 */

#ifdef __cplusplus
}
#endif

#endif // !IVM_INSTR_H

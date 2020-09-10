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

#include <cassert>
#include <cstdio>

#include <numeric>
#include <functional>
#include <string>
#include <vector>
#include <map>

#include <unistd.h>

#include "ivm/Instr.h"
#include "ivm/Buffer.h"

IvmInstr makeUnaryOpReg(IvmInstrType type, uint8_t dst){
	IvmInstr ret;
	ret.type = type;
	ret.opType = IVM_OPTYPE_REG;
	ret.unary.reg = dst;
	return ret;
}

IvmInstr makeUnaryOpImm(IvmInstrType type, uint32_t val){
	IvmInstr ret;
	ret.type = type;
	ret.opType = IVM_OPTYPE_IMM;
	ret.unary.imm.type = IVM_IMM_I32;
	ret.unary.imm.i32 = val;
	return ret;
}

IvmInstr makeBinaryOpImm(IvmInstrType type, uint8_t dst, uint32_t val){
	IvmInstr ret;
	ret.type = type;
	ret.opType = IVM_OPTYPE_IMM;
	ret.binary.dst = dst;
	ret.binary.imm.type = IVM_IMM_I32;
	ret.binary.imm.i32 = val;
	return ret;
}

IvmInstr makeBinaryOpReg(IvmInstrType type, uint8_t dst, uint8_t src){
	IvmInstr ret;
	ret.type = type;
	ret.opType = IVM_OPTYPE_REG;
	ret.binary.dst = dst;
	ret.binary.src = src;
	return ret;
}

IvmInstr makeLabelOp(IvmInstrType type, const char *label){
	IvmInstr ret;
	ret.type = type;
	ret.opType = IVM_OPTYPE_IMM;
	ret.label.name = label;
	return ret;
}

IvmInstr makeJumpCmpOpImm(IvmInstrType type, const char *label, uint8_t dst, uint32_t val){
	IvmInstr ret;
	ret.type = type;
	ret.opType = IVM_OPTYPE_IMM;
	ret.label.name = label;
	ret.label.reg = dst;
	ret.label.imm.type = IVM_IMM_I32;
	ret.label.imm.i32 = val;
	return ret;
}

IvmInstr ivm_set(uint8_t dst, uint32_t val){ return makeBinaryOpImm(IVM_SET, dst, val); }
IvmInstr ivm_setr(uint8_t dst, uint8_t src){ return makeBinaryOpReg(IVM_SET, dst, src); }

IvmInstr ivm_mul(uint8_t dst, uint32_t val){ return makeBinaryOpImm(IVM_IMUL, dst, val); }
IvmInstr ivm_mulr(uint8_t dst, uint8_t src){ return makeBinaryOpReg(IVM_IMUL, dst, src); }

IvmInstr ivm_inc(uint8_t dst){ return makeUnaryOpReg(IVM_INC, dst); }
IvmInstr ivm_dec(uint8_t dst){ return makeUnaryOpReg(IVM_DEC, dst); }

IvmInstr ivm_label(const char *name){ return makeLabelOp(IVM_LABEL, name); }

IvmInstr ivm_jump(const char *name){ return makeLabelOp(IVM_JMP, name); }
IvmInstr ivm_jg(const char *name, uint8_t dst, uint32_t val){ return makeJumpCmpOpImm(IVM_JG, name, dst, val); }
IvmInstr ivm_je(const char *name, uint8_t dst, uint32_t val){ return makeJumpCmpOpImm(IVM_JE, name, dst, val); }

IvmInstr ivm_ret(uint32_t val){ return makeUnaryOpImm(IVM_RET, val); }
IvmInstr ivm_retr(uint8_t src){ return makeUnaryOpReg(IVM_RET, src); }

int ivmReadBytecode(size_t len, const uint8_t *bytes, void *data, void(*writeFn)(void*, const IvmInstr*)){
	(void)data;
	(void)writeFn;

	size_t idx = 0;

	while(idx < len){
		auto opcode = bytes[len++];
		switch(opcode){
			//case IVM_UADD:
			case IVM_IADD:{
				//ivm_add();
				//ivm_addr();
			}

			//case IVM_USUB:
			case IVM_ISUB:{

			}

			//case IVM_UMUL:
			case IVM_IMUL:{

			}

			//case IVM_UDIV:
			case IVM_IDIV:{

			}


			default:
				std::fprintf(stderr, "Error in ivmReadBytecode: unrecognized opcode 0x%x\n", opcode);
				return 1;
		}
	}

	return 0;
}

enum IvmX86Reg: uint8_t{
	IVM_X86REG_RAX = 0,
	IVM_X86REG_RCX,
	IVM_X86REG_RDX,
	IVM_X86REG_RBX,
	IVM_X86REG_RSP,
	IVM_X86REG_RBP,
	IVM_X86REG_RSI,
	IVM_X86REG_RDI,

	IVM_X86REG_R8,
	IVM_X86REG_R9,
	IVM_X86REG_R10,
	IVM_X86REG_R11,
	IVM_X86REG_R12,
	IVM_X86REG_R13,
	IVM_X86REG_R14,
	IVM_X86REG_R15,


	IVM_X86REG_COUNT,
	IVM_X86REG_MODREG_COUNT = IVM_X86REG_R8
};

constexpr uint8_t rexPrefix = 0x48; // REX.W - Indicates 64 Bit Operand Size

enum class ModAddr{
	indirect = 0b00,
	direct = 0b11
};

inline uint8_t encodeModRegRM(ModAddr mod, uint8_t reg, uint8_t rm) noexcept{
	assert(reg < IVM_X86REG_MODREG_COUNT);
	assert(rm < IVM_X86REG_MODREG_COUNT);
	return (((uint8_t)mod & 0b11) << 6) | ((reg & 0b111) << 3) | (rm & 0b111);
}

inline void emitCmpRegImm32(IvmBuffer buf, IvmX86Reg reg, uint32_t imm) noexcept{
	ivmBufferWrite8(buf, 0x81);
	ivmBufferWrite8(buf, encodeModRegRM(ModAddr::direct, 0x7, static_cast<uint8_t>(reg)));
	ivmBufferWrite32(buf, imm);
}

inline void emitJmpNear32(IvmBuffer buf, int32_t rel) noexcept{
	ivmBufferWrite8(buf, 0xE9);
	ivmBufferWrite32(buf, static_cast<uint32_t>(rel));
}

inline void emitJgNear32(IvmBuffer buf, int32_t rel) noexcept{
	ivmBufferWrite8(buf, 0x0F);
	ivmBufferWrite8(buf, 0x8F);
	ivmBufferWrite32(buf, static_cast<uint32_t>(rel));
}

inline void emitJeNear32(IvmBuffer buf, int32_t rel) noexcept{
	ivmBufferWrite8(buf, 0x0F);
	ivmBufferWrite8(buf, 0x84);
	ivmBufferWrite32(buf, static_cast<uint32_t>(rel));
}

// Bitwise logical XOR
inline void emitXorRegReg(IvmBuffer buf, IvmX86Reg dst, IvmX86Reg src) noexcept{
	ivmBufferWrite8(buf, rexPrefix);
	ivmBufferWrite8(buf, 0x33);
	ivmBufferWrite8(buf, encodeModRegRM(ModAddr::direct, dst, src));
}

// Integer addition
inline void emitAddRegImm32(IvmBuffer buf, IvmX86Reg dst, uint32_t src) noexcept{
	ivmBufferWrite8(buf, rexPrefix);
	ivmBufferWrite8(buf, 0x81);
	ivmBufferWrite8(buf, 0xc0 + static_cast<uint8_t>(dst));
	ivmBufferWrite32(buf, src);
}

// Signed multiply
inline void emitIMulRegImm32(IvmBuffer buf, IvmX86Reg dst, uint32_t src) noexcept{
	ivmBufferWrite8(buf, rexPrefix);
	ivmBufferWrite8(buf, 0x69);
	ivmBufferWrite8(buf, 0xc0 + static_cast<uint8_t>(dst));
	ivmBufferWrite32(buf, src);
}

inline void emitIMulRegReg(IvmBuffer buf, IvmX86Reg dst, IvmX86Reg src) noexcept{
	ivmBufferWrite8(buf, rexPrefix);
	ivmBufferWrite8(buf, 0x0F);
	ivmBufferWrite8(buf, 0xAF);
	ivmBufferWrite8(buf, encodeModRegRM(ModAddr::direct, dst, static_cast<uint8_t>(src)));
}

inline void emitIncReg(IvmBuffer buf, IvmX86Reg dst) noexcept{
	ivmBufferWrite8(buf, rexPrefix);
	ivmBufferWrite8(buf, 0xFF);
	ivmBufferWrite8(buf, encodeModRegRM(ModAddr::direct, static_cast<IvmX86Reg>(0x0), static_cast<uint8_t>(dst)));
}

inline void emitDecReg(IvmBuffer buf, IvmX86Reg dst) noexcept{
	ivmBufferWrite8(buf, rexPrefix);
	ivmBufferWrite8(buf, 0xFF);
	ivmBufferWrite8(buf, encodeModRegRM(ModAddr::direct, static_cast<IvmX86Reg>(0x1), static_cast<uint8_t>(dst)));
}

// Set register
inline void emitMovRegImm32(IvmBuffer buf, IvmX86Reg dst, uint32_t src) noexcept{
	ivmBufferWrite8(buf, rexPrefix);
	ivmBufferWrite8(buf, 0xc7);
	auto value = encodeModRegRM(ModAddr::direct, static_cast<IvmX86Reg>(0x0), static_cast<uint8_t>(dst));
	ivmBufferWrite8(buf, value);
	ivmBufferWrite32(buf, src);
}

inline void emitMovRegReg(IvmBuffer buf, IvmX86Reg dst, IvmX86Reg src) noexcept{
	ivmBufferWrite8(buf, rexPrefix);
	ivmBufferWrite8(buf, 0x8B);
	auto value = encodeModRegRM(ModAddr::direct, dst, static_cast<uint8_t>(src));
	ivmBufferWrite8(buf, value);
}

inline void emitRet(IvmBuffer buf) noexcept{
	ivmBufferWrite8(buf, 0xc3);
}

int emitInstrBytecode(IvmEmitter emitter, const IvmInstr *instr);
int emitInstrX64Linux(IvmEmitter emitter, const IvmInstr *instr);

typedef int(*IvmEmitFn)(IvmEmitter, const IvmInstr*);

struct IvmEmitterT{
	IvmEmitFn emitFn;
	IvmBuffer buffer;
	std::map<std::string_view, std::size_t, std::less<>> labelIndices;
};

static inline int error(std::string_view str, int code = 1){
	std::fprintf(stderr, "Error: %*.s\n", int(str.size()), str.data());
	return code;
}

int emitInstrBytecode(IvmEmitter emitter, const IvmInstr *instr){
	(void)emitter;
	(void)instr;
	return error("bytecode emission currently unimplemented", 1);
}

int emitInstrX64Linux(IvmEmitter emitter, const IvmInstr *instr){
	auto buf = emitter->buffer;

	std::vector<std::function<int()>> fillFns;

	switch(instr->type){
		case IVM_LABEL:{
			auto res = emitter->labelIndices.find(instr->label.name);
			if(res != end(emitter->labelIndices)){
				auto msg =  "label with name '" + std::string(instr->label.name) + "' already exists";
				return error(msg, 3);
			}

			emitter->labelIndices[instr->label.name] = ivmBufferLength(buf);
			break;
		}

		case IVM_JMP:{
			auto len = ivmBufferLength(buf);

			emitJmpNear32(buf, 0);

			auto endPtr = reinterpret_cast<char*>(ivmBufferPtr(buf)) + ivmBufferLength(buf);
			auto offPtr = reinterpret_cast<std::int32_t*>(endPtr) - 1;

			auto fill = [len, offPtr, instr, emitter]{
				auto res = emitter->labelIndices.find(instr->label.name);
				if(res == end(emitter->labelIndices)){
					auto msg = "could not find label '" + std::string(instr->label.name) + "', no look ahead currently implemented";
					return error(msg, 2);
				}

				auto offset = static_cast<std::int32_t>(res->second - len);

				auto jumpSize = 5; // 1 opcode byte + 4 rel bytes

				*offPtr = offset - jumpSize;

				return 0;
			};

			fillFns.emplace_back(std::move(fill));

			break;
		}

		case IVM_JG:{
			auto len = ivmBufferLength(buf);

			emitCmpRegImm32(buf, static_cast<IvmX86Reg>(instr->label.reg), instr->label.imm.i32);
			emitJgNear32(buf, 0);

			auto endPtr = reinterpret_cast<char*>(ivmBufferPtr(buf)) + ivmBufferLength(buf);
			auto offPtr = reinterpret_cast<std::int32_t*>(endPtr) - 1;

			auto fill = [len, offPtr, instr, emitter]{
				auto res = emitter->labelIndices.find(instr->label.name);
				if(res == end(emitter->labelIndices)){
					auto msg = "could not find label '" + std::string(instr->label.name) + "', no look ahead currently implemented";
					return error(msg, 2);
				}

				auto offset = static_cast<std::int32_t>(res->second - len);

				auto cmpSize = 6; // 1 byte opcode + 1 ModRegR/W byte + 4 imm bytes
				auto jumpSize = 6; // 2 byte opcode + 4 rel bytes

				*offPtr = offset - jumpSize - cmpSize;

				return 0;
			};

			fillFns.emplace_back(std::move(fill));

			break;
		}

		case IVM_JE:{
			auto len = ivmBufferLength(buf);

			emitCmpRegImm32(buf, static_cast<IvmX86Reg>(instr->label.reg), instr->label.imm.i32);
			emitJeNear32(buf, 0);

			auto endPtr = reinterpret_cast<char*>(ivmBufferPtr(buf)) + ivmBufferLength(buf);
			auto offPtr = reinterpret_cast<std::int32_t*>(endPtr) - 1;

			auto fill = [len, offPtr, instr, emitter]{
				auto res = emitter->labelIndices.find(instr->label.name);
				if(res == end(emitter->labelIndices)){
					auto msg = "could not find label '" + std::string(instr->label.name) + "', no look ahead currently implemented";
					return error(msg, 2);
				}

				auto offset = static_cast<std::int32_t>(res->second - len);

				auto cmpSize = 6; // 1 byte opcode + 1 ModRegR/W byte + 4 imm bytes
				auto jumpSize = 6; // 2 byte opcode + 4 rel bytes

				*offPtr = offset - jumpSize - cmpSize;

				return 0;
			};

			fillFns.emplace_back(std::move(fill));

			break;
		}

		case IVM_SET:{
			switch(instr->opType){
				case IVM_OPTYPE_IMM:{
					if(instr->binary.imm.i32 < 0x2){
						emitXorRegReg(buf, static_cast<IvmX86Reg>(instr->binary.dst), static_cast<IvmX86Reg>(instr->binary.dst));
						if(instr->binary.imm.i32 == 0x1){
							emitIncReg(buf, static_cast<IvmX86Reg>(instr->binary.dst));
						}
					}
					else{
						emitMovRegImm32(buf, static_cast<IvmX86Reg>(instr->binary.dst), instr->binary.imm.i32);
					}

					break;
				}

				case IVM_OPTYPE_REG:{
					emitMovRegReg(buf, static_cast<IvmX86Reg>(instr->binary.dst), static_cast<IvmX86Reg>(instr->binary.src));
					break;
				}

				default:
					return error("unexpected value for SetInstr::type", 2);
			}

			break;
		}

		case IVM_IADD:{
			switch(instr->opType){
				case IVM_OPTYPE_IMM:{
					emitAddRegImm32(buf, static_cast<IvmX86Reg>(instr->binary.dst), instr->binary.imm.i32);
					break;
				}

				default:
					return error("unexpected value for AddInstr::type", 2);
			}

			break;
		}

		case IVM_IMUL:{
			switch(instr->opType){
				case IVM_OPTYPE_REG:{
					emitIMulRegReg(buf, static_cast<IvmX86Reg>(instr->binary.dst), static_cast<IvmX86Reg>(instr->binary.src));
					break;
				}

				default:
					return error("unexpected value for MulInstr::type", 2);
			}

			break;
		}

		case IVM_INC:{
			emitIncReg(buf, static_cast<IvmX86Reg>(instr->binary.dst));
			break;
		}

		case IVM_DEC:{
			emitDecReg(buf, static_cast<IvmX86Reg>(instr->binary.dst));
			break;
		}

		case IVM_RET:{
			switch(instr->opType){
				case IVM_OPTYPE_IMM:{
					if(instr->unary.imm.i32 < 0x2){
						emitXorRegReg(buf, IVM_X86REG_RAX, IVM_X86REG_RAX);
						if(instr->unary.imm.i32 == 0x1){
							emitIncReg(buf, IVM_X86REG_RAX);
						}
					}
					else{
						emitMovRegImm32(buf, IVM_X86REG_RAX, instr->unary.imm.i32);
					}

					break;
				}

				case IVM_OPTYPE_REG:{
					if(instr->unary.reg != 0){
						emitMovRegReg(buf, IVM_X86REG_RAX, static_cast<IvmX86Reg>(instr->unary.reg));
					}

					break;
				}

				default:
					return error("unexpected value for RetInstr::type", 2);
			}

			emitRet(buf);
			break;
		}

		default:{
			return error("unexpected instruction", 1);
		}
	}

	for(auto &&fn : fillFns){
		auto res = fn();
		if(res != 0) return res;
	}

	return 0;
}

IvmEmitter ivmCreateEmitter(IvmArch arch, IvmPlatform platform){
	assert(arch < IVM_ARCH_COUNT);
	assert(platform < IVM_PLATFORM_COUNT);

	IvmEmitFn emitFn = nullptr;

	switch(arch){
		case IVM_ARCH_BYTECODE:{
			assert((platform == IVM_PLATFORM_VM) && "Only VM platform supported for bytecode arch");
			emitFn = emitInstrBytecode;
			break;
		}
		case IVM_ARCH_X64:{
			assert((platform == IVM_PLATFORM_LINUX) && "Only Linux platform currently implemented for x86-64 arch");
			emitFn = emitInstrX64Linux;
			break;
		}
		default:
			assert((arch == IVM_ARCH_X64) && "Only 64-bit linux currently supported");
			break;
	}

	auto mem = std::malloc(sizeof(IvmEmitterT));
	if(!mem) return nullptr;

	auto pageSize = sysconf(_SC_PAGESIZE);

	auto p = new(mem) IvmEmitterT;

	p->emitFn = emitFn;
	p->buffer = ivmCreateBuffer(pageSize);

	return p;
}

void ivmDestroyEmitter(IvmEmitter emitter){
	ivmDestroyBuffer(emitter->buffer);
	std::destroy_at(emitter);
	std::free(emitter);
}

void ivmEmitterCall(IvmEmitter emitter, IvmCaller caller, void *data){
	ivmBufferMakeExecutable(emitter->buffer);

	caller(data, ivmBufferLength(emitter->buffer), ivmBufferPtr(emitter->buffer));

	ivmBufferMakeWritable(emitter->buffer);
}

int ivmEmitInstr(IvmEmitter emitter, const IvmInstr *instr){
	return emitter->emitFn(emitter, instr);
}

int ivmEmitInstrs(IvmEmitter emitter, size_t n, const IvmInstr *instrs){
	for(size_t i = 0; i < n; i++){
		auto res = ivmEmitInstr(emitter, instrs + i);
		if(res != 0) return res;
	}

	return 0;
}

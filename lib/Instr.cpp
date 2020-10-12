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

#include <cassert>
#include <cstdio>
#include <cstring>

#include <numeric>
#include <functional>
#include <string>
#include <vector>
#include <map>
#include <filesystem>

#include "ivm/Instr.h"
#include "ivm/Buffer.h"

#include "emit.hpp"

inline IvmOperand makeOperandImm(IvmImm imm){
	IvmOperand ret;
	ret.type = IVM_OPTYPE_IMM;
	ret.data.imm = imm;
	return ret;
}

inline IvmOperand makeOperandReg(uint8_t reg){
	IvmOperand ret;
	ret.type = IVM_OPTYPE_REG;
	ret.data.reg = reg;
	return ret;
}

inline IvmInstr makeUnaryOp(IvmInstrType type, IvmOperand op){
	IvmInstr ret;
	ret.flags = ivmMakeInstrFlags(type, op.type, IVM_OPTYPE_VOID, IVM_OPTYPE_VOID);
	ret.op0 = op.data;
	return ret;
}

inline IvmInstr makeBinaryOp(IvmInstrType type, IvmOperand op0, IvmOperand op1){
	IvmInstr ret;
	ret.flags = ivmMakeInstrFlags(type, op0.type, op1.type, IVM_OPTYPE_VOID);
	ret.op0 = op0.data;
	ret.op1 = op1.data;
	return ret;
}

inline IvmInstr makeLabelOp(IvmInstrType type, uint16_t idx){
	IvmInstr ret;
	ret.flags = ivmMakeInstrFlags(type, IVM_OPTYPE_IMM, IVM_OPTYPE_VOID, IVM_OPTYPE_VOID);
	ret.op0 = IVMIMM(idx).data;
	return ret;
}

inline IvmInstr makeCmpOp(IvmInstrType type, IvmOperand op0, IvmOperand op1){
	IvmInstrType instrType = type;

	if(op1.type == IVM_OPTYPE_IMM){
		const bool isSigned = ivmImmTypeIsSigned(op1.data.imm.type);

		switch(type){
			case IVM_LT: instrType = isSigned ? type : IVM_LTN; break;
			case IVM_GT: instrType = isSigned ? type : IVM_GTN; break;
			case IVM_LE: instrType = isSigned ? type : IVM_LEN; break;
			case IVM_GE: instrType = isSigned ? type : IVM_GEN; break;
			default: break;
		}
	}

	IvmInstr ret;
	ret.flags = ivmMakeInstrFlags(instrType, op0.type, op1.type, IVM_OPTYPE_VOID);
	ret.op0 = op0.data;
	ret.op1 = op1.data;
	return ret;
}

inline IvmInstr makeJumpCmpOp(IvmInstrType type, uint16_t labelIdx, IvmOperand op0, IvmOperand op1){
	IvmInstrType instrType = type;

	if(op1.type == IVM_OPTYPE_IMM){
		const bool isSigned = ivmImmTypeIsSigned(op1.data.imm.type);

		switch(type){
			case IVM_JLT:
			case IVM_LT: instrType = isSigned ? IVM_JLT : IVM_JLTN; break;
			case IVM_JGT:
			case IVM_GT: instrType = isSigned ? IVM_JGT : IVM_JGTN; break;
			case IVM_JLE:
			case IVM_LE: instrType = isSigned ? IVM_JLE : IVM_JLEN; break;
			case IVM_JGE:
			case IVM_GE: instrType = isSigned ? IVM_JGE : IVM_JGEN; break;
			case IVM_EQ: instrType = IVM_JEQ; break;
			case IVM_NE: instrType = IVM_JNE; break;
			default: break;
		}
	}

	IvmInstr ret;
	ret.flags = ivmMakeInstrFlags(instrType, IVM_OPTYPE_IMM, op0.type, op1.type);
	ret.op0 = IVMIMM(labelIdx).data;
	ret.op1 = op0.data;
	ret.op2 = op1.data;
	return ret;
}

IvmOperand ivm_immN8(uint8_t v){
	IvmImm ret;
	ret.type = IVM_IMM_N8;
	ret.n8 = v;
	return makeOperandImm(ret);
}

IvmOperand ivm_immN16(uint16_t v){
	IvmImm ret;
	ret.type = IVM_IMM_N16;
	ret.n16 = v;
	return makeOperandImm(ret);
}

IvmOperand ivm_immN32(uint32_t v){
	IvmImm ret;
	ret.type = IVM_IMM_N32;
	ret.n32 = v;
	return makeOperandImm(ret);
}

IvmOperand ivm_immN64(uint64_t v){
	IvmImm ret;
	ret.type = IVM_IMM_N64;
	ret.n64 = v;
	return makeOperandImm(ret);
}

IvmOperand ivm_immI8(int8_t v){
	IvmImm ret;
	ret.type = IVM_IMM_I8;
	ret.i8 = v;
	return makeOperandImm(ret);
}

IvmOperand ivm_immI16(int16_t v){
	IvmImm ret;
	ret.type = IVM_IMM_I16;
	ret.i16 = v;
	return makeOperandImm(ret);
}

IvmOperand ivm_immI32(int32_t v){
	IvmImm ret;
	ret.type = IVM_IMM_I32;
	ret.i32 = v;
	return makeOperandImm(ret);
}

IvmOperand ivm_immI64(int64_t v){
	IvmImm ret;
	ret.type = IVM_IMM_I64;
	ret.i64 = v;
	return makeOperandImm(ret);
}

IvmInstr ivm_nop(void){
	IvmInstr ret;
	ret.flags = ivmMakeInstrFlags(IVM_NOP, IVM_OPTYPE_VOID, IVM_OPTYPE_VOID, IVM_OPTYPE_VOID);
	return ret;
}

IvmInstr ivm_set(IvmOperand dst, IvmOperand src){ return makeBinaryOp(IVM_SET, dst, src); }

IvmInstr ivm_add(IvmOperand dst, IvmOperand src){ return makeBinaryOp(IVM_IADD, dst, src); }
IvmInstr ivm_sub(IvmOperand dst, IvmOperand src){ return makeBinaryOp(IVM_ISUB, dst, src); }
IvmInstr ivm_mul(IvmOperand dst, IvmOperand src){ return makeBinaryOp(IVM_IMUL, dst, src); }

IvmInstr ivm_inc(IvmOperand dst){ return makeUnaryOp(IVM_INC, dst); }
IvmInstr ivm_dec(IvmOperand dst){ return makeUnaryOp(IVM_DEC, dst); }

IvmInstr ivm_label(uint16_t idx){ return makeLabelOp(IVM_LABEL, idx); }

IvmInstr ivm_fn(size_t nameLen, const char *name){
	IvmInstr ret;
	ret.flags = ivmMakeInstrFlags(IVM_FN, IVM_OPTYPE_IMM, IVM_OPTYPE_IMM, IVM_OPTYPE_VOID);
	ret.op0 = IVMIMM(uint64_t(nameLen)).data;
	ret.op1 = IVMIMM(uintptr_t(name)).data;
	return ret;
}

IvmInstr ivm_jump(uint16_t labelIdx){ return makeLabelOp(IVM_JMP, labelIdx); }

IvmInstr ivm_lt(IvmOperand dst, IvmOperand src){ return makeCmpOp(IVM_LT, dst, src); }
IvmInstr ivm_gt(IvmOperand dst, IvmOperand src){ return makeCmpOp(IVM_GT, dst, src); }
IvmInstr ivm_le(IvmOperand dst, IvmOperand src){ return makeCmpOp(IVM_LE, dst, src); }
IvmInstr ivm_ge(IvmOperand dst, IvmOperand src){ return makeCmpOp(IVM_GE, dst, src); }
IvmInstr ivm_eq(IvmOperand dst, IvmOperand src){ return makeCmpOp(IVM_EQ, dst, src); }
IvmInstr ivm_ne(IvmOperand dst, IvmOperand src){ return makeCmpOp(IVM_NE, dst, src); }

IvmInstr ivm_jlt(uint16_t labelIdx, IvmOperand op0, IvmOperand op1){ return makeJumpCmpOp(IVM_JLT, labelIdx, op0, op1); }
IvmInstr ivm_jgt(uint16_t labelIdx, IvmOperand op0, IvmOperand op1){ return makeJumpCmpOp(IVM_JGT, labelIdx, op0, op1); }
IvmInstr ivm_jle(uint16_t labelIdx, IvmOperand op0, IvmOperand op1){ return makeJumpCmpOp(IVM_JLE, labelIdx, op0, op1); }
IvmInstr ivm_jge(uint16_t labelIdx, IvmOperand op0, IvmOperand op1){ return makeJumpCmpOp(IVM_JGE, labelIdx, op0, op1); }
IvmInstr ivm_jeq(uint16_t labelIdx, IvmOperand op0, IvmOperand op1){ return makeJumpCmpOp(IVM_JEQ, labelIdx, op0, op1); }
IvmInstr ivm_jne(uint16_t labelIdx, IvmOperand op0, IvmOperand op1){ return makeJumpCmpOp(IVM_JNE, labelIdx, op0, op1); }

IvmInstr ivm_ret(IvmOperand operand){ return makeUnaryOp(IVM_RET, operand); }

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

enum class X64CallConv{
	SysV, Win32
};

int emitInstrBytecode(IvmEmitter emitter, size_t n, const IvmInstr *instrs);
int emitInstrsX64(X64CallConv cc, IvmEmitter emitter, size_t n, const IvmInstr *instr);

using IvmEmitFn = std::function<int(IvmEmitter, size_t, const IvmInstr*)>;

struct IvmEmitterT{
	IvmEmitFn emitFn;
	IvmBuffer buffer;
	std::map<uint8_t, size_t, std::less<>> labelOffsets;
	std::map<std::string, size_t, std::less<>> fnOffsets;
	bool inFn = false;
};

IvmEmitter ivmCreateEmitter(IvmArch arch, IvmPlatform platform){
	assert(arch < IVM_ARCH_COUNT);
	assert(platform < IVM_PLATFORM_COUNT);

	IvmEmitFn emitFn = nullptr;

	switch(arch){
		case IVM_ARCH_BYTECODE:{
			if(platform != IVM_PLATFORM_VM) return nullptr;
			emitFn = emitInstrBytecode;
			break;
		}
		case IVM_ARCH_X64:{
			switch(platform){
				case IVM_PLATFORM_LINUX:{
					emitFn = [](IvmEmitter emitter, size_t n, const IvmInstr *instrs){ return emitInstrsX64(X64CallConv::SysV, emitter, n, instrs); };
					break;
				}

				case IVM_PLATFORM_WIN32:{
					emitFn = [](IvmEmitter emitter, size_t n, const IvmInstr *instrs){ return emitInstrsX64(X64CallConv::Win32, emitter, n, instrs); };
					break;
				}

				default:{
					return nullptr;
				}
			}

			break;
		}

		default:
			return nullptr;
	}

	auto mem = std::malloc(sizeof(IvmEmitterT));
	if(!mem) return nullptr;

	auto p = new(mem) IvmEmitterT;

	p->emitFn = emitFn;
	p->buffer = ivmCreatePageBuffer();

	return p;
}

void ivmDestroyEmitter(IvmEmitter emitter){
	ivmDestroyBuffer(emitter->buffer);
	std::destroy_at(emitter);
	std::free(emitter);
}

void ivmEmitterCall(IvmEmitter emitter, size_t nameLen, const char *name, IvmCaller caller, void *data){
	ivmBufferMakeExecutable(emitter->buffer);

	auto nameStr = std::string(name, nameLen);

	auto res = emitter->fnOffsets.find(nameStr);
	if(res != end(emitter->fnOffsets)){
		auto off = res->second;
		auto len = ivmBufferLength(emitter->buffer) - off;
		auto code = reinterpret_cast<char*>(ivmBufferPtr(emitter->buffer)) + off;
		caller(data, len, code);
	}

	ivmBufferMakeWritable(emitter->buffer);
}

int ivmEmitInstr(IvmEmitter emitter, const IvmInstr *instr){
	return emitter->emitFn(emitter, 1, instr);
}

int ivmEmitInstrs(IvmEmitter emitter, size_t n, const IvmInstr *instrs){
	return emitter->emitFn(emitter, n, instrs);
}

void ivmWriteElf(IvmEmitter emitter, size_t pathLen, const char *path){
	namespace fs = std::filesystem;

	auto p = fs::path(std::string_view(path, pathLen));

	assert(!fs::exists(p) && "file already exists");

	assert(!"ELF file writing unimplemented");
}

static inline int error(std::string_view str, int code = 1){
	std::fprintf(stderr, "Error: %.*s\n", int(str.size()), str.data());
	return code;
}

int emitInstrBytecode(IvmEmitter emitter, size_t n, const IvmInstr *instrs){
	(void)emitter;
	(void)n;
	(void)instrs;
	return error("bytecode emission currently unimplemented", 1);
}

int emitInstrsX64(X64CallConv cc, IvmEmitter emitter, size_t n, const IvmInstr *instrs){
	using namespace ivm::x64;

	X64EmitCtx ctx;
	ctx.buffer = emitter->buffer;

	switch(cc){
		case X64CallConv::Win32:{
			ctx.paramRegs = {IVM_X64REG_RCX, IVM_X64REG_RDX, IVM_X64REG_R8, IVM_X64REG_R9 };
			break;
		}

		default:
		case X64CallConv::SysV:{
			// RDI, RSI, RDX, RCX, R8, R9
			ctx.paramRegs = {IVM_X64REG_RDI, IVM_X64REG_RSI, IVM_X64REG_RDX, IVM_X64REG_RCX, IVM_X64REG_R8, IVM_X64REG_R9 };
			break;
		}
	}

	std::vector<std::function<int()>> fillFns;

	auto fillJmp = [emitter](auto instr, size_t instrSize, size_t len, auto offPtr){
		auto labelIdx = instr->op0.imm.n16;

		auto res = emitter->labelOffsets.find(labelIdx);
		if(res == end(emitter->labelOffsets)){
			auto msg = "could not find label " + std::to_string(labelIdx);
			return error(msg, 2);
		}

		auto offset = static_cast<std::int32_t>(res->second - len);

		*offPtr = offset - instrSize;

		return 0;
	};

	auto emitJmp = [&](auto instr, bool doCmp, auto emitJmpFn){
		auto len = ivmBufferLength(ctx.buffer);

		if(doCmp){
			IvmOperand op1, op2;
			op1.type = ivmInstrFlagsOpType(instr->flags, 1);
			op1.data = instr->op1;
			op2.type = ivmInstrFlagsOpType(instr->flags, 2);
			op2.data = instr->op2;
			emitCmp(&ctx, op1, op2);
		}

		emitJmpFn(&ctx, 0);

		// after emitting 'rel' will be at end of buffer

		auto instrSize = ivmBufferLength(ctx.buffer) - len;

		auto endPtr = reinterpret_cast<char*>(ivmBufferPtr(ctx.buffer)) + ivmBufferLength(ctx.buffer);
		auto offPtr = reinterpret_cast<std::int32_t*>(endPtr) - 1; // get pointer to 'rel'

		auto fill = [&fillJmp, instr, len, offPtr, instrSize](){
			return fillJmp(instr, instrSize, len, offPtr);
		};

		fillFns.emplace_back(std::move(fill));

		return 0;
	};

	for(size_t i = 0; i < n; i++){
		auto instr = instrs + i;

		auto type = ivmInstrFlagsType(instr->flags);
		auto op0 = IvmOperand{ .type = ivmInstrFlagsOpType(instr->flags, 0), .data = instr->op0 };
		auto op1 = IvmOperand{ .type = ivmInstrFlagsOpType(instr->flags, 1), .data = instr->op1 };
		auto op2 = IvmOperand{ .type = ivmInstrFlagsOpType(instr->flags, 2), .data = instr->op2 };

		switch(type){
			case IVM_NOP:{ break; }

			case IVM_LABEL:{
				auto labelIdx = instr->op0.imm.n16;

				auto res = emitter->labelOffsets.find(labelIdx);
				if(res != end(emitter->labelOffsets)){
					auto msg =  "label with index " + std::to_string(labelIdx) + " already exists";
					return error(msg, 3);
				}

				emitter->labelOffsets[labelIdx] = ivmBufferLength(ctx.buffer);
				break;
			}

			case IVM_FN:{
				if(emitter->inFn){
					return error("can not define nested functions", 4);
				}

				auto nameLen = instr->op0.imm.n64;
				auto namePtr = reinterpret_cast<const char*>(instr->op1.imm.n64);

				auto name = std::string(namePtr, nameLen);

				auto res = emitter->fnOffsets.find(name);
				if(res != end(emitter->fnOffsets)){
					auto msg =  "function with name '" + std::string(name) + "' already exists";
					return error(msg, 3);
				}

				emitter->fnOffsets[name] = ivmBufferLength(ctx.buffer);

				if(cc == X64CallConv::SysV){
					emitPushReg(&ctx, true, IVM_X64REG_RBP);
					emitMovRegReg(&ctx, IVM_X64REG_RBP, IVM_X64REG_RSP);
				}

				emitter->inFn = true;

				break;
			}

			case IVM_JMP:{
				auto res = emitJmp(instr, false, emitJmpn32);
				if(res != 0) return res;
				break;
			}

			case IVM_JEQ:{
				auto res = emitJmp(instr, true, emitJneq32);
				if(res != 0) return res;
				break;
			}

			case IVM_JNE:{
				auto res = emitJmp(instr, true, emitJnne32);
				if(res != 0) return res;
				break;
			}

			case IVM_JLT:{
				auto res = emitJmp(instr, true, emitJnlt32);
				if(res != 0) return res;
				break;
			}

			case IVM_JGT:{
				auto res = emitJmp(instr, true, emitJngt32);
				if(res != 0) return res;
				break;
			}

			case IVM_JLE:{
				auto res = emitJmp(instr, true, emitJnle32);
				if(res != 0) return res;
				break;
			}

			case IVM_JGE:{
				auto res = emitJmp(instr, true, emitJnge32);
				if(res != 0) return res;
				break;
			}

			case IVM_SET:{
				IvmX64Reg dstReg = IVM_X64REG_RAX;

				if(op0.type == IVM_OPTYPE_REG){
					dstReg = IvmX64Reg(instr->op0.reg);
				}
				else if(op0.type == IVM_OPTYPE_PARAM){
					if(op0.data.param >= ctx.paramRegs.size()){
						std::fprintf(stderr, "Error: max %zu params currently supported\n", ctx.paramRegs.size());
						return 69;
					}

					dstReg = IvmX64Reg(ctx.paramRegs[instr->op0.param]);
				}
				else{
					std::fprintf(stderr, "Error: only register or param dst operand supported by SET\n");
					return 1;
				}

				switch(op1.type){
					case IVM_OPTYPE_IMM:{
						if(instr->op1.imm.n32 < 0x2){
							emitXorRegReg(&ctx, dstReg, dstReg);
							if(instr->op1.imm.n32 == 0x1){
								emitIncReg(&ctx, dstReg);
							}
						}
						else{
							emitMovRegImm32(&ctx, dstReg, instr->op1.imm.n32);
						}

						break;
					}

					case IVM_OPTYPE_REG:{
						emitMovRegReg(&ctx, dstReg, static_cast<IvmX64Reg>(instr->op1.reg));
						break;
					}

					case IVM_OPTYPE_PARAM:{
						if(instr->op1.param >= ctx.paramRegs.size()){
							std::fprintf(stderr, "Error: max %zu params currently supported\n", ctx.paramRegs.size());
							return 69;
						}

						emitMovRegReg(&ctx, dstReg, static_cast<IvmX64Reg>(ctx.paramRegs[instr->op1.param]));
						break;
					}

					default: return error("unexpected value for SetInstr::type", 2);
				}

				break;
			}

			case IVM_IADD:{
				int res = emitAdd(&ctx, op0, op1);
				if(res != 0) return res;
				break;
			}

			case IVM_ISUB:{
				int res = emitSub(&ctx, op0, op1);
				if(res != 0) return res;
				break;
			}

			case IVM_IMUL:{
				auto res = emitIMul(&ctx, op0, op1);
				if(res != 0) return res;
				break;
			}

			case IVM_INC:{
				emitIncReg(&ctx, static_cast<IvmX64Reg>(instr->op0.reg));
				break;
			}

			case IVM_DEC:{
				auto res = emitDec(&ctx, op0);
				if(res != 0) return res;
				break;
			}

			case IVM_RET:{
				if(!emitter->inFn){
					return error("can not emit return instruction outside of function", 4);
				}

				switch(op0.type){
					case IVM_OPTYPE_IMM:{
						switch(instr->op0.imm.type){
							case IVM_IMM_N32:
							case IVM_IMM_N64:{
								uint64_t bits;

								if(instr->op0.imm.type == IVM_IMM_N32){
									bits = instr->op0.imm.n32;
								}
								else{
									bits = instr->op0.imm.n64;
								}

								if(bits < 0x2){
									emitXorRegReg(&ctx, IVM_X64REG_RAX, IVM_X64REG_RAX);
									if(bits == 0x1){
										emitIncReg(&ctx, IVM_X64REG_RAX);
									}
								}
								else{
									emitMovRegImm32(&ctx, IVM_X64REG_RAX, bits);
								}

								break;
							}
							case IVM_IMM_I32:
							case IVM_IMM_I64:{
								int64_t bits;

								if(instr->op0.imm.type == IVM_IMM_I32){
									bits = instr->op0.imm.i32;
								}
								else{
									bits = instr->op0.imm.i64;
								}

								if(bits < 0x2 && bits > -0x2){
									emitXorRegReg(&ctx, IVM_X64REG_RAX, IVM_X64REG_RAX);
									if(bits == 0x1){
										emitIncReg(&ctx, IVM_X64REG_RAX);
									}
									else if(bits == -0x1){
										emitDecReg(&ctx, IVM_X64REG_RAX);
									}
								}
								else{
									emitMovRegImm32(&ctx, IVM_X64REG_RAX, bits);
								}

								break;
							}

							default:
								return error("only 32 and 64 bit integers currently supported", 69);
						}

						break;
					}

					case IVM_OPTYPE_REG:{
						if(instr->op0.reg != IVM_X64REG_RAX){
							emitMovRegReg(&ctx, IVM_X64REG_RAX, static_cast<IvmX64Reg>(instr->op0.reg));
						}

						break;
					}

					case IVM_OPTYPE_PARAM:{
						if(instr->op0.param >= ctx.paramRegs.size()){
							std::fprintf(stderr, "Error: max %zu params currently supported\n", ctx.paramRegs.size());
							return 69;
						}

						auto srcReg = ctx.paramRegs[instr->op0.param];

						if(srcReg != IVM_X64REG_RAX){
							emitMovRegReg(&ctx, IVM_X64REG_RAX, srcReg);
						}

						break;
					}

					default:
						return error("unexpected value for RetInstr::type", 2);
				}

				if(cc == X64CallConv::SysV){
					emitPopReg(&ctx, PopSize::_64, IVM_X64REG_RBP);
				}

				emitRet(&ctx);

				emitter->inFn = false;

				break;
			}

			default:{
				return error("unexpected instruction", 1);
			}
		}
	}

	for(auto &&fn : fillFns){
		auto res = fn();
		if(res != 0){
			return res;
		}
	}

	return 0;
}

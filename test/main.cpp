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
#include <iostream>
#include <fstream>
#include <chrono>
#include <vector>

#include "ivm/Instr.h"
#include "ivm/IR.h"

#if defined(_WIN32)
#define TEST_ARCH IVM_ARCH_X64
#define TEST_PLATFORM IVM_PLATFORM_WIN32
#elif defined(__linux__)
#define TEST_ARCH IVM_ARCH_X64
#define TEST_PLATFORM IVM_PLATFORM_LINUX
#else
#define TEST_ARCH IVM_ARCH_BYTECODE
#define TEST_PLATFORM IVM_PLATFORM_VM
#endif

using Clock = std::chrono::high_resolution_clock;

extern "C"
int cFact(int x){
	int res = 1;
	for(; x > 0; x--){
		res *= x;
	}
	return res;
}

extern "C"
int cFib(int i){
	int tmp;
	if(i < 2){
		return i;
	}
	else{
		tmp = cFib(i - 1);
		tmp += cFib(i - 2);
		return tmp;
	}
}

std::string openIr(){
	std::ifstream file("test.iir");
	if(!file) throw std::runtime_error("could not open test file 'test.iir'");

	std::string src, tmp;
	while(std::getline(file, tmp)){
		src += tmp + '\n';
	}

	return src;
}

IvmCompileResultPtr compileFact(){
	auto lit0 = ivmir_litInt32(0);
	auto lit1 = ivmir_litInt32(1);

	auto int32Ty = (IvmIRType)ivmIRTypeInt(32);

	auto param0 = ivmir_param(int32Ty);
	auto paramSub1 = ivmir_dec(&param0.base.base);
	auto decParam = ivmir_store(&param0.base, &paramSub1.base);

	auto accumVar = ivmir_local(int32Ty);
	auto setAccum1 = ivmir_store(&accumVar, &lit1.base);
	auto retAccum = ivmir_return(&accumVar.base);

	auto accumMul = ivmir_mul(&accumVar.base, &param0.base.base);
	auto accumSet = ivmir_store(&accumVar, &accumMul.base);

	auto branchCond = ivmir_gt(&param0.base.base, &lit0.base);
	auto branch = ivmir_branch(&branchCond.base, nullptr, nullptr);

	IvmIRNodePtr trueBody[] = {
		&accumSet.base,
		&decParam.base,
		&branch.base,
	};

	auto trueBlock = ivmir_block(std::size(trueBody), trueBody);

	IvmIRNodePtr falseBody[] = {
		&retAccum.base
	};

	auto falseBlock = ivmir_block(std::size(falseBody), falseBody);

	branch.trueBlock = &trueBlock;
	branch.falseBlock = &falseBlock;

	auto factFn = ivmir_fnUndef(4, "fact", int32Ty, 1, &param0);
	auto factLam = &factFn.base;

	IvmIRNodePtr factBody[] = {
		&accumVar.base,
		&setAccum1.base,
		&branch.base
	};

	auto factBlock = ivmir_block(std::size(factBody), factBody);

	ivmIRLambdaDef(factLam, &factBlock);

	IvmIRNodePtr irNodes[] = { &factLam->base };

	return ivmCompileIR(std::size(irNodes), irNodes);
}

/*
IvmCompileResultPtr compileFib(){
	auto int32Ty = IvmIRType(ivmIRTypeInt(32));

	auto lit0 = ivmir_litInt32(0);
	auto lit1 = ivmir_litInt32(1);

	auto param0 = ivmir_param(int32Ty);

	auto lastVar = ivmir_local(int32Ty);
	auto accumVar = ivmir_local(int32Ty);
	auto tmpVar = ivmir_local(int32Ty);

	auto setAccum0 = ivmir_store(&accumVar, &lit0.base);
	auto setTmp = ivmir_store(&tmpVar, &accumVar.base);
	auto setLast = ivmir_store(&lastVar, &tmpVar.base);
	auto addAccum = ivmir_add(&lastVar.base, &accumVar.base);
	auto setAccum = ivmir_store(&accumVar, &addAccum.base);
	auto subParam = ivmir_sub(&param0.base.base, &lit1.base);
	auto setParam = ivmir_store(&param0.base, &subParam.base);

	auto retAccum = ivmir_return(&accumVar.base);

	IvmIRNodePtr branchTrueBody[] = {
		&setTmp.base,
		&setAccum.base,
		&setLast.base,
		&setParam.base
	};

	IvmIRNodePtr branchFalseBody[] = {
		&retAccum.base
	};

	auto branchTrueBlock = ivmir_block(std::size(branchTrueBody), branchTrueBody);
	auto branchFalseBlock = ivmir_block(std::size(branchFalseBody), branchFalseBody);

	auto branchCond = ivmir_gt(&param0.base.base, &lit0.base);

	auto branch = ivmir_branch(&branchCond.base, &branchTrueBlock, &branchFalseBlock);

	auto fibFn = ivmir_fnUndef(3, "fib", int32Ty, 1, &param0);
	auto fibLam = &fibFn.base;

	IvmIRNodePtr fibBody[] = {
		&setAccum0.base,
		&branch.base
	};

	auto fibBlock = ivmir_block(std::size(fibBody), fibBody);

	ivmIRLambdaDef(fibLam, &fibBlock);

	IvmIRNodePtr irNodes[] = { &fibLam->base };

	return ivmCompileIR(std::size(irNodes), irNodes);
}
 */

void factCaller(void*, size_t len, void *code){
	auto bytes = reinterpret_cast<const unsigned char*>(code);

	if(!bytes){
		std::fprintf(stderr, "Error: Could not get compiled function 'fact'\n");
		std::exit(EXIT_FAILURE);
	}

	std::printf("%x", int(bytes[0]));

	for(size_t i = 1; i < len; i++){
		std::printf(" %x", int(bytes[i]));
	}

	std::printf("\n");

	std::fflush(stdout);

	auto iFact = reinterpret_cast<int(*)(int)>(code);

	constexpr size_t numTests = 10'000'000;

	std::srand(std::time(nullptr));

	std::vector<int> testData;
	testData.resize(numTests);

	for(std::size_t i = 0; i < numTests; i++){
		testData[i] = std::rand() % 5;
	}

	int res = 0;

	auto cstart = Clock::now();
	for(size_t i = 0; i < numTests; i++){
		res = cFact(testData[i]);
		(void)res;
	}
	auto cend = Clock::now();

	std::cout << "C factorial result:   " << res << '\n' << std::flush;

	auto istart = Clock::now();
	for(size_t i = 0; i < numTests; i++){
		res = iFact(testData[i]);
		(void)res;
	}
	auto iend = Clock::now();

	std::cout << "IVM factorial result: " << res << '\n' << std::flush;

	auto cdt = cend - cstart;
	auto idt = iend - istart;

	auto cavg = std::chrono::nanoseconds(cdt).count() / double(numTests);
	auto iavg = std::chrono::nanoseconds(idt).count() / double(numTests);
	auto davg = iavg - cavg;

	std::cout << numTests << " tests\n";
	std::cout << "C factorial avg:   " << cavg << "ns\n";
	std::cout << "IVM factorial avg: " << std::chrono::nanoseconds(idt).count() / double(numTests) << "ns\n";
	std::cout << davg << " difference\n";
}

int main(int argc, char *argv[]){
	(void)argc;
	(void)argv;

#define outExpr(expr) (std::cout << #expr " = " << (expr) << '\n')

	outExpr(sizeof(IvmImm));
	outExpr(sizeof(IvmOpData));
	outExpr(sizeof(IvmInstr));

#undef outExpr

	/*
	auto irSrc = openIr();

	auto parseRes = ivmParseIR(irSrc.size(), irSrc.data());

	auto compRes = ivmCompileIR(ivmParseResultNumNodes(parseRes), ivmParseResultNodes(parseRes));

	if(!compRes){
		std::fprintf(stdout, "Error compiling\n");
		std::exit(EXIT_FAILURE);
	}

	auto emitter = ivmCreateEmitter(TEST_ARCH, TEST_PLATFORM);

	if(ivmEmitInstrs(emitter, ivmCompileResultNumInstrs(compRes), ivmCompileResultInstrs(compRes)) == 0){
		ivmEmitterCall(emitter, 4, "fact", factCaller, nullptr);
	}
	else{
		std::cerr << "JIT compile failed\n";
	}

	ivmDestroyCompileResult(compRes);
	ivmDestroyParseResult(parseRes);
	 */

	auto compRes = compileFact();

	if(!compRes){
		std::fprintf(stdout, "Error compiling\n");
		std::exit(EXIT_FAILURE);
	}

	{
		auto emitter = ivmCreateEmitter(TEST_ARCH, TEST_PLATFORM);

		if(ivmEmitInstrs(emitter, ivmCompileResultNumInstrs(compRes), ivmCompileResultInstrs(compRes)) == 0){
			ivmEmitterCall(emitter, 4, "fact", factCaller, nullptr);
		}
		else{
			std::cerr << "JIT compile failed\n";
		}

		ivmDestroyCompileResult(compRes);
		ivmDestroyEmitter(emitter);
	}

	/*
	auto emitter = ivmCreateEmitter(TEST_ARCH, TEST_PLATFORM);

	IvmInstr instrs[] = {
		ivm_fn(4, "fact"),
		ivm_set(IVMREG(0), IVMIMM(1)),
		ivm_label(0),
		ivm_mul(IVMREG(0), IVMPARAM(0)),
		ivm_dec(IVMPARAM(0)),
		ivm_jgt(0, IVMPARAM(0), IVMIMM(0)),
		ivm_ret(IVMREG(0)),
		//ivm_nop()
	};

	if(ivmEmitInstrs(emitter, std::size(instrs), instrs) == 0){
		ivmEmitterCall(emitter, 4, "fact", factCaller, nullptr);
	}
	else{
		std::cerr << "JIT compile failed\n";
	}

	ivmDestroyEmitter(emitter);
	 */

	return 0;
}

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

#include <iostream>

#include "ivm/Instr.h"

int main(int argc, char *argv[]){
	(void)argc;
	(void)argv;

	//auto emitter = ivmCreateEmitter(IVM_ARCH_BYTECODE, IVM_PLATFORM_VM);
	auto emitter = ivmCreateEmitter(IVM_ARCH_X64, IVM_PLATFORM_LINUX);

	IvmInstr instrs[] = {
		ivm_set(1, 5),
		ivm_set(0, 1),
		ivm_label("l0"),
		ivm_mulr(0, 1),
		ivm_dec(1),
		ivm_jg("l0", 1, 0),
		ivm_retr(0),
	};

	if(ivmEmitInstrs(emitter, std::size(instrs), instrs) == 0){
		auto caller = [](void*, size_t, void *code){
			auto fn = reinterpret_cast<int(*)()>(code);
			std::cout << "JIT Result: " << fn() << '\n';
		};

		ivmEmitterCall(emitter, caller, nullptr);
	}
	else{
		std::cerr << "JIT compile failed\n";
	}

	ivmDestroyEmitter(emitter);

	return 0;
}

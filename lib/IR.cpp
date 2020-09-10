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
#include <cstdio>

#include <memory>

#include "ivm/IR.h"

struct IvmIRTypeT{
	virtual ~IvmIRTypeT() = default;
	virtual size_t numBits() const noexcept{ return 0; }
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

static const auto int8Type = IvmIRIntTypeT(8);
static const auto int16Type = IvmIRIntTypeT(16);
static const auto int32Type = IvmIRIntTypeT(32);
static const auto int64Type = IvmIRIntTypeT(64);

static const auto nat8Type = IvmIRNatTypeT(8);
static const auto nat16Type = IvmIRNatTypeT(16);
static const auto nat32Type = IvmIRNatTypeT(32);
static const auto nat64Type = IvmIRNatTypeT(64);

size_t ivmIRTypeNumBits(IvmIRType ty){ return ty->numBits(); }

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
	virtual ~IvmIRNodeT() = default;
	virtual IvmIRType type() const noexcept{ return nullptr; }
};

struct IvmIRLiteralT: IvmIRNodeT{
	IvmIRType type() const noexcept override{ return m_type; }

	IvmIRType m_type;

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
	} m_val;
};

template<typename T, typename ... Args>
inline T *allocT(Args &&... args){
	auto mem = std::malloc(sizeof(T));
	if(!mem) return nullptr;
	return new(mem) T(std::forward<Args>(args)...);
}

void ivmDestroyIRNode(IvmIRNode node){
	std::destroy_at(node);
	std::free(node);
}

IvmIRLiteral ivmCreateLitInt32(int32_t val){
	auto node = allocT<IvmIRLiteralT>();
	node->m_type = ivmIRTypeInt(32);
	node->m_val.i.v32 = val;
	return node;
}

IvmIRLiteral ivmCreateLitNat32(uint32_t val){
	auto node = allocT<IvmIRLiteralT>();
	node->m_type = ivmIRTypeNat(32);
	node->m_val.n.v32 = val;
	return node;
}

uint64_t ivmIRLiteralValue(IvmIRLiteral lit){ return lit->m_val.bits; }

IvmIRType ivmIRNodeType(IvmIRNode node){ return node->type(); }

int ivmCompileIR(IvmBuffer buf, size_t len, const IvmIRNode *nodes){
	(void)buf;

	for(size_t i = 0; i < len; i++){
		auto node = nodes + i;
		(void)node;

		std::fprintf(stderr, "Error: IR compilation currently unimplemented\n");
		return 1;
	}

	return 0;
}

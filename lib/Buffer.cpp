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
#include <cstdlib>
#include <cstring>
#include <cmath>
#include <new>

#include <sys/mman.h>

#include "ivm/Buffer.h"

struct IvmBufferT{
	void *ptr;
	size_t len, cap;
	bool isWritable;
};

IvmBuffer ivmCreateBuffer(size_t capacity){
	auto ptr = mmap(nullptr, capacity, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
	assert(ptr != MAP_FAILED);

	auto mem = std::malloc(sizeof(IvmBufferT));
	if(!mem) return nullptr;

	auto ret = new(mem) IvmBufferT;

	ret->ptr = ptr;
	ret->len = 0;
	ret->cap = capacity;
	ret->isWritable = true;

	return ret;
}

void ivmDestroyBuffer(IvmBuffer buf){ munmap(buf->ptr, buf->cap); }

bool ivmBufferIsWritable(IvmBufferConst buf){ return buf->isWritable; }

bool ivmBufferIsExecutable(IvmBufferConst buf){ return !buf->isWritable; }

size_t ivmBufferCapacity(IvmBufferConst buf){ return buf->cap; }

size_t ivmBufferLength(IvmBufferConst buf){ return buf->len; }

bool ivmBufferEnsureCapacity(IvmBuffer buf, size_t size){
	if(buf->len + size <= buf->cap) return true;

	auto newCapacity = std::max(buf->cap * 2, buf->cap + size);

	auto newMap = mmap(nullptr, newCapacity, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
	if(newMap == MAP_FAILED){
		return false;
	}

	std::memcpy(newMap, buf->ptr, buf->len);

	// TODO: handle failure
	munmap(buf->ptr, buf->cap);

	buf->ptr = newMap;
	buf->cap = newCapacity;
	buf->isWritable = true;

	return true;
}

void *ivmBufferPtr(IvmBuffer buf){ return buf->ptr; }

const void *ivmBufferConstPtr(IvmBufferConst buf){ return buf->ptr; }

uint8_t ivmBufferGet(IvmBufferConst buf, size_t idx){
	return reinterpret_cast<const uint8_t*>(buf->ptr)[idx];
}

uint8_t ivmBufferSet(IvmBuffer buf, size_t idx, uint8_t bits){
	auto old = reinterpret_cast<uint8_t*>(buf->ptr)[idx];
	reinterpret_cast<uint8_t*>(buf->ptr)[idx] = bits;
	return old;
}

void ivmBufferWrite(IvmBuffer buf, size_t n, const uint8_t *bytes){
	ivmBufferEnsureCapacity(buf, n);
	std::memcpy(reinterpret_cast<char*>(buf->ptr) + buf->len, bytes, n);
	buf->len += n;
}

void ivmBufferWrite8(IvmBuffer buf, uint8_t bits){
	ivmBufferEnsureCapacity(buf, 1);
	ivmBufferSet(buf, buf->len, bits);
	++buf->len;
}

void ivmBufferWrite32(IvmBuffer buf, uint32_t bits){
	static uint8_t bytes[sizeof(bits)];

	for(std::size_t i = 0; i < sizeof(bits); i++){
		bytes[i] = (bits >> (i * 8)) & 0xFF;
	}

	ivmBufferWrite(buf, sizeof(bits), bytes);
}

bool ivmBufferMakeExecutable(IvmBuffer buf){
	if(mprotect(buf->ptr, buf->len, PROT_EXEC) != 0){
		return false;
	}

	buf->isWritable = false;
	return true;
}

bool ivmBufferMakeWritable(IvmBuffer buf){
	if(mprotect(buf->ptr, buf->len, PROT_READ | PROT_WRITE) != 0){
		return false;
	}

	buf->isWritable = true;
	return true;
}

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
#include <cstdlib>
#include <cstring>
#include <cmath>

#include <new>
#include <memory>

#if defined(__linux__)
#include <sys/mman.h>

static inline void *allocMemory(size_t capacity){
	auto ret = mmap(nullptr, capacity, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
	return ret == MAP_FAILED ? nullptr : ret;
}

static inline bool freeMemory(size_t len, void *mem){
	return munmap(mem, len) == 0;
}

static inline bool makeMemoryExecutable(size_t len, void *mem){
	return mprotect(mem, len, PROT_EXEC) == 0;
}

static inline bool makeMemoryWritable(size_t len, void *mem){
	return mprotect(mem, len, PROT_READ | PROT_WRITE) == 0;
}

static inline const char *getAllocError(){ return strerror(errno); }
#elif defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

static inline void *allocMemory(size_t capacity){
	return VirtualAlloc(nullptr, capacity, MEM_COMMIT, PAGE_READWRITE);
}

static inline bool freeMemory(size_t len, void *mem){
	(void)len;
	return VirtualFree(mem, 0, MEM_RELEASE);
}

static inline bool makeMemoryExecutable(size_t len, void *mem){
	DWORD dummy;
	return VirtualProtect(mem, len, PAGE_EXECUTE_READ, &dummy);
}

static inline bool makeMemoryWritable(size_t len, void *mem){
	DWORD dummy;
	return VirtualProtect(mem, len, PAGE_READWRITE, &dummy);
}

static inline const char *getAllocError(){ return "error in VirtualAlloc or VirtualProtect"; }
#else
#error "Unsupported platform"
#endif


#include "ivm/Buffer.h"

struct IvmBufferT{
	void *ptr;
	size_t len, cap;
	bool isWritable;
};

IvmBuffer ivmCreateBuffer(size_t capacity){
	auto mem = std::malloc(sizeof(IvmBufferT));
	if(!mem) return nullptr;

	auto ptr = allocMemory(capacity);
	if(!ptr){
		std::free(mem);
		return nullptr;
	}

	auto ret = new(mem) IvmBufferT;

	ret->ptr = ptr;
	ret->len = 0;
	ret->cap = capacity;
	ret->isWritable = true;

	return ret;
}

void ivmDestroyBuffer(IvmBuffer buf){
	freeMemory(buf->cap, buf->ptr);
	std::destroy_at(buf);
	std::free(buf);
}

bool ivmBufferIsWritable(IvmBufferConst buf){ return buf->isWritable; }

bool ivmBufferIsExecutable(IvmBufferConst buf){ return !buf->isWritable; }

size_t ivmBufferCapacity(IvmBufferConst buf){ return buf->cap; }

size_t ivmBufferLength(IvmBufferConst buf){ return buf->len; }

bool ivmBufferEnsureCapacity(IvmBuffer buf, size_t size){
	if(buf->len + size <= buf->cap) return true;

	auto newCapacity = std::max(buf->cap * 2, buf->cap + size);

	auto mem = allocMemory(newCapacity);
	if(!mem) return false;

	std::memcpy(mem, buf->ptr, buf->len);

	// TODO: handle failure
	freeMemory(buf->cap, buf->ptr);

	buf->ptr = mem;
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
	if(!makeMemoryExecutable(buf->len, buf->ptr)){
		return false;
	}

	buf->isWritable = false;
	return true;
}

bool ivmBufferMakeWritable(IvmBuffer buf){
	if(!makeMemoryWritable(buf->len, buf->ptr)){
		return false;
	}

	buf->isWritable = true;
	return true;
}

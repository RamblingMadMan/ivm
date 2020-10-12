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
#include <cstdio>
#include <new>

#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>

static inline size_t sysPageSize() noexcept{
	SYSTEM_INFO system_info;
    GetSystemInfo(&system_info);
    return system_info.dwPageSize;
}

static inline void *mapMem(size_t len){
	return VirtualAlloc(nullptr, len, MEM_COMMIT, PAGE_READWRITE);
}

static inline bool freeMem(size_t len, void *ptr){
	(void)len;
	return VirtualFree(ptr, 0, MEM_RELEASE);
}

static inline bool makeMemoryExecutable(size_t len, void *ptr){
	DWORD dummy;
	return VirtualProtect(ptr, len, PAGE_EXECUTE_READ, &dummy);
}

static inline bool makeMemoryWritable(size_t len, void *ptr){
	DWORD dummy;
	return VirtualProtect(ptr, len, PAGE_READWRITE, &dummy);
}

static inline const char *getMemError(){ return "error in VirtualAlloc or VirtualProtect"; }
#elif defined(__linux__)
#include <errno.h>
#include <sys/mman.h>
#include <unistd.h>

static inline size_t sysPageSize() noexcept{ return size_t(sysconf(_SC_PAGESIZE)); }

static inline void *mapMem(size_t len){
	auto ptr = mmap(nullptr, len, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
	if(ptr == MAP_FAILED) return nullptr;
	return ptr;
}

static inline bool unmapMem(size_t len, void *ptr){
	return munmap(ptr, len) == 0;
}

static inline bool makeMemoryExecutable(size_t len, void *mem){
	return mprotect(mem, len, PROT_EXEC) == 0;
}

static inline bool makeMemoryWritable(size_t len, void *mem){
	return mprotect(mem, len, PROT_READ | PROT_WRITE) == 0;
}

static inline const char *getMemError(){
	return strerror(errno);
}
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
	auto ptr = mapMem(capacity);
	if(!ptr){
		std::fprintf(stderr, "error allocating memory for buffer: %s\n", getMemError());
		return nullptr;
	}

	auto mem = std::malloc(sizeof(IvmBufferT));
	if(!mem) return nullptr;

	auto ret = new(mem) IvmBufferT;

	ret->ptr = ptr;
	ret->len = 0;
	ret->cap = capacity;
	ret->isWritable = true;

	return ret;
}

IvmBuffer ivmCreatePageBuffer(){
	auto pageSize = sysPageSize();
	return ivmCreateBuffer(pageSize);
}

void ivmDestroyBuffer(IvmBuffer buf){ unmapMem(buf->cap, buf->ptr); }

bool ivmBufferIsWritable(IvmBufferConst buf){ return buf->isWritable; }

bool ivmBufferIsExecutable(IvmBufferConst buf){ return !buf->isWritable; }

size_t ivmBufferCapacity(IvmBufferConst buf){ return buf->cap; }

size_t ivmBufferLength(IvmBufferConst buf){ return buf->len; }

bool ivmBufferEnsureCapacity(IvmBuffer buf, size_t size){
	if(buf->len + size <= buf->cap) return true;

	auto newCapacity = std::max(buf->cap * 2, buf->cap + size);

	auto newMap = mapMem(newCapacity);
	if(!newMap){
		std::fprintf(stderr, "error allocating memory: %s\n", getMemError());
		return false;
	}

	std::memcpy(newMap, buf->ptr, buf->len);

	// TODO: handle failure
	unmapMem(buf->cap, buf->ptr);

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

bool ivmBufferWrite(IvmBuffer buf, size_t len, const void *ptr){
	if(!ivmBufferEnsureCapacity(buf, len)) return false;
	std::memcpy(reinterpret_cast<char*>(buf->ptr) + buf->len, ptr, len);
	buf->len += len;
	return true;
}

bool ivmBufferWrite8(IvmBuffer buf, uint8_t bits){
	if(!ivmBufferEnsureCapacity(buf, 1)) return false;
	ivmBufferSet(buf, buf->len, bits);
	++buf->len;
	return true;
}

template<typename Val>
static inline bool ivmBufferWriteN(IvmBuffer buf, Val val){
	static uint8_t bytes[sizeof(Val)];

	for(size_t i = 0; i < sizeof(Val); i++){
		bytes[i] = (val >> (i * 8u)) & 0xFFu;
	}

	return ivmBufferWrite(buf, sizeof(bytes), bytes);
}

bool ivmBufferWrite16(IvmBuffer buf, uint16_t bits){
	return ivmBufferWriteN(buf, bits);
}

bool ivmBufferWrite32(IvmBuffer buf, uint32_t bits){
	return ivmBufferWriteN(buf, bits);
}

bool ivmBufferWrite64(IvmBuffer buf, uint64_t bits){
	return ivmBufferWriteN(buf, bits);
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

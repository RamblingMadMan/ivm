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

#ifndef IVM_BUFFER_H
#define IVM_BUFFER_H 1

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stddef.h>

/**
 * @defgroup Buffer Code buffers
 * @{
 */

typedef struct IvmBufferT *IvmBuffer;
typedef const struct IvmBufferT *IvmBufferConst;

IvmBuffer ivmCreateBuffer(size_t capacity);
IvmBuffer ivmCreatePageBuffer();

void ivmDestroyBuffer(IvmBuffer buf);

bool ivmBufferIsWritable(IvmBufferConst buf);
bool ivmBufferIsExecutable(IvmBufferConst buf);

size_t ivmBufferCapacity(IvmBufferConst buf);
size_t ivmBufferLength(IvmBufferConst buf);

bool ivmBufferEnsureCapacity(IvmBuffer buf, size_t size);

void *ivmBufferPtr(IvmBuffer buf);
const void *ivmBufferConstPtr(IvmBufferConst buf);

uint8_t ivmBufferGet(IvmBufferConst buf, size_t idx);
uint8_t ivmBufferSet(IvmBuffer buf, size_t idx, uint8_t bits);

bool ivmBufferWrite(IvmBuffer buf, size_t len, const void *ptr);
bool ivmBufferWrite8(IvmBuffer buf, uint8_t bits);
bool ivmBufferWrite16(IvmBuffer buf, uint16_t bits);
bool ivmBufferWrite32(IvmBuffer buf, uint32_t bits);
bool ivmBufferWrite64(IvmBuffer buf, uint64_t bits);

bool ivmBufferMakeExecutable(IvmBuffer buf);
bool ivmBufferMakeWritable(IvmBuffer buf);

/**
 * @}
 */

#ifdef __cplusplus
}
#endif

#endif // !IVM_BUFFER_H

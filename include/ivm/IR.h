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

#ifndef IVM_IR_H
#define IVM_IR_H 1

#include "Instr.h"
#include "Buffer.h"

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>

/**
 * @defgroup IR Intermediate language representation
 * An intermediate representation for language compilers to target.
 * @{
 */

/**
 * @defgroup Types Types
 * @{
 */

/**
 * @defgroup TypeTypes Types of Type
 * All types can be cast to ``IvmIRType``.
 * @{
 */

typedef const struct IvmIRTypeT *IvmIRType;
typedef const struct IvmIRIntTypeT* IvmIRIntType;
typedef const struct IvmIRNatTypeT* IvmIRNatType;

/**
 * @}
 */

/**
 * @brief Get the number of bits required to represent a type.
 * @param ty the type to query
 * @returns number of bits or 0 for an unrepresentable type
 */
size_t ivmIRTypeNumBits(IvmIRType ty);

/**
 * @brief Get a sized integer type.
 * @param numBits number of bits in integer type
 * @returns integer type
 */
IvmIRIntType ivmIRTypeInt(size_t numBits);

/**
 * @brief Get a sized natural type.
 * @param numBits number of bits in natural type
 * @returns natural type
 */
IvmIRNatType ivmIRTypeNat(size_t numBits);

/**
 * @}
 */

/**
 * @defgroup NodeTypes IR Node types
 * All node types can be cast to ``IvmIRNode``.
 */

typedef struct IvmIRNodeT *IvmIRNode;
typedef struct IvmIRLiteralT *IvmIRLiteral;
typedef struct IvmIRBinaryOpT *IvmIRBinaryOp;
typedef struct IvmIRBindingT *IvmIRBinding;
typedef struct IvmIRLambdaT *IvmIRLambda;
typedef struct IvmIRCallT *IvmIRCall;

/**
 * @}
 */

/**
 * @brief Destroy an IR node
 * @param node node to destroy
 */
void ivmDestroyIRNode(IvmIRNode node);

IvmIRType ivmIRNodeType(IvmIRNode node);

IvmIRLiteral ivmCreateLitInt32(int32_t val);
IvmIRLiteral ivmCreateLitNat32(uint32_t val);

/**
 * @brief Get the bit value of a literal.
 * @param lit literal to query
 * @returns value bits
 */
uint64_t ivmIRLiteralValue(IvmIRLiteral lit);

IvmIRBinaryOp ivmCreateIRAdd(IvmIRNode lhs, IvmIRNode rhs);
IvmIRBinaryOp ivmCreateIRSub(IvmIRNode lhs, IvmIRNode rhs);
IvmIRBinaryOp ivmCreateIRMul(IvmIRNode lhs, IvmIRNode rhs);
IvmIRBinaryOp ivmCreateIRDiv(IvmIRNode lhs, IvmIRNode rhs);

IvmIRBinding ivmCreateIRBinding(size_t len, const char *str, IvmIRNode val);

IvmIRLambda ivmCreateIRLambda(IvmIRType resultTy, size_t numParams, const IvmIRType *paramTys, size_t len, const IvmIRNode *body);

IvmIRCall ivmCreateIRCall(IvmIRNode fn, size_t numArgs, const IvmIRNode *args);

/**
 * @brief Compile IR nodes into IVM instructions.
 * After compilation
 * @param buf buffer to store the instructions in
 * @param len number of nodes
 * @param nodes pointer to the nodes
 * @returns 0 on success, otherwise an error is printed to ``stderr``.
 */
int ivmCompileIR(IvmBuffer buf, size_t len, const IvmIRNode *nodes);

/**
 * @}
 */

#ifdef __cplusplus
}
#endif

#endif // !IVM_IR_H

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
typedef const struct IvmIRVoidTypeT *IvmIRVoidType;
typedef const struct IvmIRBoolTypeT *IvmIRBoolType;
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
 * @brief Get the void type.
 * @returns void type
 */
IvmIRVoidType ivmIRTypeVoid();

/**
 * @brief Get the boolean type.
 * @return boolean type
 */
IvmIRBoolType ivmIRTypeBool();

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

typedef enum {
	IVM_IR_LOCAL,
	IVM_IR_LOAD,
	IVM_IR_STORE,
	IVM_IR_LITERAL,
	IVM_IR_BINARYOP,
	IVM_IR_UNARYOP,
	IVM_IR_BINDING,
	IVM_IR_PARAM,
	IVM_IR_LAMBDA,
	IVM_IR_FN,
	IVM_IR_BLOCK,
	IVM_IR_RETURN,
	IVM_IR_BRANCH,
	IVM_IR_CALL,

	IVM_IRNODETYPE_COUNT
} IvmIRNodeType;

typedef struct {
	IvmIRNodeType nodeType;
	IvmIRType type;
} IvmIRNode;

typedef const IvmIRNode *IvmIRNodePtr;

typedef struct {
	IvmIRNode base;
	uint64_t len;
} IvmIRLocalMem;

typedef const IvmIRLocalMem *IvmIRLocalMemPtr;

typedef struct {
	IvmIRLocalMem base;
	IvmIRLocalMemPtr mem;
	uint64_t offset;
} IvmIRLocalRef;

typedef struct {
	IvmIRNode base;
	IvmIRLocalMemPtr mem;
} IvmIRLoad;

typedef struct {
	IvmIRNode base;
	IvmIRLocalMemPtr mem;
	IvmIRNodePtr value;
} IvmIRStore;

typedef const IvmIRStore *IvmIRStorePtr;

typedef struct {
	IvmIRNode base;
	union {
		uint8_t n8;
		uint16_t n16;
		uint32_t n32;
		uint64_t n64;
		int8_t i8;
		int16_t i16;
		int32_t i32;
		int64_t i64;
	} value;
} IvmIRLiteral;

typedef const IvmIRLiteral *IvmIRLiteralPtr;

typedef enum {
	IVM_IRUNARYOP_INC,
	IVM_IRUNARYOP_DEC,
	IVM_IRUNARYOP_NOT,
	IVM_IRUNARYOP_BITNOT,

	IVM_IRUNARYOP_COUNT
} IvmIRUnaryOperator;

inline bool ivmUnaryOperatorIsLogic(IvmIRUnaryOperator op){
	return op == IVM_IRUNARYOP_NOT;
}

typedef struct {
	IvmIRNode base;
	IvmIRUnaryOperator op;
	IvmIRNodePtr value;
} IvmIRUnaryOp;

typedef const IvmIRUnaryOp *IvmIRUnaryOpPtr;

typedef enum {
	IVM_IRBINARYOP_ADD,
	IVM_IRBINARYOP_SUB,
	IVM_IRBINARYOP_MUL,
	IVM_IRBINARYOP_DIV,

	IVM_IRBINARYOP_LT,
	IVM_IRBINARYOP_GT,
	IVM_IRBINARYOP_LE,
	IVM_IRBINARYOP_GE,
	IVM_IRBINARYOP_EQ,
	IVM_IRBINARYOP_NE,

	IVM_IRBINARYOP_COUNT
} IvmIRBinaryOperator;

inline bool ivmBinaryOperatorIsLogic(IvmIRBinaryOperator op){
	return op < IVM_IRBINARYOP_COUNT && op > IVM_IRBINARYOP_DIV;
}

typedef struct {
	IvmIRNode base;
	IvmIRBinaryOperator op;
	IvmIRNodePtr lhs, rhs;
} IvmIRBinaryOp;

typedef const IvmIRBinaryOp *IvmIRBinaryOpPtr;

typedef struct {
	IvmIRNode base;
	size_t nameLen;
	const char *name;
	IvmIRNodePtr value;
} IvmIRBinding;

typedef const IvmIRBinding *IvmIRBindingPtr;

typedef struct {
	IvmIRLocalMem base;
} IvmIRParam;

typedef const IvmIRParam *IvmIRParamPtr;

typedef struct {
	IvmIRNode base;
	size_t len;
	const IvmIRNodePtr *body;
} IvmIRBlock;

typedef const IvmIRBlock *IvmIRBlockPtr;

typedef struct {
	IvmIRNode base;
	IvmIRType resultTy;
	size_t numParams;
	const IvmIRParam *params;
	IvmIRBlockPtr body;
} IvmIRLambda;

typedef const IvmIRLambda *IvmIRLambdaPtr;

typedef enum {
	IVM_IRLINK_EXPORT,
	IVM_IRLINK_IMPORT,
	IVM_IRLINK_INLINE,
	IVM_IRLINK_STATIC,

	IVM_IRLINKTYPE_COUNT
} IvmIRLinkType;

typedef struct {
	IvmIRLambda base;
	IvmIRLinkType linkType;
	size_t nameLen;
	const char *name;
} IvmIRFunction;

typedef const IvmIRFunction *IvmIRFunctionPtr;

typedef struct {
	IvmIRNode base;
	IvmIRNodePtr value;
} IvmIRReturn;

typedef const IvmIRReturn *IvmIRReturnPtr;

typedef struct {
	IvmIRNode base;
	IvmIRLambdaPtr lam;
	size_t numArgs;
	const IvmIRNodePtr *args;
} IvmIRCall;

typedef const IvmIRCall *IvmIRCallPtr;

typedef struct {
	IvmIRNode base;
	IvmIRNodePtr cond;
	IvmIRBlockPtr trueBlock, falseBlock;
} IvmIRBranch;

typedef const IvmIRBranch *IvmIRBranchPtr;

/**
 * @}
 */

/**
 * @brief Destroy an IR node
 * @param node node to destroy
 */

IvmIRType ivmIRNodeType(IvmIRNodePtr node);

IvmIRLocalMem ivmir_localN(IvmIRType type, uint64_t n);
IvmIRLocalMem ivmir_local(IvmIRType type);
IvmIRLoad ivmir_load(IvmIRLocalMemPtr mem);
IvmIRStore ivmir_store(IvmIRLocalMemPtr mem, IvmIRNodePtr val);

IvmIRLiteral ivmir_litInt32(int32_t val);
IvmIRLiteral ivmir_litNat32(uint32_t val);

/**
 * @brief Get the bit value of a literal.
 * @param lit literal to query
 * @returns value bits
 */
uint64_t ivmIRLiteralValue(IvmIRLiteral lit);

IvmIRBinaryOp ivmir_add(IvmIRNodePtr lhs, IvmIRNodePtr rhs);
IvmIRBinaryOp ivmir_sub(IvmIRNodePtr lhs, IvmIRNodePtr rhs);
IvmIRBinaryOp ivmir_mul(IvmIRNodePtr lhs, IvmIRNodePtr rhs);
IvmIRBinaryOp ivmir_div(IvmIRNodePtr lhs, IvmIRNodePtr rhs);

IvmIRBinaryOp ivmir_lt(IvmIRNodePtr lhs, IvmIRNodePtr rhs);
IvmIRBinaryOp ivmir_gt(IvmIRNodePtr lhs, IvmIRNodePtr rhs);
IvmIRBinaryOp ivmir_le(IvmIRNodePtr lhs, IvmIRNodePtr rhs);
IvmIRBinaryOp ivmir_ge(IvmIRNodePtr lhs, IvmIRNodePtr rhs);
IvmIRBinaryOp ivmir_eq(IvmIRNodePtr lhs, IvmIRNodePtr rhs);
IvmIRBinaryOp ivmir_ne(IvmIRNodePtr lhs, IvmIRNodePtr rhs);

IvmIRUnaryOp ivmir_inc(IvmIRNodePtr value);
IvmIRUnaryOp ivmir_dec(IvmIRNodePtr value);
IvmIRUnaryOp ivmir_not(IvmIRNodePtr value);

IvmIRBinding ivmCreateIRBinding(size_t len, const char *str, IvmIRNodePtr val);

IvmIRParam ivmir_param(IvmIRType type);

IvmIRFunction ivmir_fn(
	size_t nameLen, const char *name,
	IvmIRType resultTy, size_t numParams, const IvmIRParam *params,
	IvmIRBlockPtr body
);

IvmIRFunction ivmir_fnUndef(
	size_t nameLen, const char *name,
	IvmIRType resultTy, size_t numParams, const IvmIRParam *params
);

IvmIRLambda ivmir_lambdaUndef(IvmIRType resultTy, size_t numParams, const IvmIRParam *params);
IvmIRLambda ivmir_lambda(IvmIRType resultTy, size_t numParams, const IvmIRParam *params, IvmIRBlockPtr body);

IvmIRType IvmIRLambdaResultType(IvmIRLambda lam);

size_t ivmIRLambdaNumParams(IvmIRLambda lam);
IvmIRParamPtr ivmIRLambdaParam(IvmIRLambda lam, size_t idx);

bool ivmIRLambdaDef(IvmIRLambda *undefLam, IvmIRBlockPtr body);

IvmIRBlock ivmir_block(size_t len, const IvmIRNodePtr *body);

IvmIRReturn ivmir_return(IvmIRNodePtr value);

IvmIRBranch ivmir_branch(IvmIRNodePtr cond, IvmIRBlockPtr trueBlock, IvmIRBlockPtr falseBlock);

IvmIRCall ivmir_call(IvmIRLambdaPtr lam, size_t numArgs, const IvmIRNodePtr *args);

typedef struct IvmParseResult *IvmParseResultPtr;

/**
 * @brief Load IR in from a
 * @param len
 * @param str
 * @return
 */
IvmParseResultPtr ivmParseIR(size_t len, const char *str);

void ivmDestroyParseResult(IvmParseResultPtr res);

size_t ivmParseResultNumNodes(IvmParseResultPtr res);
const IvmIRNodePtr *ivmParseResultNodes(IvmParseResultPtr res);

typedef struct IvmCompileResult *IvmCompileResultPtr;

/**
 * @brief Compile IR nodes into IVM instructions.
 * After compilation
 * @param len number of nodes
 * @param nodes pointer to the nodes
 * @returns Buffer filled with instructions, otherwise `NULL` and an error is printed to `stderr`.
 */
IvmCompileResultPtr ivmCompileIR(size_t len, const IvmIRNodePtr *nodes);

void ivmDestroyCompileResult(IvmCompileResultPtr res);

size_t ivmCompileResultNumInstrs(IvmCompileResultPtr res);
const IvmInstr *ivmCompileResultInstrs(IvmCompileResultPtr res);

/**
 * @}
 */

#ifdef __cplusplus
}
#endif

#endif // !IVM_IR_H

cmake_minimum_required(VERSION 3.10.0 FATAL_ERROR)

project(ivm VERSION 0.1.0 LANGUAGES CXX C)

set(CMAKE_C_STANDARD 11)
set(CMAKE_CXX_STANDARD 17)

set(IVM_INCLUDE_DIR ${CMAKE_CURRENT_LIST_DIR}/include)

set(
	IVM_HEADERS
	${IVM_INCLUDE_DIR}/ivm/Buffer.h
	${IVM_INCLUDE_DIR}/ivm/Instr.h
	${IVM_INCLUDE_DIR}/ivm/IR.h
)

add_subdirectory(deps/sexi)

add_subdirectory(lib)
add_subdirectory(test)

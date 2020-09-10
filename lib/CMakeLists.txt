set(
	IVM_LIB_SOURCES
	Buffer.cpp
	Instr.cpp
)

add_library(ivm ${IVM_HEADERS} ${IVM_LIB_SOURCES})

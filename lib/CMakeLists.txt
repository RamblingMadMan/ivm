set(
	IVM_LIB_SOURCES
	Buffer.cpp
	Instr.cpp
	IR.cpp
	emit.hpp
	emitX64.cpp
)

add_library(ivm ${IVM_HEADERS} ${IVM_LIB_SOURCES})

target_include_directories(ivm PUBLIC ${IVM_INCLUDE_DIR})

target_link_libraries(ivm sexi)

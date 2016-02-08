cmake_minimum_required(VERSION 2.6)

#------------------------------------------------------------------------------
# Project variables

project(workoued)

set(WRKED_MAJOR_VERSION 0)
set(WRKED_MINOR_VERSION 0)
set(WRKED_PATCH_VERSION 1)
set(WRKED_VERSION
  "${WRKED_MAJOR_VERSION}.${WRKED_MINOR_VERSION}.${WRKED_PATCH_VERSION}")

#------------------------------------------------------------------------------
# Find packages

find_path(FIT_ROOT_DIR FitGen.exe ${PROJECT_BINARY_DIR})

#------------------------------------------------------------------------------
# Compiler-specific

if(CMAKE_COMPILER_IS_GNUCXX)
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wextra")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fno-gnu-keywords")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -pedantic")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -pipe")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c99")
endif(CMAKE_COMPILER_IS_GNUCXX)

#------------------------------------------------------------------------------
# Changeset detection

if(EXISTS ${PROJECT_SOURCE_DIR}/.hg)
  find_package(Hg)
  if(HG_FOUND)
    execute_process(COMMAND ${HG_EXECUTABLE} id --id
      WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
      OUTPUT_VARIABLE WRKED_VERSION_ID
      OUTPUT_STRIP_TRAILING_WHITESPACE)
  endif(HG_FOUND)
endif(EXISTS ${PROJECT_SOURCE_DIR}/.hg)

#------------------------------------------------------------------------------
# Targets

configure_file(
  ${PROJECT_SOURCE_DIR}/defs.h.in
  ${PROJECT_BINARY_DIR}/defs.h
  )
include_directories(${PROJECT_BINARY_DIR})

set(FIT_C_SRCS
  ${FIT_ROOT_DIR}/c/fit.c
  ${FIT_ROOT_DIR}/c/fit_convert.c
  ${FIT_ROOT_DIR}/c/fit_crc.c
  ${FIT_ROOT_DIR}/c/fit_product.c
  ${FIT_ROOT_DIR}/c/fit_ram.c
  )
include_directories(${FIT_ROOT_DIR}/c)

add_executable(workoued-ir2fit ir2fit.c ${FIT_C_SRCS})

#------------------------------------------------------------------------------
# Installation

install(TARGETS workoued-ir2fit DESTINATION bin)

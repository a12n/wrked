cmake_minimum_required(VERSION 2.6)

include(CheckIncludeFile)

#------------------------------------------------------------------------------
# Project variables

project(workoued)

set(WRKED_MAJOR_VERSION 0)
set(WRKED_MINOR_VERSION 0)
set(WRKED_PATCH_VERSION 7)
set(WRKED_VERSION
  "${WRKED_MAJOR_VERSION}.${WRKED_MINOR_VERSION}.${WRKED_PATCH_VERSION}")

#------------------------------------------------------------------------------
# Find packages

find_path(FIT_ROOT_DIR FitGen.exe ${PROJECT_BINARY_DIR})

check_include_file(stdint.h HAVE_STDINT_H)
if(HAVE_STDINT_H)
  add_definitions(-DFIT_USE_STDINT_H)
endif(HAVE_STDINT_H)

#------------------------------------------------------------------------------
# Compiler-specific

if(CMAKE_COMPILER_IS_GNUCXX)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wextra")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-gnu-keywords")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pedantic")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pipe")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
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

set(FIT_CXX_SRCS
  ${FIT_ROOT_DIR}/cpp/fit.cpp
  ${FIT_ROOT_DIR}/cpp/fit_crc.cpp
  ${FIT_ROOT_DIR}/cpp/fit_encode.cpp
  ${FIT_ROOT_DIR}/cpp/fit_field.cpp
  ${FIT_ROOT_DIR}/cpp/fit_field_definition.cpp
  ${FIT_ROOT_DIR}/cpp/fit_mesg.cpp
  ${FIT_ROOT_DIR}/cpp/fit_mesg_definition.cpp
  ${FIT_ROOT_DIR}/cpp/fit_profile.cpp
  ${FIT_ROOT_DIR}/cpp/fit_unicode.cpp
  )
include_directories(${FIT_ROOT_DIR}/c)
include_directories(${FIT_ROOT_DIR}/cpp)

add_executable(workoued-ir2fit ir2fit.cpp ${FIT_CXX_SRCS})

#------------------------------------------------------------------------------
# Installation

install(TARGETS workoued-ir2fit DESTINATION bin)

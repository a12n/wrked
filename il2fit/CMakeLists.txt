cmake_minimum_required(VERSION 2.6)

include(CheckIncludeFile)

#------------------------------------------------------------------------------
# Project variables

project(il2fit)

set(IL2FIT_MAJOR_VERSION 0)
set(IL2FIT_MINOR_VERSION 0)
set(IL2FIT_PATCH_VERSION 7)
set(IL2FIT_VERSION
  "${IL2FIT_MAJOR_VERSION}.${IL2FIT_MINOR_VERSION}.${IL2FIT_PATCH_VERSION}")

#------------------------------------------------------------------------------
# Build options

option(IL2FIT_WITH_TESTS
  "Build tests."
  OFF)

#------------------------------------------------------------------------------
# Find packages

find_path(FIT_ROOT_DIR FitGen.exe ${PROJECT_BINARY_DIR})

check_include_file(stdint.h HAVE_STDINT_H)
if(HAVE_STDINT_H)
  add_definitions(-DFIT_USE_STDINT_H)
endif(HAVE_STDINT_H)

#------------------------------------------------------------------------------
# Compiler-specific

if(${CMAKE_CXX_COMPILER_ID} STREQUAL "GNU")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-gnu-keywords")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pedantic")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pipe")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++1y")
  set_source_files_properties(il2fit.cpp COMPILE_FLAGS "-Wall -Wextra")
elseif(${CMAKE_CXX_COMPILER_ID} MATCHES "Clang")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++1y")
  set_source_files_properties(il2fit.cpp COMPILE_FLAGS "-Weverything")
endif(${CMAKE_CXX_COMPILER_ID} STREQUAL "GNU")

#------------------------------------------------------------------------------
# Changeset detection

if(EXISTS ${PROJECT_SOURCE_DIR}/.hg)
  find_package(Hg)
  if(HG_FOUND)
    execute_process(COMMAND ${HG_EXECUTABLE} id --id
      WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
      OUTPUT_VARIABLE IL2FIT_VERSION_ID
      OUTPUT_STRIP_TRAILING_WHITESPACE)
  endif(HG_FOUND)
endif(EXISTS ${PROJECT_SOURCE_DIR}/.hg)

#------------------------------------------------------------------------------
# Targets

set(FIT_CXX_SRCS
  ${FIT_ROOT_DIR}/cpp/fit.cpp
  ${FIT_ROOT_DIR}/cpp/fit_crc.cpp
  ${FIT_ROOT_DIR}/cpp/fit_date_time.cpp
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

add_executable(il2fit il2fit.cpp ${FIT_CXX_SRCS})

if(IL2FIT_WITH_TESTS)
  file(DOWNLOAD
    https://raw.githubusercontent.com/philsquared/Catch/v1.3.3/single_include/catch.hpp
    ${PROJECT_BINARY_DIR}/catch.hpp
    EXPECTED_MD5 "f21d005ecf1e5c576d4f8abad0a08ace" SHOW_PROGRESS
    )
  include_directories(${PROJECT_BINARY_DIR})
  add_executable(il2fit-test il2fit.cpp ${FIT_CXX_SRCS})
  set_target_properties(il2fit-test PROPERTIES
    COMPILE_DEFINITIONS "_WITH_TESTS=1")
endif(IL2FIT_WITH_TESTS)

#------------------------------------------------------------------------------
# Installation

install(TARGETS il2fit DESTINATION bin)

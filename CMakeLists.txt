cmake_minimum_required(VERSION 3.24)

project(
  FH_TABLE
  VERSION 1.0.1
  LANGUAGES Fortran)

option(BUILD_SHARED_LIBS "Build using shared libraries" ON)
option(BUILD_TESTS "Build test executables" ON)
# If possible, use the -DNonstandard flag, as long as the tests pass. It will be significantly faster, but 
# depends on non-Fortran-standard-compliant behavior for integer overflow
option(NON_STANDARD_WRAP "Use non-standard-compliant wrapping behavior for overflow" ON)

if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fpp -real-size:64 -integer-size:64 -warn")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cpp -fdefault-integer-8 -fdefault-real-8 -Wall -fno-range-check")
endif()

if(NON_STANDARD_WRAP)
  if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
      set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -DNonstandard -fno-strict-overflow")
  elseif(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
      set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -DNonstandard -fwrapv")
  endif()
endif()

add_subdirectory(src)

if(BUILD_TESTS)
  add_subdirectory(test)
endif()
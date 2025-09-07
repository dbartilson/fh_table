cmake_minimum_required(VERSION 3.24)

project(
  FH_TABLE
  VERSION 1.0.0
  LANGUAGES Fortran)

option(BUILD_SHARED_LIBS "Build using shared libraries" ON)
option(BUILD_TESTS "Build test executables" ON)

# If possible, use the -DNonstandard flag, as long as the tests pass. It will be significantly faster, but 
# depends on non-Fortran-standard-compliant behavior for integer overflow
if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fpp -real-size:64 -integer-size:64 -warn -DNonstandard")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cpp -fdefault-integer-8 -fdefault-real-8 -Wall")
endif()

add_library(fh_table hash_functions.f90 hash_table_module.f90)

if(BUILD_SHARED_LIBS AND WIN32)
  set_target_properties(fh_table PROPERTIES WINDOWS_EXPORT_ALL_SYMBOLS TRUE)
endif()

install(TARGETS fh_table)
install(FILES 
        ${CMAKE_CURRENT_BINARY_DIR}/hash_functions.mod 
        ${CMAKE_CURRENT_BINARY_DIR}/hash_table_module.mod
        DESTINATION include)

if(BUILD_TESTS)
  add_executable(fh_functionality_test functionality_test.f90)
  target_link_libraries(fh_functionality_test PRIVATE fh_table)
  install(TARGETS fh_functionality_test)

  add_executable(fh_speed_test speed_test.f90)
  target_link_libraries(fh_speed_test PRIVATE fh_table)
  install(TARGETS fh_speed_test)
endif()
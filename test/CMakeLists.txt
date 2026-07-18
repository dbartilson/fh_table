enable_testing()

add_executable(fh_functionality_test functionality_test.f90)
target_link_libraries(fh_functionality_test PUBLIC fh_table)
add_test(NAME fh_functionality_test COMMAND fh_functionality_test)

add_executable(fh_speed_test speed_test.f90)
target_link_libraries(fh_speed_test PUBLIC fh_table)
add_test(NAME fh_speed_test COMMAND fh_speed_test)
# Prevent the test from running by default
set_tests_properties(fh_speed_test PROPERTIES DISABLED TRUE)

if(WIN32)
  get_filename_component(COMPILER_DIR ${CMAKE_Fortran_COMPILER} DIRECTORY)
  set_tests_properties(fh_functionality_test fh_speed_test PROPERTIES
      ENVIRONMENT_MODIFICATION "PATH=path_list_append:$<TARGET_FILE_DIR:fh_table>"
  )
endif()
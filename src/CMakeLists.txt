add_library(fh_table hash_functions.f90 hash_table_module.f90)

if(BUILD_SHARED_LIBS AND WIN32)
  set_target_properties(fh_table PROPERTIES WINDOWS_EXPORT_ALL_SYMBOLS TRUE)
endif()

set_target_properties(fh_table PROPERTIES
    VERSION   "${PROJECT_VERSION}"
    SOVERSION "${PROJECT_VERSION_MAJOR}"
)
set_target_properties(fh_table PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod)

# making LIB_DIR/mod available for libraries linking LIB 
target_include_directories(fh_table INTERFACE ${CMAKE_CURRENT_BINARY_DIR}/mod)

install(TARGETS fh_table)
install(FILES 
        ${CMAKE_CURRENT_BINARY_DIR}/hash_functions.mod 
        ${CMAKE_CURRENT_BINARY_DIR}/hash_table_module.mod
        DESTINATION include)
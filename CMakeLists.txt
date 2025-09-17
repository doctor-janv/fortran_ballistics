# Required minimum version of cmake
cmake_minimum_required(VERSION 3.20.0 FATAL_ERROR)

# CMake requires that you declare a project in the top level CMakeList
set(PROJECT_NAME forbal)
project(${PROJECT_NAME} LANGUAGES Fortran)

# Set all the sources
file (GLOB_RECURSE SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/src/*.F90")

# Add executable
add_executable(${PROJECT_NAME}.x ${SOURCES})

# Setup output
set_target_properties( ${PROJECT_NAME}.x
        PROPERTIES
        ARCHIVE_OUTPUT_DIRECTORY "${PROJECT_SOURCE_DIR}/lib"
        LIBRARY_OUTPUT_DIRECTORY "${PROJECT_SOURCE_DIR}/lib"
        RUNTIME_OUTPUT_DIRECTORY "${PROJECT_SOURCE_DIR}/bin"
        )

# |------------ Write Makefile to root directory
file(WRITE ${PROJECT_SOURCE_DIR}/Makefile "subsystem:\n" "\t$(MAKE) -C forbal_build \n\n"
        "clean:\n\t$(MAKE) -C forbal_build clean\n")


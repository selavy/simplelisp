cmake_minimum_required(VERSION 3.5)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/modules")

add_library(Flags INTERFACE)
target_compile_options(Flags INTERFACE
    -Wall
    -Werror
    -march=native
)

message("CMAKE_MODULE_PATH = ${CMAKE_MODULE_PATH}")

find_package(Readline REQUIRED)

add_executable(lispy lispy.c)
target_link_libraries(lispy PUBLIC Flags ${Readline_LIBRARY})
include_directories(lispy PUBLIC ${Readline_INCLUDE_DIR})
set_target_properties(lispy PROPERTIES C_STANDARD 11)

configure_file(library.lisp "${CMAKE_CURRENT_BINARY_DIR}/library.lisp" COPYONLY)

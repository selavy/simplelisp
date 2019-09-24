cmake_minimum_required(VERSION 3.5)

add_library(Flags INTERFACE)
target_compile_options(Flags INTERFACE
    -Wall
    -Werror
    -march=native
)

add_executable(lispy lispy.c)
target_link_libraries(lispy Flags)
set_target_properties(lispy PROPERTIES C_STANDARD 11)

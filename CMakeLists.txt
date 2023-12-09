cmake_minimum_required(VERSION 3.17)
project(Atari_Emulator)

# Include Google Test
find_package(GTest REQUIRED)
include_directories(${GTEST_INCLUDE_DIRS})

set(CMAKE_CXX_STANDARD 20)

include_directories(.)
include_directories(include)

add_executable(Atari_Emulator
        include/cpu.h
        src/cpu.cpp
        main.cpp
        include/memory.h
        src/memory.cpp
        tests/CpuTests.cpp
        include/tia.h
        src/tia.cpp)

# Link Google Test to your executable
target_link_libraries(Atari_Emulator ${GTEST_LIBRARIRIES} pthread)

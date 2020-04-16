# vim: se fdm=marker
cmake_minimum_required(VERSION 3.5)
set(CMAKE_EXPORT_COMPILE_COMMANDS TRUE)

#ADD_LIBRARY(test.exe SHARED test.cpp)

SET(BOOST_DIR /home/sehe/custom/boost_1_70_0)
SET(CMAKE_C_COMPILER gcc-8)

ADD_DEFINITIONS(-DDEBUG -DSTANDALONE)

SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -isystem ${BOOST_DIR}")
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -pedantic") # -Wconversion
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++17")

SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3 -pthread -march=native")
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-var-tracking-assignments")
#SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -flto")
#SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=address,undefined")
#SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -ggdb -fno-omit-frame-pointer -O0 -pthread -march=native")

FILE(GLOB CHECK_AST_CPPS check_ast_??.cpp)
ADD_EXECUTABLE(test.exe test.cpp ast.cpp ast_gen.cpp parser.cpp
    #${CHECK_AST_CPPS}
)

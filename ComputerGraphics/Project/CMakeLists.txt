cmake_minimum_required(VERSION 2.8)
project(CG)

add_executable(app src/main.cpp)

add_subdirectory(libs)
find_package(glfw3 3.3 REQUIRED)
find_package(assimp REQUIRED)

target_include_directories(app PRIVATE src ${GLM_INCLUDE_DIR})
#target_include_directories(app PRIVATE src ${ASSIMP_INCLUDE_DIRS})
target_link_libraries(app glad glfw stb assimp json ${CMAKE_DL_LIBS})





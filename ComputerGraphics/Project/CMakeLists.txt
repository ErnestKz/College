cmake_minimum_required(VERSION 3.2)
project(CG)

set(CMAKE_CXX_FLAGS "-std=c++11")
add_executable(app src/main.cpp)

add_subdirectory(libs)
find_package(glfw3 3.3 REQUIRED)

find_package(assimp REQUIRED)
message("xxx ${ASSIMP_LIBRARIES}")
find_library(assimp_LIBRARIES NAMES assimp)
message("yyy ${assimp_LIBRARIES}")

target_include_directories(app PRIVATE src ${GLM_INCLUDE_DIR})

target_link_libraries(app glad glfw stb
#assimp
${assimp_LIBRARIES}
json ${CMAKE_DL_LIBS})





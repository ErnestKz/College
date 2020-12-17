cmake_minimum_required(VERSION 3.0.0)
project(Computer_Vision_Assignment2)

find_package(OpenCV REQUIRED)

add_executable(app src/main.cpp)
add_executable(show_frame src/show_frame.cpp)
add_executable(supa src/supa.cpp)
add_executable(metric src/metrics.cpp)


target_include_directories(app PRIVATE ${OpenCV_INCLUDE_DIRS})
target_link_libraries(app PRIVATE ${OpenCV_LIBS})

target_include_directories(show_frame PRIVATE ${OpenCV_INCLUDE_DIRS})
target_link_libraries(show_frame PRIVATE ${OpenCV_LIBS})

target_include_directories(supa PRIVATE ${OpenCV_INCLUDE_DIRS})
target_link_libraries(supa PRIVATE ${OpenCV_LIBS})


message("OpenCV Include Directories: "${OpenCV_INCLUDE_DIRS})
message("OpenCV Libraries "${OpenCV_LIBS})

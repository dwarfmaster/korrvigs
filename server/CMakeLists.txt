cmake_minimum_required(VERSION 3.15)
project(problem CXX)

find_package(Boost 1.79.0
  REQUIRED
  COMPONENTS url program_options iostreams
)

add_library(project_options INTERFACE)                       
target_compile_features(project_options INTERFACE cxx_std_17)
add_subdirectory(src)

cmake_minimum_required(VERSION 3.15)
project(problem CXX)

find_package(Boost 1.79.0
  REQUIRED
  COMPONENTS url
)
include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/FindSouffle.cmake)
configure_souffle()

add_library(project_options INTERFACE)                       
target_compile_features(project_options INTERFACE cxx_std_17)
add_subdirectory(src)

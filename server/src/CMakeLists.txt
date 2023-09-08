
set(KORR_SRC
  main.cpp
  datalog.cpp
)
set(KORR_HEADERS
  parser.hpp
  parser_impl.hpp
  datalog.hpp
)

add_executable(korrvigs-server ${KORR_SRC} ${KORR_HEADERS})
set_property(
  TARGET korrvigs-server
  PROPERTY CXX_STANDARD 17)
target_include_directories(korrvigs-server PUBLIC ${Boost_INCLUDE_DIR})
install(TARGETS korrvigs-server DESTINATION bin)

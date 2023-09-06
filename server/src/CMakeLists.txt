
set(KORR_SRC
  main.cpp
)
set(KORR_HEADERS
)

add_executable(korrvigs-server ${KORR_SRC} ${KORR_HEADERS})
target_include_directories(korrvigs-server PUBLIC ${Boost_INCLUDE_DIR})
install(TARGETS korrvigs-server DESTINATION bin)

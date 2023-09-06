
set(KORR_SRC
  main.cpp
)
set(KORR_HEADERS
)

add_executable(korrvigs-server ${KORR_SRC} ${KORR_HEADERS})
install(TARGETS korrvigs-server DESTINATION bin)

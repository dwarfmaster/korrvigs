# Taken from https://github.com/insieme/insieme/blob/master/cmake/add_souffle.cmake

macro(configure_souffle)
	# TODO find_package(SOUFFLE REQUIRED)
  if(NOT SOUFFLE_ROOT AND DEFINED ENV{SOUFFLE_ROOT})
    set(SOUFFLE_ROOT "$ENV{SOUFFLE_ROOT}" CACHE PATH "Souffle base directory location")
	endif()
	if(NOT SOUFFLE_ROOT)
		message(WARNING "Souffle NOT found")
	endif()

	# Find Soufflé static header files
  if(NOT SOUFFLE_HEADER_FILES)
    set(SOUFFLE_HEADER_FILES "${SOUFFLE_ROOT}/include" CACHE PATH "Souffle header directory")
  endif()
	# TODO attach to relevant targets
	# include_directories(SYSTEM ${souffle_header_files})

	# Find the Soufflé binary
  if(NOT SOUFFLE_BINARY)
    set(SOUFFLE_BINARY "${SOUFFLE_ROOT}/bin/souffle" CACHE PATH "Souffle binary")
  endif()

endmacro(configure_souffle)

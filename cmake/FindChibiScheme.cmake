# - Try to find Chibi Scheme
#
# This will defined in the end:
#
#     CHIBISCHEME_FOUND        - Chibi Scheme was found
#     CHIBISCHEME_INCLUDE_DIRS - Chibi Scheme combined includes
#     CHIBISCHEME_LIBRARIES    - Chibi Scheme combined libraries

# Poke for pkg-config

include(FindPkgConfig)

if(PKG_CONFIG_FOUND)
	pkg_check_modules(PC_CHIBISCHEME QUIET chibi-scheme)
endif(PKG_CONFIG_FOUND)

# Look for the include directory

find_path(CHIBISCHEME_INCLUDE_DIR
	NAMES chibi/eval.h chibi/sexp.h
	HINTS ${PC_CHIBISCHEME_INCLUDE_DIRS})

# Look for the libraries

find_library(CHIBISCHEME_LIBRARY
	NAMES chibi-scheme
	HINTS ${PC_CHIBISCHEME_LIBRARY_DIRS})

# Check and set output variables

include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(ChibiScheme
	"Chibi Scheme not found"
	CHIBISCHEME_INCLUDE_DIR CHIBISCHEME_LIBRARY)

set(CHIBISCHEME_INCLUDE_DIRS ${CHIBISCHEME_INCLUDE_DIR})
set(CHIBISCHEME_LIBRARIES ${CHIBISCHEME_LIBRARY})

mark_as_advanced(CHIBISCHEME_INCLUDE_DIRS CHIBISCHEME_LIBRARIES)

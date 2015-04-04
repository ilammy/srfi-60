cmake_minimum_required(VERSION 2.6)

project(native-implementation C)

set(library_name native-implementation)
set(impl_directory "srfi-60/srfi/60")

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} cmake)

find_package(ChibiScheme MODULE REQUIRED)

include_directories(${CHIBISCHEME_INCLUDE_DIRS})

add_library(${library_name} SHARED
	${impl_directory}/native-implementation.c)

target_link_libraries(${library_name}
	${CHIBISCHEME_LIBRARIES})

set_target_properties(${library_name} PROPERTIES
	PREFIX ""
	LIBRARY_OUTPUT_DIRECTORY ${impl_directory})

cmake_minimum_required(VERSION 3.0.0)

project(Coupling_project VERSION 1.0.0)

# CONFIGURE FORTRAN COMPILER

enable_language(Fortran)
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modFiles)

# Add compilers flags
#
# -Wno-surprising flag is enabled to suppress a buggy warning about final
# subroutine it should be periodicly removed to see if other surprising
# warining are not raised This buggy warning should be repaired in GCC 7.0
#
# -flto (Link Time Optimisation) is SUPER important for performance.
#Allows to inline function across diffrent source files (and modules)
#

# Set core Fortran Compiler flags -> Will be used in all files
set(CMAKE_Fortran_FLAGS "-std=f2008 -O3 -g -pedantic -Wall -Wno-surprising -Wno-unused-dummy-argument")

add_library(
io
serpent_procedures.f90
cobraen_procedures.f90
results.f90
)

add_executable(
matcom
main.f90
)

target_link_libraries(matcom io)

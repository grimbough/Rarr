dnl Process this file with autoconf to produce a configure script.
dnl author: Mike Smith (grimbough@gmail.com)

AC_PREREQ([2.65])
AC_INIT([Rarr], [0.0.0.9000], [grimbough@gmail.com])
AC_CONFIG_SRCDIR([src])

m4_include([tools/ax_gcc_x86_cpu_supports.m4])
m4_include([tools/ax_check_compile_flag.m4])

: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi

AC_SUBST(CC, `"${R_HOME}/bin/R" CMD config CC`)
AC_SUBST(CXX, `"${R_HOME}/bin/R" CMD config CXX`)
AC_SUBST(CFLAGS, `"${R_HOME}/bin/R" CMD config CFLAGS`)
AC_SUBST(CXXFLAGS, `"${R_HOME}/bin/R" CMD config CXXFLAGS`)
AC_SUBST(CPICFLAGS, `"${R_HOME}/bin/R" CMD config CPICFLAGS`)
AC_SUBST(CXXPICFLAGS, `"${R_HOME}/bin/R" CMD config CXXPICFLAGS`)
AC_SUBST(CPPFLAGS, `"${R_HOME}/bin/R" CMD config CPPFLAGS`)
AC_SUBST(MAKE, `"${R_HOME}/bin/R" CMD config MAKE`)
AC_SUBST(RANLIB, `"${R_HOME}/bin/R" CMD config RANLIB`)

dnl seems unreliable as to whether this information is available
AR=`"${R_HOME}/bin/R" CMD config AR`
AS_IF([test "x$AR" = "xERROR: no information for variable 'AR'"], [AC_PATH_PROG([AR], [ar])], [])
AC_SUBST(AR)

R_CPPFLAGS=`"${R_HOME}/bin/R" CMD config --cppflags`

if [[ -z "$R_CPPFLAGS" ]]; then
  R_CPPFLAGS=""
  NO_RLIB="-DNO_RLIB"
else
  NO_RLIB=""
fi

AC_SUBST(R_CPPFLAGS)
AC_SUBST(NO_RLIB)

AC_CONFIG_FILES([src/Makevars])

##############################

AC_ARG_WITH(bundled-libraries,,
  [
    bundled_libs="yes"
    AC_MSG_NOTICE([Forcing the use of bundled compression libraries])
  ],
  [
    bundled_libs="no"
  ])

dnl we will use the bundled compression libraries if: requested OR this is the BBS
AS_IF([test "x$bundled_libs" = xyes],
      [AC_MSG_NOTICE(requesting bundled libs)],
      [test "${IS_BIOC_BUILD_MACHINE}" = "true"],
      [AC_MSG_NOTICE(is Bioc build machine)],
      [
        AC_MSG_NOTICE(is NOT Bioc build machine)
        HAVE_ZSTD="yes"
        HAVE_BLOSC="yes"
      ])
      
##############################
AC_MSG_NOTICE(configuring the Zstd filter...)

if test "x$HAVE_ZSTD" = "xyes"; then
  AC_CHECK_HEADERS([zstd.h], [HAVE_ZSTD_H="yes"], [unset HAVE_ZSTD])
  if test "x$HAVE_ZSTD_H" = "xyes"; then
    AC_CHECK_LIB([zstd], [ZSTD_decompress],, [unset HAVE_ZSTD])
  fi
fi

if test -z "$HAVE_ZSTD"; then
  AC_MSG_NOTICE(compiling bundled Zstd library)
  AC_SUBST(BUILD_ZSTD, libzstd.a)
  AC_SUBST(ZSTD_LIB, compression_tools/zstd/libzstd.a)
else
  AC_MSG_NOTICE(Using system Zstd library)
  AC_SUBST(BUILD_ZSTD, "")
  AC_SUBST(ZSTD_LIB, -lzstd)
fi

AC_SUBST(BUILD_ZSTD)
AC_SUBST(ZSTD_LIB)

##############################

AC_MSG_NOTICE(configuring the BLOSC filter...)

if test "x$HAVE_BLOSC" = "xyes"; then
  AC_CHECK_HEADERS([blosc.h], [HAVE_BLOSC_H="yes"], [unset HAVE_BLOSC])
  if test "x$HAVE_BLOSC_H" = "xyes"; then
    AC_CHECK_LIB([blosc], [blosc_compress],, [unset HAVE_BLOSC])
  fi
fi

dnl if there's not a system install of blosc build our own version

if test -z "$HAVE_BLOSC"; then

  AC_MSG_NOTICE(compiling bundled BLOSC library)

  ## check if we have an x86 CPU
  machine=`uname -m`
  AS_CASE([$machine],
    [*86*], [x86_CPU=true],
    [x86_CPU=false]
  )
  
  ## see if -msse2 is a supported flag
  AX_CHECK_COMPILE_FLAG([-msse2], [MSSE2=true], [MSSE2=false])
  
  IS_BIOC_BUILD_MACHINE=${IS_BIOC_BUILD_MACHINE:-false}
  
  if [ "${IS_BIOC_BUILD_MACHINE}" == true ]; then
    AC_MSG_NOTICE(Bioc build machine; disabling MAVX2)
    MAVX2=false
  else
    ## see if -mavx2 is a supported flag
    AX_CHECK_COMPILE_FLAG([-mavx2], [MAVX2=true], [MAVX2=false])
  fi
  
  AC_RUN_IFELSE(
        [AC_LANG_PROGRAM([#include <stdlib.h>],
          [__builtin_cpu_init ();])
        ],
        [ax_cv_gcc_check_x86_cpu_init=true],
        [ax_cv_gcc_check_x86_cpu_init=false])
  
  ## check CPU supports sse2 and avx2
  if [ "$x86_CPU" == true ] && [ "$ax_cv_gcc_check_x86_cpu_init" == true ]; then
      AX_GCC_X86_CPU_SUPPORTS(sse2, [SSE2_SUPPORTED=true], [SSE2_SUPPORTED=false])
      if [ "$MAVX2" = true ]; then
        AX_GCC_X86_CPU_SUPPORTS(avx2, [AVX2_SUPPORTED=true], [AVX2_SUPPORTED=false])
      fi
  else
      SSE2_SUPPORTED=false
      AVX2_SUPPORTED=false
  fi
  
  if [ "$MSSE2" = true ] && [ "$SSE2_SUPPORTED" = true ] ; then
    SSE2="-DSHUFFLE_SSE2_ENABLED"
    MSSE2="-msse2"
    SSE2_BITSHUFFLE='bitshuffle-sse2.o: bitshuffle-sse2.c
	$(CC) $(FLAGS) -c bitshuffle-sse2.c'
    SSE2_BITSHUFFLE_OBJ='bitshuffle-sse2.o'
    SSE2_SHUFFLE='shuffle-sse2.o: shuffle-sse2.c
	$(CC) $(FLAGS) -c shuffle-sse2.c'
    SSE2_SHUFFLE_OBJ='shuffle-sse2.o'
    SSE2_OBJ_PATH='lib/blosc-1.20.1/'
  else
    SSE2=""
    MSSE2=""
    SSE2_BITSHUFFLE=""
    SSE2_SHUFFLE=""
    SSE2_BITSHUFFLE_OBJ=""
    SSE2_SHUFFLE_OBJ=""
    SSE2_OBJ_PATH=""
  fi
  
  AC_SUBST(SSE2)
  AC_SUBST(MSSE2)
  AC_SUBST(SSE2_BITSHUFFLE)
  AC_SUBST(SSE2_SHUFFLE)
  AC_SUBST(SSE2_BITSHUFFLE_OBJ)
  AC_SUBST(SSE2_SHUFFLE_OBJ)
  AC_SUBST(SSE2_OBJ_PATH)
  
  if [ "$MAVX2" = true ] && [ "$AVX2_SUPPORTED" = true ] ; then
    AVX2="-DSHUFFLE_AVX2_ENABLED"
    MAVX2="-mavx2"
    AVX2_BITSHUFFLE='bitshuffle-avx2.o: bitshuffle-avx2.c
	$(CC) $(FLAGS) -c bitshuffle-avx2.c'
    AVX2_BITSHUFFLE_OBJ='bitshuffle-avx2.o'
    AVX2_SHUFFLE='shuffle-avx2.o: shuffle-avx2.c
	$(CC) $(FLAGS) -c shuffle-avx2.c'
    AVX2_SHUFFLE_OBJ='shuffle-avx2.o'
    AVX2_OBJ_PATH='lib/blosc-1.20.1/'
  else
    AVX2=""
    MAVX2=""
    AVX2_BITSHUFFLE=""
    AVX2_SHUFFLE=""
    AVX2_BITSHUFFLE_OBJ=""
    AVX2_SHUFFLE_OBJ=""
    AVX2_OBJ_PATH=""
  fi
  
  AC_SUBST(AVX2)
  AC_SUBST(MAVX2)
  AC_SUBST(AVX2_BITSHUFFLE)
  AC_SUBST(AVX2_SHUFFLE)
  AC_SUBST(AVX2_BITSHUFFLE_OBJ)
  AC_SUBST(AVX2_SHUFFLE_OBJ)
  AC_SUBST(AVX2_OBJ_PATH)
  
  AC_SUBST(BUILD_BLOSC, "libblosc.a")
  AC_SUBST(BLOSC_INCLUDE, "-I./lib/blosc-1.20.1")
  AC_SUBST(BLOSC_LIB, "./compression_tools/blosc/libblosc.a")
else
  AC_MSG_NOTICE(Using system BLOSC library)
  AC_SUBST(BUILD_BLOSC, "")
  AC_SUBST(BLOSC_INCLUDE, "")
  AC_SUBST(BLOSC_LIB, -lblosc)
fi

AC_CONFIG_FILES([src/compression_tools/blosc/lib/blosc-1.20.1/Makefile src/compression_tools/blosc/Makefile src/compression_tools/blosc/lib/snappy-1.1.1/Makefile])
AC_OUTPUT

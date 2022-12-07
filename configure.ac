dnl Process this file with autoconf to produce a configure script.
dnl author: Mike Smith (grimbough@gmail.com)

AC_PREREQ([2.65])
AC_INIT([Rarr], [0.0.0.9000], [grimbough@gmail.com])
AC_CONFIG_SRCDIR([src])

: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi

AC_SUBST(CC, `"${R_HOME}/bin/R" CMD config CC`)
AC_SUBST(CFLAGS, `"${R_HOME}/bin/R" CMD config CFLAGS`)
AC_SUBST(CPICFLAGS, `"${R_HOME}/bin/R" CMD config CPICFLAGS`)
AC_SUBST(CXXPICFLAGS, `"${R_HOME}/bin/R" CMD config CXXPICFLAGS`)


dnl seems unreliable as to whether this information is available
AR=`"${R_HOME}/bin/R" CMD config AR`
AS_IF([test "x$AR" = "xERROR: no information for variable 'AR'"], [AC_PATH_PROG([AR], [ar])], [])
AC_SUBST(AR)



AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT

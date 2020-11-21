# -*- Autoconf -*-
#
#   Copyright (C) 2020  Ahmet Artu Yildirim
#
#   orca is free software: you can redistribute it and/or modify
#   it under the terms of the GNU Lesser General Public License as
#   published by the Free Software Foundation, either version 3 of
#   the License, or (at your option) any later version.
#
#   orca is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU Lesser General Public License for more details.
#
#   You should have received a copy of the GNU Lesser General Public License
#   along with orca. If not, see <https://www.gnu.org/licenses/>.

AC_INIT(guile-orca, 0.1.0)
AC_CONFIG_SRCDIR(orca)
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([color-tests -Wall -Wno-portability foreign])
AC_CONFIG_MACRO_DIRS([m4])

GUILE_PKG([3.0 2.2 2.0])
GUILE_PROGS
GUILE_SITE_DIR

if test "x$GUILD" = "x"; then
   AC_MSG_ERROR(['guile' binary not found; please check your guile-2.x installation.])
fi

AC_ARG_WITH([libmpi-path],
  [AS_HELP_STRING([--with-libmpi-path=PATH], [PATH of mpi dynamic library])],
  [LIBMPI_PATH="$withval"],
  [LIBMPI_PATH="libmpich.so"])

AC_MSG_CHECKING([for libmpi shared library path])
AC_MSG_RESULT([$LIBMPI_PATH])
AC_SUBST([LIBMPI_PATH])

AC_CONFIG_FILES([Makefile orca/config.scm])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])

AC_OUTPUT

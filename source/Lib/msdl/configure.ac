#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.52)
AC_INIT(msdl, 1.2.7-r2, me_t_ro@yahoo.com)
AM_INIT_AUTOMAKE
AC_CONFIG_SRCDIR([src/msdl.c])
AC_CONFIG_HEADER([config.h])

# Checks for programs.
AC_PROG_CC

# Checks for libraries.

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([arpa/inet.h fcntl.h inttypes.h netdb.h netinet/in.h stdlib.h string.h unistd.h sys/socket.h sys/time.h ])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_INLINE
AC_TYPE_SIZE_T
AC_HEADER_TIME
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT64_T
AC_TYPE_UINT8_T

# Checks for library functions.
AC_FUNC_MALLOC
AC_FUNC_MEMCMP
AC_FUNC_REALLOC
AC_FUNC_SELECT_ARGTYPES
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([inet_ntoa memmove memset select socket strcasecmp strchr strdup strncasecmp strrchr strstr])

AC_CONFIG_FILES([Makefile
                 src/Makefile
		 doc/Makefile
		 doc/msdl.1])
AC_OUTPUT

echo "
Configured msdl:
  Compiler:                  ${CC}
  Compiler flags:            ${CFLAGS}

Please run 'make' to compile, and 'make install' to install.
"

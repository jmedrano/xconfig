PHP_ARG_ENABLE(xconfig, whether to enable my extension,
[ --enable-xconfig   Enable my extension])
 
if test "$PHP_XCONFIG" = "yes"; then
  PHP_REQUIRE_CXX()
  PHP_SUBST(XCONFIG_SHARED_LIBADD)
  PHP_ADD_LIBRARY(stdc++, 1, XCONFIG_SHARED_LIBADD)
  PHP_ADD_LIBRARY(xconfig, 1, XCONFIG_SHARED_LIBADD)
  CPPFLAGS="$CPPFLAGS -std=c++0x -Wall"
  PHP_SUBST(CPPFLAGS)
  AC_DEFINE(HAVE_XCONFIG, 1, [Whether you have my extension])
  PHP_NEW_EXTENSION(xconfig, xconfig.cpp, $ext_shared)
fi

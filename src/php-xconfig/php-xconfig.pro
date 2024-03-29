SOURCES_COPYM4 = config.m4.in
copym4.input = SOURCES_COPYM4
copym4.target = config.m4
copym4.output = config.m4
copym4.commands = mkdir -p $$TOP_BUILDDIR/src/php-xconfig/ && $$QMAKE_COPY $$TOP_SRCDIR/src/php-xconfig/config.m4.in $$TOP_BUILDDIR/src/php-xconfig/config.m4

SOURCES_COPYCPP = xconfig.cpp.in
copycpp.input = SOURCES_COPYCPP
copycpp.target = xconfig.cpp
copycpp.output = xconfig.cpp
copycpp.commands = mkdir -p $$TOP_BUILDDIR/src/php-xconfig/ && $$QMAKE_COPY $$TOP_SRCDIR/src/php-xconfig/xconfig.cpp.in $$TOP_BUILDDIR/src/php-xconfig/xconfig.cpp

SOURCES_PHPIZE = config.m4 xconfig.cpp
phpize.input = SOURCES_PHPIZE
phpize.target = configure
phpize.output = configure
phpize.depends = config.m4 xconfig.cpp
phpize.commands = phpize

SOURCES_CONFIGURE = configure
configure.input = SOURCES_CONFIGURE
configure.target = build/Makefile
configure.output = build/Makefile
configure.depends = configure
configure.commands = cd build && CPPFLAGS=\"-I$$TOP_SRCDIR/src/libxconfig -g\" LDFLAGS=-L$$TOP_BUILDDIR/target/lib ../configure --enable-xconfig && cd -

QMAKE_EXTRA_COMPILERS += phpize copym4 copycpp configure

QMAKE_LINK = make -C build && true
OBJECTS = build/Makefile
TARGET = build/modules/xconfig.so

extensions.path = "$$system(php -r \"echo ini_get(\'extension_dir\');\")"
extensions.files = build/modules/xconfig.so
extensions.CONFIG = no_check_exist
ini.path = "$$system(php -r \"echo dirname(php_ini_loaded_file()) . \'/conf.d\';\")"
ini.files = xconfig.ini

INSTALLS += extensions ini

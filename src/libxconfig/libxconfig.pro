TEMPLATE = lib
TARGET = xconfig
VERSION = 1.0.0

include($$TOP_SRCDIR/xconfig.pri)

CONFIG += create_prl no_install_prl create_pc
QT -= core
PKGCONFIG +=
LIBS +=

INSTALL_HEADERS += \
	xconfig.h

HEADERS += \
	$$INSTALL_HEADERS

SOURCES += \
	xconfig.cpp

DESTDIR = $$TOP_BUILDDIR/target/$$LIBDIR

target.path = $$PREFIX$$LIBDIR
INSTALLS += target

QMAKE_CLEAN += ${TARGET}

headers.path = $$PREFIX/include/$$PACKAGE
headers.files = $$HEADERS
INSTALLS += headers


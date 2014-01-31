TEMPLATE = lib
TARGET = xconfig
VERSION = 1.0.0

include($$TOP_SRCDIR/xconfig.pri)

CONFIG += create_prl no_install_prl create_pc
QT -= core
PKGCONFIG += cmph tbb
LIBS += -Wl,--add-needed -lboost_thread

INSTALL_HEADERS += \
	xconfig.h \
	xconfig-inl.h \
	xconfig_connection.h \
	xconfig_file.h

HEADERS += \
	$$INSTALL_HEADERS

SOURCES += \
	xconfig_connection.cpp \
	xconfig.cpp

DESTDIR = $$TOP_BUILDDIR/target/$$LIBDIR

target.path = $$PREFIX$$LIBDIR
INSTALLS += target

QMAKE_CLEAN += ${TARGET}

headers.path = $$PREFIX/include/$$PACKAGE
headers.files = $$HEADERS
INSTALLS += headers


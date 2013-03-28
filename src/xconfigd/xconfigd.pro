TEMPLATE = app
TARGET = xconfigd

include($$TOP_SRCDIR/xconfig.pri)

PKGCONFIG += libdaemon liblog4cxx
LIBS += -L$$TOP_BUILDDIR/target/$$LIBDIR -lxconfig

DESTDIR = $$TOP_BUILDDIR/target/$$BINDIR

target.path = $$PREFIX$$BINDIR
INSTALLS += target

QMAKE_CLEAN += ${TARGET}

HEADERS += \
	TApplication.h \
	TLogger.h \
	XConfigDaemon.h

SOURCES += \
	TApplication.cpp \
	XConfigDaemon.cpp

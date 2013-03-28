TEMPLATE = app
TARGET = xconfig

include($$TOP_SRCDIR/xconfig.pri)

LIBS += -L$$TOP_BUILDDIR/target/$$LIBDIR -lxconfig

DESTDIR = $$TOP_BUILDDIR/target/$$BINDIR

target.path = $$PREFIX$$BINDIR
INSTALLS += target

QMAKE_CLEAN += ${TARGET}

HEADERS += \

SOURCES += \
	xconfig.cpp

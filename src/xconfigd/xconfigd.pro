TEMPLATE = app
TARGET = xconfigd

include($$TOP_SRCDIR/xconfig.pri)

PKGCONFIG += 
LIBS += 

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

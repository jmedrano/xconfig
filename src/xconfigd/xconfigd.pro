TEMPLATE = app
TARGET = xconfigd

include($$TOP_SRCDIR/xconfig.pri)

QT += network
PKGCONFIG += libdaemon liblog4cxx tbb
LIBS += -Wl,--add-needed -L$$TOP_BUILDDIR/target/$$LIBDIR -lxconfig -lyaml -lcmph

DESTDIR = $$TOP_BUILDDIR/target/$$BINDIR

target.path = $$PREFIX$$BINDIR
INSTALLS += target

QMAKE_CLEAN += ${TARGET}

HEADERS += \
	TLogger.h \
	TApplication.h \
	XConfigDaemon.h \
	ServerSocket.h \
	FileLock.h \
	ConnectionManager.h \
	ConfigurationTree.h \
	ConfigurationPool.h \
	YamlParser.h \
	ConfigurationMerger.h \
	crc/crc64.h

SOURCES += \
	TApplication.cpp \
	XConfigDaemon.cpp \
	ServerSocket.cpp \
	FileLock.cpp \
	ConnectionManager.cpp \
	ConfigurationTree.cpp \
	ConfigurationPool.cpp \
	YamlParser.cpp \
	ConfigurationMerger.cpp \
	crc/crc64.c

INCLUDEPATH += $$TOP_SRCDIR/src/libxconfig

TEMPLATE = app
TARGET = xconfigd

include($$TOP_SRCDIR/xconfig.pri)

QT += network
PKGCONFIG += libdaemon liblog4cxx
LIBS += -L$$TOP_BUILDDIR/target/$$LIBDIR -lxconfig -lyaml -lcmph

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
	ConfigurationMerger.h

SOURCES += \
	TApplication.cpp \
	XConfigDaemon.cpp \
	ServerSocket.cpp \
	FileLock.cpp \
	ConnectionManager.cpp \
	ConfigurationTree.cpp \
	ConfigurationPool.cpp \
	YamlParser.cpp \
	ConfigurationMerger.cpp

INCLUDEPATH += $$TOP_SRCDIR/src/libxconfig

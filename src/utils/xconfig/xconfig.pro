TEMPLATE = app
TARGET = xconfig

include($$TOP_SRCDIR/xconfig.pri)

PKGCONFIG += cmph
LIBS += -Wl,--add-needed -L$$TOP_BUILDDIR/target/$$LIBDIR -lxconfig -lyaml -lboost_program_options -lboost_thread

DESTDIR = $$TOP_BUILDDIR/target/$$BINDIR

target.path = $$PREFIX$$BINDIR
INSTALLS += target

QMAKE_CLEAN += ${TARGET}

HEADERS += \

SOURCES += \
	xconfig.cpp

INCLUDEPATH += $$TOP_SRCDIR/src/libxconfig

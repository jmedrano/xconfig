TEMPLATE = app
TARGET = xconfig

include($$TOP_SRCDIR/xconfig.pri)

LIBS += -L$$TOP_BUILDDIR/target/$$LIBDIR -lxconfig -lyaml -lcmph -lboost_program_options

DESTDIR = $$TOP_BUILDDIR/target/$$BINDIR

target.path = $$PREFIX$$BINDIR
INSTALLS += target

QMAKE_CLEAN += ${TARGET}

HEADERS += \

SOURCES += \
	xconfig.cpp

INCLUDEPATH += $$TOP_SRCDIR/src/libxconfig

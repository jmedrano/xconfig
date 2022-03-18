CONFIG += link_pkgconfig nostrip silent
QT -= gui
QT +=

DISABLE_FEATURES_FLAGS =

release {
	OPTIMIZE_FLAGS = -O3 -fomit-frame-pointer -fstrict-aliasing -DNDEBUG
}

equals(QMAKE_HOST.arch, x86_64) {
	DISABLE_FEATURES_FLAGS = -mno-sse2 -mno-aes -mno-pclmul
}

profile {
	PROFILE_CFLAGS = -pg
	PROFILE_LFLAGS = -pg
}

QMAKE_CXXFLAGS += -I$$TOP_BUILDDIR -I$$TOP_SRCDIR -std=gnu++0x -rdynamic -Werror $$OPTIMIZE_FLAGS $$PROFILE_CFLAGS $$QMAKE_CXXFLAGS_DEBUG $$DISABLE_FEATURES_FLAGS
QMAKE_CFLAGS += -Werror -I$$TOP_BUILDDIR -I$$TOP_SRCDIR $$OPTIMIZE_FLAGS $$PROFILE_CFLAGS $$QMAKE_CFLAGS_DEBUG $$DISABLE_FEATURES_FLAGS
QMAKE_LFLAGS += $$PROFILE_LFLAGS $$QMAKE_LFLAGS_DEBUG

COMMON_DEPENDENCIES = \
	$$TOP_BUILDDIR \
	$$TOP_SRCDIR/include/QtArg \
	$$TOP_SRCDIR/src/common

DEPENDPATH += $$COMMON_DEPENDENCIES
INCLUDEPATH += $$COMMON_DEPENDENCIES


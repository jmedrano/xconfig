TEMPLATE = app
CONFIG -= gdb_dwarf_index
QMAKE_LINK = @: IGNORE THIS LINE

include($$TOP_SRCDIR/xconfig.pri)

DESTDIR = $$TOP_BUILDDIR/target/$$PKGSYSCONFDIR
INSTALLDIR = $$PKGSYSCONFDIR

BUILD_SUBSTITUTES = \
	./xconfigd.ini.in \
	./log4cxx.xml.in

ALL_BUILD_SUBSTITUTES = $$BUILD_SUBSTITUTES $$BUILD_SUBSTITUTES_NOINSTALL
QMAKE_SUBSTITUTES += $$ALL_BUILD_SUBSTITUTES

for(file, BUILD_SUBSTITUTES) {
	dir = $$dirname(file)
	name = $$replace(dir, "[./\\\\]", "_")
	file = $$replace(file, "(.*)\\.in$", "\\1")
	eval($${name}.path = $$INSTALLDIR/$$dir)
	eval($${name}.files += $$file)
	INSTALLS *= $$name
}


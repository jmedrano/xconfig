PACKAGE = xconfig
VERSION = -1.1.0

CACHEFILE = $$quote($${PWD}/.qmake.cache)
CACHEFILETMP = $$quote($${CACHEFILE}.tmp)

defineReplace(quotepath) {
	return(\\\$\\\$quote\\\($$quote($$1)\\\))
}

system(rm -f \'$$CACHEFILE\')

VARS = TOP_SRCDIR \
       TOP_BUILDDIR \
       BINDIR \
       LIBDIR \
       LOCALSTATEDIR \
       SYSCONFDIR \
       DATADIR \
	   PKGLIBDIR \
       PKGDATADIR \
       PKGLOCALSTATEDIR \
       PKGSYSCONFDIR \
       PREFIX \
	   PACKAGE \
	   VERSION

TOP_SRCDIR = $$quote($$PWD)
TOP_BUILDDIR = $$quote($$OUT_PWD)

BINDIR = $$quote(/bin)
LIBDIR = $$quote(/lib)
DATADIR = $$quote(/share)
LOCALSTATEDIR = $$quote(/var)
SYSCONFDIR = $$quote(/etc)
PKGLIBDIR = $$quote(/lib/$$PACKAGE$$VERSION)
PKGDATADIR = $$quote(/share/$$PACKAGE$$VERSION)
PKGLOCALSTATEDIR = $$quote(/var/$$PACKAGE)
PKGSYSCONFDIR = $$quote(/etc/$$PACKAGE)

PREFIX = $$quote($$PREFIX)

system(rm -f \'$$CACHEFILETMP\')

for(v, VARS):{
	eval(VALUE = \$\$$${v})
	system(echo '$$v = $$VALUE' >> \'$$CACHEFILETMP\')
}

system(mv \'$$CACHEFILETMP\' \'$$CACHEFILE\')

PACKAGE = xconfig
VERSION = -1.0.0

CACHEFILE = $$quote($${IN_PWD}/.qmake.cache)
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

TOP_SRCDIR = $$quotepath($$IN_PWD)
TOP_BUILDDIR = $$quotepath($$OUT_PWD)

BINDIR = $$quotepath(/bin)
LIBDIR = $$quotepath(/lib)
DATADIR = $$quotepath(/share)
LOCALSTATEDIR = $$quotepath(/var)
SYSCONFDIR = $$quotepath(/etc)
PKGLIBDIR = $$quotepath(/lib/$$PACKAGE$$VERSION)
PKGDATADIR = $$quotepath(/share/$$PACKAGE$$VERSION)
PKGLOCALSTATEDIR = $$quotepath(/var/$$PACKAGE$$VERSION)
PKGSYSCONFDIR = $$quotepath(/etc/$$PACKAGE)

PREFIX = $$quotepath($$PREFIX)

system(rm -f \'$$CACHEFILETMP\')

for(v, VARS):{
	eval(VALUE = $$quote($$)$${v})
	system(echo '$$v = $$VALUE' >> \'$$CACHEFILETMP\')
}

system(mv \'$$CACHEFILETMP\' \'$$CACHEFILE\')

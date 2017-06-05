TEMPLATE = subdirs
SUBDIRS = conf src/libxconfig src/utils src/xconfigd src/php-xconfig src/pyxconfig

src-utils.depends = sub-src-libxconfig
src-xconfigd.depends = sub-src-libxconfig
src-php-xconfig.depends = sub-src-libxconfig
src-pyxconfig.depends = sub-src-libxconfig

QMAKE_SUBSTITUTES += config.h.in

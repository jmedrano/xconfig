TEMPLATE = subdirs
SUBDIRS = conf src/xconfigd

QMAKE_SUBSTITUTES += config.h.in

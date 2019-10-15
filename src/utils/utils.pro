TEMPLATE = subdirs
SUBDIRS = xconfig

confighelper.path = $$PREFIX$$BINDIR
confighelper.files = config_helper

INSTALLS += confighelper 

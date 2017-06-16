QMAKE_LINK = LDFLAGS=-L$$TOP_BUILDDIR/target/lib python setup.py build && true

extension.path = /usr/lib/python2.7/dist-packages
extension.files = build/lib.linux-x86_64-2.7/xconfig.so

TARGET = build/lib.linux-x86_64-2.7/xconfig.so

INSTALLS += extension

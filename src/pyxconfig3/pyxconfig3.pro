QMAKE_LINK = LDFLAGS=-L$$TOP_BUILDDIR/target/lib python3.5 setup.py build && true

extension.path = /usr/lib/python3.5/dist-packages
extension.files = build/lib.linux-x86_64-3.5/xconfig.cpython-35m-x86_64-linux-gnu.so

TARGET = build/lib.linux-x86_64-3.5/xconfig.cpython-35m-x86_64-linux-gnu.so

INSTALLS += extension

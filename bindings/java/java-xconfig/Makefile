VERSION=$(shell dpkg-parsechangelog -n1 | grep Version | grep -Eo '([0-9]+[.]?)*' | head -n1)

build:
	mvn clean package

install: 
	mkdir -p $(DESTDIR)/usr/share/java
	mkdir -p $(DESTDIR)/usr/lib/jni
	cp xconfig-native/target/xconfig-native-$(VERSION).jar \
		xconfig-base/target/xconfig-base-$(VERSION).jar \
		$(DESTDIR)/usr/share/java
	cp xconfig-native/target/nar/xconfig-native-$(VERSION)-amd64-Linux-gpp-jni/lib/amd64-Linux-gpp/jni/libxconfig-native-$(VERSION).so \
		$(DESTDIR)/usr/lib/jni

	ln -s xconfig-native-$(VERSION).jar $(DESTDIR)/usr/share/java/xconfig-native.jar
	ln -s xconfig-base-$(VERSION).jar $(DESTDIR)/usr/share/java/xconfig-base.jar
	ln -s libxconfig-native-$(VERSION).so $(DESTDIR)/usr/lib/jni/libxconfig-native.so

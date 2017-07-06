# XConfig implementation for Java
This maven project contains the XConfig implementation for Java language.

## Modules
- `xconfig-base`: common code between all implementations
- `xconfig-java-parser`: parsing of yaml directly in Java
- `xconfig-java`: implementation of XConfig just using Java
- `xconfig-native`: native implementation using a native binding. *Must be used in production*.

Please bear in mind that `xconfig-java-parser` and `xconfig-java` are helpers
to develop or run tests, but must never be used in production.

## Packaging
`xconfig-native` needs the equivalent `libxconfig-native.so` to be able to work.
In order to use all the matching versions, a `java-xconfig` debian package
is created with:
- `/usr/share/java`: `xconfig-base` and `xconfig-native` jars. That path must
be included in the application classpath. It's already a standard in Tomcat.
- `/usr/lib/jni`: `libxconfig-native.so` with the native binding. That path
must be specified in `java.library.path` when running the application.

## Release
Prior to release, please remember to update the debian package changelog using
`dch -n`.

All the modules jars must be built and deployed through a standard *automated
release*. Check Flow for specific details.

In order to create the debian package, please jump to the release branch and
use `cigen` with the `build_java_deb_package` workflow. You can now grab
the generated package and ask SRE to upload it to our mirror.


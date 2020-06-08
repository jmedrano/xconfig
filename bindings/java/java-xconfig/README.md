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
`xconfig-native` dynamically links to `libxconfig.so`, so it needs to be available
in the system for it to be able to work.

## Release
All the modules jars must be built and deployed through a standard *automated
release*. Check Flow for specific details.


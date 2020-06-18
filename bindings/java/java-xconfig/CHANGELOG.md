# Java XConfig Changelog

### 1.4.0
 - Added support for caching xconfig access.
 - Changed how the native binding is package and distributed, not longer needs to be present in the base image.
    - This means that new versions of the base image (>= java-base:7.0) is required to avoid classpath collisions
    between xconfig classes.
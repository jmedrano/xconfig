# Java XConfig Changelog

### 1.5.0
 - [FRWK-532](https://jira.tid.es/browse/FRWK-532) XConfig values are immutable now.
 - [FRWK-532](https://jira.tid.es/browse/FRWK-532) Cached method and field ids in xconfig-native cpp code.

### 1.4.2
 - [FRWK-532](https://jira.tid.es/browse/FRWK-532) Fixed BreedXConfig mutating the cached map when merging breeds.

### 1.4.1
 - [FRWK-511](https://jira.tid.es/browse/FRWK-511) Changed the xconfig cache to a bounded implementation to mitigate memory pressure when a service reads a lot of different keys.
    * Current implementation uses a bounded cache with lru-like evictions.

### 1.4.0
 - Added support for caching xconfig access.
 - Changed how the native binding is package and distributed, not longer needs to be present in the base image.
    - This means that new versions of the base image (>= java-base:7.0) is required to avoid classpath collisions
    between xconfig classes.
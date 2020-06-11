The build system is based on cigen, you can generate the xconfig packages and 
the python wheels with the appropiate target depending on the target operating
system:

    build_buster

The generated debian packages and wheel files will be left on the packages directory.

To generate the java binding there is a different target:

    build_java_deb_package

More info about this project can be found on the project documentation page:
https://doc.tuenti.io/global-doc/platform/configuration/xconfig/

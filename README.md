The build system is based on cigen, you can generate the xconfig packages, PHP
binding and the python wheels with:

    build_buster

The generated debian packages and wheel files will be left on the packages directory.

You can also build everything and upload python packages with:

    release_python

To generate and upload the java binding there is a different target:

    release_java

More info about this project can be found on the project documentation page:
https://doc.tuenti.io/global-doc/platform/configuration/xconfig/

# XConfig

## What is it
XConfig is a hierarchy configuration system based in a superset of the YAML format.
Is a client-server system built using C++ and using the QT framework. The server is called
*Xconfigd* and the client is *libxconfig*. Also, there are a couple of bindings for several
languages built on top of *libxconfig* (Python, PHP, and Java).

Configuration files are readed from a list of directories, and the config is merged considering
that the list goes from the more general config to the more specific. This allows to have some
directories for global configuration for all the services and others mapping only to
a specific service deployed on a given cluster. For this reason when a config is requested
it needs also a config path. This config path will allow XConfig to know where to read
the config files and in which order. 

This config path consist on a list of paths inside separated by colon ":".

## How does it work?
XConfigd works in a lazy way. It doesn't process any config until it receives the first
connection. This connection should be done via UNIX socket domain, so the Xconfig daemon
and client should be in the same machine and shared visibility to the socket file.
This makes XConfig quite efficient but also not easy to port to other operating systems.

Every time a connection is received, the client needs to provide the `CONFIG_PATH` from where it
should read the yaml files. Once this happens, XConfig will process the config inside those
directories and generate a config tree. This tree will be cached and monitored for some time.
If a path is not requested in that time, it will be flushed.

Once the server has created the config tree, it will respond to the client a message contaning the
XConfigd version and more importantly it will send an open file descriptor to the client process
which contains the config tree. This is done OOB with a `SCM_RIGHTS` access control message on 
the [sendDatagram function](https://github.com/Telefonica/xconfig/blob/119d1e27c07aab919efda9109a89483ebdc3c9d6/src/xconfigd/ConnectionManager.cpp#L164).

Also, when the configuration tree is reloaded (check section bellow to see how), the server sends
a [push message](https://github.com/Telefonica/xconfig/blob/119d1e27c07aab919efda9109a89483ebdc3c9d6/src/xconfigd/ConnectionManager.cpp#L228), 
with the new configuration tree, also using *sendDatagram* function.

There are no more communication messages than the ones described, which means that most operations
regarding the config are handled on the client side (since he was complete visibility to the 
configuration tree). This means that the xconfig daemon sits idle the majority of the time, waiting
for events like new connections or config changes, even if there are thousands of clients constantly
reading the configuration.

### Config changes
When XConfigd creates a config tree, it also adds iNotify watches to each path inside the config
path. This makes that everytime something changes inside, it will know about the change and
evaluate if it needs to recreate the tree or not.

These changes are made in a delayed way, so if many config changes arrive with no so much time
difference, they will only be processed once, trying to minimize the work.

Also XConfig has a timer to refresh the stored config trees if it didn't receive any iNotify event
in a while, making sure no change is lost

Once a config path is not used for a long time, XConfig will ask QT to delete all the objects related
to the tree in a delayed way, trying to clean them once the event loop of the app has finished an 
iteration.

### Config merge
Once XConfig receives a path it will process the different directories in the order found
in the config path. It should go from the most generic to the most specific. 

This is because it will take the first path as the "base" tree, and it will be adding the
trees found in the next directories on top of it, merging or overriding parts of the base
tree as needed. In general, dictionaries will be merged, and values and lists will be overwriten.

There are some rules on top of yaml files that allows to compose / modify or delete
some parts of the tree on demand. You can find more information about this in [this doc](https://doc.tuenti.io/global-doc/platform/configuration/xconfig/)

### How is it deployed
For the reasons described previously, XConfig is a very special system. The requirements
to have the server on the same host are satisfied on our k8s clusters using DaemonSets
(to ensure that every worker node has a xconfig daemon running) and hostPath shared volume
to share the unix socket as a volume between the server and the client.

## How to build it
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

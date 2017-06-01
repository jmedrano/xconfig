import os, sys, resource, gc
sys.path.append('./build/lib.linux-x86_64-2.7')
import xconfig

xconfig.init(os.getcwd() + "/config")

print(resource.getrusage(resource.RUSAGE_SELF).ru_maxrss)
for i in xrange(10):
    for i in xrange(100000):
        xconfig.getValue(sys.argv[1].split('/'))
    gc.collect();
    print(resource.getrusage(resource.RUSAGE_SELF).ru_maxrss)
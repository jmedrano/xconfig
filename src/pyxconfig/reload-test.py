import os, sys, resource, gc
sys.path.append('./build/lib.linux-x86_64-2.7')
import xconfig

xconfig.init(os.getcwd() + "/config")

while True:
    try:
        xconfig.reload()
        key = raw_input().split("/")
        value = xconfig.getValue(key)
        print repr(value)
    except Exception as e:
        print e


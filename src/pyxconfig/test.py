import os, sys
sys.path.append('./build/lib.linux-x86_64-2.7')
import xconfig

xconfig.init(os.getcwd() + "/config")

assert xconfig.getValue("testConfig/sampleInt") == 1
assert xconfig.getValue("testConfig/sampleBool") == True
assert xconfig.getValue("testConfig/sampleNull") is None
assert xconfig.getValue("testConfig/sampleString") == "test"
assert xconfig.getValue("testConfig/sampleList") == [1, 2, []]
assert xconfig.getValue("testConfig/sampleMap") == {"a": 1, "b": [{"x": []}], "c": "test"}

try:
    xconfig.getValue("missingConfig")
    assert False
except xconfig.XConfigNotFoundException as e:
    pass

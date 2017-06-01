import os, sys
sys.path.append('./build/lib.linux-x86_64-2.7')
import xconfig

try:
    xconfig.getValue(["notConnected"])
    assert False
except xconfig.XConfigNotConnectedException as e:
    pass

xconfig.init(os.getcwd() + "/config")

assert xconfig.getValue(["testConfig", "sampleInt"]) == 1
assert xconfig.getValue(["testConfig", "sampleBool"]) == True
assert xconfig.getValue(["testConfig", "sampleNull"]) is None
assert xconfig.getValue(["testConfig", "sampleString"]) == "test"
assert xconfig.getValue(["testConfig", "sampleList"]) == [1, 2, []]
assert xconfig.getValue(["testConfig", "sampleMap"]) == {"a": 1, "b": [{"x": []}], "c": "test"}

try:
    xconfig.getValue(["missingConfig"])
    assert False
except xconfig.XConfigNotFoundException as e:
    pass

wrong_keys = [None, {}, True, 123]
for wrong_key in wrong_keys:
    try:
        xconfig.getValue(wrong_key)
        assert False
    except xconfig.XConfigWrongTypeException as e:
        pass

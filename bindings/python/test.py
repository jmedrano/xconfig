import os, sys, time, resource, gc
sys.path.append('./build/lib.linux-x86_64-%s' % os.getenv('PYXCONFIG_TARGET_VERSION'))
import xconfig
import json

CONFIG_PATH_ROOT = os.getcwd() + "/config"
CONFIG_PATHS = f"{CONFIG_PATH_ROOT}/base:{CONFIG_PATH_ROOT}/override"
xc = xconfig.XConfig(CONFIG_PATHS)

assert xc.getValue(["testConfig", "sampleInt"]) == 1
assert xc.getValue(["testConfig", "sampleBool"]) is True
assert xc.getValue(["testConfig", "sampleNull"]) is None
assert xc.getValue(["testConfig", "sampleString"]) == "test"
assert xc.getValue(["testConfig", "sampleList"]) == [1, 2, []]
assert xc.getValue(["testConfig", "sampleMap"]) == {"a": 1, "b": [{"x": []}], "c": "test"}


assert xc.getValue(["testConfig", "listWithNull"]) == [1, 2, None, 4]
assert xc.getValue(["testConfig", "mapWithNull"]) == {"a": 1, "b": None, "c": 3}

assert xc.getValue(["testConfig", "sampleExpandString"]) == "this is an expanded test"
assert xc.getValue(["testConfig", "sampleExpandRef"]) == 1
os.environ["SAMPLE_ENV"] = "random content"
assert xc.getValue(["testConfig", "sampleExpandEnv"]) == "random content"
assert xc.exists(["testConfig", "sampleMap"]) is True
assert xc.exists(["testConfig", "missing"]) is False

assert xc.exists(["testConfig", "deletedKey"]) is False
assert xc.getValue(["testConfig", "overridedKey"]) == {"a": 1, "b": 42}
assert xc.exists(["testConfig", "nonExistentKey"]) is True
assert xc.getValue(["testConfig", "nonExistentKey"]) == {}

#print(json.dumps(xc.getValue([""]), sort_keys=True, indent=4))
#print(json.dumps(xc.getValue(["testConfig", "overridedKey"]), sort_keys=True, indent=4))

try:
    xc.getValue(["missingConfig"])
    assert False
except xconfig.XConfigNotFoundException as e:
    pass

wrong_keys = [None, {}, True, 123, 'abc']
for wrong_key in wrong_keys:
    try:
        xc.getValue(wrong_key)
        assert False
    except xconfig.XConfigWrongTypeException as e:
        pass

checksum = xc.getChecksum(["testConfig"])
xc.reload()
assert checksum == xc.getChecksum(["testConfig"])

print('Basic tests OK')


xc2 = xconfig.XConfig("nonexistent_path")
try:
    xc2.getValue(["testConfig"])
except xconfig.XConfigNotFoundException as e:
    pass

xc.getValue(["testConfig"])

xc3 = xconfig.XConfig(CONFIG_PATHS)
xc3.getValue(["testConfig"])

print('Multiple instances tests OK')


for i in range(10):
    yaml = "tmp: %d" % i
    with open('config/base/tmp.yaml','w') as f:
        f.write(yaml)

    # 100ms seems too low to ensure a reload
    time.sleep(1)
    xc.reload()
    assert xc.getValue(['tmp']) == i

print('Reload tests OK')


original_memory_usage = memory_usage = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
print('Memory usage: %d' % memory_usage)
for i in range(10):
    for j in range(100000):
        xc = xconfig.XConfig(CONFIG_PATHS)
        xc.getValue(['testConfig'])
        try:
            xc.getValue(['testConfig2'])
        except xconfig.XConfigNotFoundException as e:
            pass
    for j in range(100):
        xc.close()
        xc.reload()
    gc.collect()
    memory_usage = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    print('Memory usage: %d' % memory_usage)

if original_memory_usage - memory_usage < 100:
    print('Memory tests OK')

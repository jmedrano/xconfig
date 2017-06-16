import os, sys, time, resource, gc
sys.path.append('./build/lib.linux-x86_64-2.7')
import xconfig

xc = xconfig.XConfig(os.getcwd() + "/config")

assert xc.getValue(["testConfig", "sampleInt"]) is 1
assert xc.getValue(["testConfig", "sampleBool"]) is True
assert xc.getValue(["testConfig", "sampleNull"]) is None
assert xc.getValue(["testConfig", "sampleString"]) == "test"
assert xc.getValue(["testConfig", "sampleList"]) == [1, 2, []]
assert xc.getValue(["testConfig", "sampleMap"]) == {"a": 1, "b": [{"x": []}], "c": "test"}

assert xc.exists(["testConfig", "sampleMap"]) is True
assert xc.exists(["testConfig", "missing"]) is False

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

print 'Basic tests OK'


xc2 = xconfig.XConfig(os.getcwd() + "/noconfig")
try:
    xc2.getValue(["testConfig"])
except xconfig.XConfigNotFoundException as e:
    pass

xc.getValue(["testConfig"])

xc3 = xconfig.XConfig(os.getcwd() + "/config")
xc3.getValue(["testConfig"])

print 'Multiple instances tests OK'


for i in xrange(10):
    yaml = "tmp: %d" % i
    with open('config/tmp.yaml','w') as f:
        f.write(yaml)

    # 100ms seems too low to ensure a reload
    time.sleep(1)
    xc.reload()
    assert xc.getValue(['tmp']) == i

print 'Reload tests OK'


original_memory_usage = memory_usage = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
print 'Memory usage: %d' % memory_usage
for i in xrange(10):
    for j in xrange(100000):
        xc = xconfig.XConfig(os.getcwd() + "/config")
        xc.getValue(['testConfig'])
        try:
            xc.getValue(['testConfig2'])
        except xconfig.XConfigNotFoundException as e:
            pass
    for j in xrange(100):
        xc.close()
        xc.reload()
    gc.collect()
    memory_usage = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    print 'Memory usage: %d' % memory_usage

if original_memory_usage - memory_usage < 100:
    print 'Memory tests OK'

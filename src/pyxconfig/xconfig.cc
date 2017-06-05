#include <Python.h>
#include <xconfig.h>
#include <iostream>

using std::string;
using std::vector;
using xconfig::XConfig;
using xconfig::XConfigNode;
using xconfig::XConfigNotConnected;
using xconfig::UnixConnection;
using xconfig::UnixConnectionPool;

static XConfig* xc = NULL;
static PyObject *XConfigWrongTypeException = NULL;
static PyObject *XConfigNotFoundException = NULL;
static PyObject *XConfigNotConnectedException = NULL;
static UnixConnectionPool* xconfig_pool = NULL;

/**
 * Get a python value for an xconfig node
 * @param node the node
 * @return the parsed value
 */
static PyObject* getValue(const XConfigNode node) {
    switch (node.getType()) {
        case xconfig::TYPE_NULL:
            return Py_BuildValue("z", NULL);
        case xconfig::TYPE_BOOLEAN:
            return PyBool_FromLong(node.getBool());
        case xconfig::TYPE_INTEGER:
            return Py_BuildValue("l", node.getInt());
        case xconfig::TYPE_FLOAT:
            return Py_BuildValue("f", node.getFloat());
        case xconfig::TYPE_STRING:
            return Py_BuildValue("s", node.getString().c_str());
        case xconfig::TYPE_SEQUENCE: {
            PyObject *list = PyList_New(0);
            std::vector<XConfigNode> children = node.getChildren();
            for (XConfigNode child : node.getChildren()) {
                PyObject* childValue = getValue(child);
                PyList_Append(list, childValue);
                Py_DECREF(childValue);
            }
            return list;
        }
        case xconfig::TYPE_MAP: {
            PyObject *dict = PyDict_New();
            std::vector<XConfigNode> children = node.getChildren();
            for (XConfigNode child : node.getChildren()) {
                PyObject* childValue = getValue(child);
                PyDict_SetItemString(dict, child.getName().c_str(), childValue);
                Py_DECREF(childValue);
            }
            return dict;
        }
        default:
            PyErr_SetString(XConfigWrongTypeException, "Wrong type for key");
            return NULL;
    }
}

/**
 * xconfig.xconfig_init(config_path)
 * Create an xconfig connection and set the config paths
 * @param self
 * @param args
 * @return null
 */
static PyObject* xconfig_init(PyObject *self, PyObject *args) {
    char *path;

    if (PyArg_ParseTuple(args, "s", &path)) {
        try {
            if (!xconfig_pool) {
                xconfig_pool = new UnixConnectionPool;
            }
            xc = new XConfig(xconfig_pool->getConnection(path), true);
            return Py_BuildValue("z", NULL);
        } catch (const XConfigNotConnected &e) {
            PyErr_SetString(XConfigNotConnectedException, "XConfig not connected");
            return NULL;
        }
    }
}

/**
 * xconfig.xconfig.getValue(key)
 * Get the xconfig value for a given key (specified as a list)
 * @param self
 * @param args
 * @return the python value
 */
static PyObject* xconfig_getValue(PyObject *self, PyObject *args) {
    if (!xc) {
        PyErr_SetString(XConfigNotConnectedException, "XConfig not connected");
        return NULL;
    }

    PyObject* keysParam;
    XConfigNode node;

    if (PyArg_ParseTuple(args, "O", &keysParam) && PySequence_Check(keysParam)) {
        int size = PySequence_Size(keysParam);
        vector <string> keys(size);

        for (int i=0; i<size; i++) {
            PyObject* item = PySequence_GetItem(keysParam, i);
            PyObject* itemStr = PyObject_Str(item);

            const char* keyPart = PyString_AsString(itemStr);
            keys[i] = (string) keyPart;

            Py_DECREF(item);
            Py_DECREF(itemStr);
        }

        node = xc->getNodeNoThrow(keys);
        if (!node) {
            PyErr_SetString(XConfigNotFoundException, "Key not found");
            return NULL;
        } else {
            return getValue(node);
        }
    }

    PyErr_SetString(XConfigWrongTypeException, "Wrong type for key");
    return NULL;
}


static PyObject* xconfig_getMTime(PyObject *self, PyObject *args) {
    if (!xc) {
        PyErr_SetString(XConfigNotConnectedException, "XConfig not connected");
        return NULL;
    }

    PyObject* keysParam;
    XConfigNode node;

    if (PyArg_ParseTuple(args, "O", &keysParam) && PySequence_Check(keysParam)) {
        int size = PySequence_Size(keysParam);
        vector <string> keys(size);

        for (int i = 0; i < size; i++) {
            const char *keyPart = PyString_AsString(PyObject_Str(PySequence_GetItem(keysParam, i)));
            keys[i] = (string) keyPart;
        }

        node = xc->getNodeNoThrow(keys);
        if (!node) {
            PyErr_SetString(XConfigNotFoundException, "Key not found");
            return NULL;
        }

        timespec ts = xc->getMtime(node);
        return PyTuple_Pack(2, PyInt_FromLong(ts.tv_sec), PyInt_FromLong(ts.tv_nsec));
    }

    PyErr_SetString(XConfigWrongTypeException, "Wrong type for key");
    return NULL;
}

static PyObject* xconfig_reload(PyObject *self, PyObject *args) {
    if (!xc) {
        PyErr_SetString(XConfigNotConnectedException, "XConfig not connected");
        return NULL;
    }
    xc->reload();
    return Py_BuildValue("z", NULL);
}

static PyMethodDef XConfigMethods[] = {
        {"init",  xconfig_init, METH_VARARGS,
                        "Initialize XConfig module."},
        {"reload",  xconfig_reload, METH_VARARGS,
                        "Reload XConfig connection."},
        {"getValue",  xconfig_getValue, METH_VARARGS,
                        "Gets a config value for a node specification."},
        {"getMTime",  xconfig_getMTime, METH_VARARGS,
                        "Gets a tuple (secs, nsecs) with the modification time of a key."},
        {NULL, NULL, 0, NULL}        /* Sentinel */
};

PyMODINIT_FUNC initxconfig(void) {
    PyObject *m;

    m = Py_InitModule("xconfig", XConfigMethods);
    if (m == NULL) {
        return;
    }

    XConfigNotFoundException = PyErr_NewException("xconfig.XConfigNotFoundException", NULL, NULL);
    Py_INCREF(XConfigNotFoundException);
    PyModule_AddObject(m, "XConfigNotFoundException", XConfigNotFoundException);

    XConfigWrongTypeException = PyErr_NewException("xconfig.XConfigWrongTypeException", NULL, NULL);
    Py_INCREF(XConfigWrongTypeException);
    PyModule_AddObject(m, "XConfigWrongTypeException", XConfigWrongTypeException);

    XConfigNotConnectedException = PyErr_NewException("xconfig.XConfigNotConnectedException", NULL, NULL);
    Py_INCREF(XConfigNotConnectedException);
    PyModule_AddObject(m, "XConfigNotConnectedException", XConfigNotConnectedException);
}

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

static PyObject *XConfigWrongTypeException = NULL;
static PyObject *XConfigNotFoundException = NULL;
static PyObject *XConfigNotConnectedException = NULL;
static UnixConnectionPool* xconfig_pool = NULL;

struct XConfigObject {
    PyObject_HEAD
    XConfig* xc;
};

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
            PyErr_SetString(XConfigWrongTypeException, "Unsupported type for node");
            return NULL;
    }
}

static bool getKeyVectorFromArgs(PyObject *args, vector<string> &keys) {
    PyObject* keysParam;

    if (PyArg_ParseTuple(args, "O", &keysParam) && PyList_Check(keysParam)) {
        int size = PySequence_Size(keysParam);

        for (int i=0; i<size; i++) {
            PyObject* item = PySequence_GetItem(keysParam, i);
            PyObject* itemStr = PyObject_Str(item);

            const char* keyPart = PyBytes_AS_STRING(itemStr);
            keys.push_back((string) keyPart);

            Py_DECREF(item);
            Py_DECREF(itemStr);
        }

        return true;
    }

    PyObject* argsStr = PyObject_Str(args);
    PyErr_SetString(XConfigWrongTypeException, ((string)"Wrong type for key " + PyBytes_AS_STRING(argsStr)).c_str());
    Py_DECREF(argsStr);
    return false;
}

static bool getNodeFromArgs(XConfigObject *self, PyObject *args, XConfigNode &node, bool nothrow = false) {
    vector<string> keys;

    if (!getKeyVectorFromArgs(args, keys)) { return false; }

    try {
        node = self->xc->getNodeNoThrow(keys);
        if (!node && !nothrow) {
            PyObject* argsStr = PyObject_Str(args);
            string message = (string)"Key " + PyBytes_AS_STRING(argsStr) + " not found";
            PyErr_SetString(XConfigNotFoundException, message.c_str());
            Py_DECREF(argsStr);
            return false;
        }
        return true;
    } catch (const XConfigNotConnected& e) {
        PyErr_SetString(XConfigNotConnectedException, "XConfig not connected");
        return false;
    }
}

/**
 * XConfig(config_path)
 * Create an xconfig connection and set the config paths
 * @param self
 * @param args
 * @return null
 */
static int XConfig_init(XConfigObject *self, PyObject *args) {
    char *path;

    if (PyArg_ParseTuple(args, "s", &path)) {
        try {
            if (!xconfig_pool) {
                xconfig_pool = new UnixConnectionPool;
            }
            self->xc = new XConfig(xconfig_pool->getConnection(path), false);;
            return 0;
        } catch (const XConfigNotConnected &e) {
            PyObject* argsStr = PyObject_Str(args);
            string message = (string)"XConfig could not connect with path " + PyBytes_AS_STRING(argsStr);
            PyErr_SetString(XConfigNotConnectedException, message.c_str());
            Py_DECREF(argsStr);
            return -1;
        }
    }
}

static void XConfig_dealloc(XConfigObject* self)
{
    delete self->xc;
    Py_TYPE(self)->tp_free((PyObject*)self);
}

/**
 * XConfig.getValue(key)
 * Get the xconfig value for a given key (specified as a list)
 * @param self
 * @param args
 * @return the python value
 */
static PyObject* xconfig_getValue(XConfigObject *self, PyObject *args) {
    XConfigNode node;
    if (!getNodeFromArgs(self, args, node)) { return NULL; }
    return getValue(node);
}

/**
 * XConfig.getChecksum(key)
 * Get the checksum of a key
 * @param self
 * @param args
 * @return the checksum as long
 */
static PyObject* xconfig_getChecksum(XConfigObject *self, PyObject *args) {
    XConfigNode node;
    if (!getNodeFromArgs(self, args, node)) { return NULL; }
    uint64_t checksum = self->xc->getChecksum(node);
    return PyLong_FromLong(checksum);
}

/**
 * XConfig.exists(key)
 * Check if a node exists
 * @param self
 * @param args
 * @return true if the node exists
 */
static PyObject* xconfig_exists(XConfigObject *self, PyObject *args) {
    XConfigNode node;
    if (!getNodeFromArgs(self, args, node, true)) { return NULL; }
    return PyBool_FromLong((bool) node);
}

/**
 * XConfig.reload()
 * Check for file content changes
 * @param self
 * @param args
 * @return
 */
static PyObject* xconfig_reload(XConfigObject *self, PyObject *args) {
    self->xc->reload();
    return Py_BuildValue("z", NULL);
}

/**
 * XConfig.close()
 * Close xconfig connection
 * @param self
 * @param args
 * @return
 */
static PyObject* xconfig_close(XConfigObject *self, PyObject *args) {
    self->xc->close();
    return Py_BuildValue("z", NULL);
}

static PyMethodDef XConfigMethods[] = {
        {NULL}
};

static PyMethodDef XConfigObjectMethods[] = {
        {"reload",  (PyCFunction) xconfig_reload, METH_VARARGS,
                        "Reload XConfig connection."},
        {"getValue",  (PyCFunction) xconfig_getValue, METH_VARARGS,
                        "Gets a config value for a node specification."},
        {"getChecksum",  (PyCFunction) xconfig_getChecksum, METH_VARARGS,
                "Gets a numeric hash for a node."},
        {"exists",  (PyCFunction) xconfig_exists, METH_VARARGS,
                "Checks if a node exists."},
        {"close",  (PyCFunction) xconfig_close, METH_VARARGS,
                "Close XConfig connection."},
        {NULL, NULL, 0, NULL}        /* Sentinel */
};

static PyTypeObject xconfig_XConfigType = {
        PyVarObject_HEAD_INIT(NULL, 0)
        "xconfig.XConfig",         /* tp_name */
        sizeof(XConfigObject),     /* tp_basicsize */
        0,                         /* tp_itemsize */
        (destructor) XConfig_dealloc, /* tp_dealloc */
        0,                         /* tp_print */
        0,                         /* tp_getattr */
        0,                         /* tp_setattr */
        0,                         /* tp_compare */
        0,                         /* tp_repr */
        0,                         /* tp_as_number */
        0,                         /* tp_as_sequence */
        0,                         /* tp_as_mapping */
        0,                         /* tp_hash */
        0,                         /* tp_call */
        0,                         /* tp_str */
        0,                         /* tp_getattro */
        0,                         /* tp_setattro */
        0,                         /* tp_as_buffer */
        Py_TPFLAGS_DEFAULT,        /* tp_flags */
        "XConfig object",          /* tp_doc */
        0,                         /* tp_traverse */
        0,                         /* tp_clear */
        0,                         /* tp_richcompare */
        0,                         /* tp_weaklistoffset */
        0,                         /* tp_iter */
        0,                         /* tp_iternext */
        XConfigObjectMethods,      /* tp_methods */
        0,                         /* tp_members */
        0                ,         /* tp_getset */
        0,                         /* tp_base */
        0,                         /* tp_dict */
        0,                         /* tp_descr_get */
        0,                         /* tp_descr_set */
        0,                         /* tp_dictoffset */
        (initproc) XConfig_init,   /* tp_init */
        0,                         /* tp_alloc */
        PyType_GenericNew,         /* tp_new */
};

struct module_state {
    PyObject *error;
};

#define GETSTATE(m) ((struct module_state*)PyModule_GetState(m))


static int xconfig_traverse(PyObject *m, visitproc visit, void *arg) {
    Py_VISIT(GETSTATE(m)->error);
    return 0;
}

static int xconfig_clear(PyObject *m) {
    Py_CLEAR(GETSTATE(m)->error);
    return 0;
}


static struct PyModuleDef moduledef = {
        PyModuleDef_HEAD_INIT,
        "xconfig",
        NULL,
        sizeof(struct module_state),
        XConfigMethods,
        NULL,
        xconfig_traverse,
        xconfig_clear,
        NULL
};

#define INITERROR return NULL

PyMODINIT_FUNC PyInit_xconfig(void) {
    PyObject *m;

    m = PyModule_Create(&moduledef);
    if (m == NULL) {
        return NULL;
    }

    if (PyType_Ready(&xconfig_XConfigType) < 0)
        return NULL;

    XConfigNotFoundException = PyErr_NewException("xconfig.XConfigNotFoundException", NULL, NULL);
    Py_INCREF(XConfigNotFoundException);
    PyModule_AddObject(m, "XConfigNotFoundException", XConfigNotFoundException);

    XConfigWrongTypeException = PyErr_NewException("xconfig.XConfigWrongTypeException", NULL, NULL);
    Py_INCREF(XConfigWrongTypeException);
    PyModule_AddObject(m, "XConfigWrongTypeException", XConfigWrongTypeException);

    XConfigNotConnectedException = PyErr_NewException("xconfig.XConfigNotConnectedException", NULL, NULL);
    Py_INCREF(XConfigNotConnectedException);
    PyModule_AddObject(m, "XConfigNotConnectedException", XConfigNotConnectedException);

    Py_INCREF(&xconfig_XConfigType);
    PyModule_AddObject(m, "XConfig", (PyObject *) &xconfig_XConfigType);

    return m;
}

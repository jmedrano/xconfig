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
            PyErr_SetString(XConfigWrongTypeException, "Wrong type for key");
            return NULL;
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
            PyErr_SetString(XConfigNotConnectedException, "XConfig not connected");
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
    if (!self->xc) {
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

        node = self->xc->getNodeNoThrow(keys);
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

/**
 * XConfig.getMTime(key)
 * Get the modification timestamp of a key
 * @param self
 * @param args
 * @return the timestamp as a tuple (sec, nsec)
 */
static PyObject* xconfig_getMTime(XConfigObject *self, PyObject *args) {
    if (!self->xc) {
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

        node = self->xc->getNodeNoThrow(keys);
        if (!node) {
            PyErr_SetString(XConfigNotFoundException, "Key not found");
            return NULL;
        }

        timespec ts = self->xc->getMtime(node);
        return PyTuple_Pack(2, PyInt_FromLong(ts.tv_sec), PyInt_FromLong(ts.tv_nsec));
    }

    PyErr_SetString(XConfigWrongTypeException, "Wrong type for key");
    return NULL;
}

/**
 * XConfig.reload()
 * Check for file content changes
 * @param self
 * @param args
 * @return
 */
static PyObject* xconfig_reload(XConfigObject *self, PyObject *args) {
    if (!self->xc) {
        PyErr_SetString(XConfigNotConnectedException, "XConfig not connected");
        return NULL;
    }
    self->xc->reload();
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
        {"getMTime",  (PyCFunction) xconfig_getMTime, METH_VARARGS,
                        "Gets a tuple (secs, nsecs) with the modification time of a key."},
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

PyMODINIT_FUNC initxconfig(void) {
    PyObject *m;

    m = Py_InitModule("xconfig", XConfigMethods);
    if (m == NULL) {
        return;
    }

    if (PyType_Ready(&xconfig_XConfigType) < 0)
        return;

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
}

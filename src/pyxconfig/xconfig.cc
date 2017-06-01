#include <Python.h>
#include <xconfig/xconfig.h>
#include <iostream>

using std::string;
using std::vector;
using xconfig::XConfig;
using xconfig::XConfigNode;
using xconfig::UnixConnection;

static XConfig* xc = NULL;

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
        for (std::vector<int>::size_type i = 0; i < children.size(); i++)
            PyList_Append(list, getValue(children[i]));
        //for (XConfigNode child : node.getChildren())
        //    PyList_Append(list, getValue(child));
        return list;
    }
    case xconfig::TYPE_MAP: {
        PyObject *dict = PyDict_New();
		std::vector<XConfigNode> children = node.getChildren();
        for (std::vector<int>::size_type i = 0; i < children.size(); i++)
            PyDict_SetItemString(dict, children[i].getName().c_str(), getValue(children[i]));
        return dict;
    }
    default:
    //    throw xc_getXConfigWrongTypeException("Unknown node type");
        return NULL;
    }
}

static PyObject* xconfig_init(PyObject *self, PyObject *args) {
	char *path;
	PyArg_ParseTuple(args, "s", &path);
	const boost::shared_ptr<UnixConnection> unix_connection = boost::shared_ptr<UnixConnection>(new UnixConnection(path, ""));
	xc = new XConfig(unix_connection, true);
	return Py_BuildValue("z", NULL);
}

static PyObject* xconfig_test(PyObject *self, PyObject *args) {
	return Py_BuildValue("i", 42);
}

static PyObject* xconfig_getValue(PyObject *self, PyObject *args) {
	char* key;
    XConfigNode node;
	PyArg_ParseTuple(args, "s", &key);
    node = xc->getNodeNoThrow(string(key));
    return getValue(node);
}


//static PyObject* xconfig_getMTime(PyObject *self, PyObject *args) {
//}

//static PyObject* xconfig_reload(PyObject *self, PyObject *args) {
//}

//static PyObject* xconfig_getMTime(PyObject *self, PyObject *args) {
//}

//static PyObject* xconfig_getMTime(PyObject *self, PyObject *args) {
//}

static PyMethodDef XConfigMethods[] = {
    {"init",  xconfig_init, METH_VARARGS,
     "Initialize XConfig module."},
    {"test",  xconfig_test, METH_VARARGS,
     "Test method."},
    {"getValue",  xconfig_getValue, METH_VARARGS,
     "Gets a config value for a node specification."},
    {NULL, NULL, 0, NULL}        /* Sentinel */
};

PyMODINIT_FUNC initxconfig(void) {
    (void) Py_InitModule("xconfig", XConfigMethods);
}


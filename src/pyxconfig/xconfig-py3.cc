#include <Python.h>

static PyObject* xconfig_getValue(PyObject *self, PyObject *args) {
	return PyLong_FromLong(42);
}

static PyMethodDef XConfigMethods[] = {
    {"getValue",  xconfig_getValue, METH_VARARGS,
     "Gets a config value for a node specification."},
    {NULL, NULL, 0, NULL}        /* Sentinel */
};

static struct PyModuleDef xconfig_module = {
   PyModuleDef_HEAD_INIT,
   "xconfig",   /* name of module */
   NULL, /* module documentation, may be NULL */
   -1,       /* size of per-interpreter state of the module,
                or -1 if the module keeps state in global variables. */
   XConfigMethods
};

PyMODINIT_FUNC PyInit_xconfig(void) {
    return PyModule_Create(&xconfig_module);
}


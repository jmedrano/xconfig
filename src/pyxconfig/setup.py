from distutils.core import setup, Extension

module1 = Extension('xconfig',
                    sources = ['xconfig.cc'],
                    libraries = ['xconfig'],
                    extra_compile_args = ['-std=c++0x'],
                    extra_link_args = ['-std=c++0x'],
                    )

setup (name = 'xconfig',
       version = '1.0',
       description = 'XConfig extension',
       ext_modules = [module1])

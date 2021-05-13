from setuptools import setup, Extension

module1 = Extension('xconfig',
                    sources=['xconfig.cc'],
                    include_dirs=['/source/src/libxconfig/'],
                    libraries=['xconfig', 'boost_system'],
                    extra_compile_args=['-std=c++0x'],
                    extra_link_args=['-std=c++0x'],
                   )

setup(name='xconfig',
      version='1.0.6',
      author="Infrastructure Team",
      author_email="novum-infrastructure@telefonica.com",
      description='XConfig extension',
      url="https://github.com/Telefonica/xconfig",
      ext_modules=[module1])

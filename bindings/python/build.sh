#!/bin/bash

dpkg -i /source/packages/libxconfig*.deb

apt update
apt-get -y install python3-pip
pip3 install --user --upgrade setuptools wheel
python3 setup.py sdist bdist_wheel

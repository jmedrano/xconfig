#!/bin/bash

dpkg -i /source/packages/libxconfig*.deb

apt update
apt-get -y install python3-pip
pip3 install --user --upgrade setuptools wheel
python3 setup.py sdist bdist_wheel

# Instructions to get a wheel for python 3.8
#apt install wget https://people.debian.org/~paravoid/python-all/unofficial-python-all.asc
#wget https://people.debian.org/~paravoid/python-all/unofficial-python-all.asc
#echo "deb http://people.debian.org/~paravoid/python-all $(lsb_release -sc) main" | tee /etc/apt/sources.list.d/python-all.list
#apt update
#apt install python3.8
#python3.8 setup.py sdist bdist_wheel

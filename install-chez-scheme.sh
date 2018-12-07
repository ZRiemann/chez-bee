#!/bin/bash

echo "MIT License"
echo "Copyright (c) 2018 Z.Riemann"
echo "https://github.com/ZRiemann/chez-bee"
echo

display-scheme-information(){
    echo 
    echo "scheme directory:"
    which scheme
    echo
    echo "scheme version: $version"
    scheme --version
    echo
}

if [ "$(type -t scheme)" = "file" ]; then
    echo "Chez Scheme already installed."
    display-scheme-information
    exit 0
fi

git clone "https://github.com/cisco/ChezScheme.git"
cd ChezScheme
./configure
make -j4
sudo make install

echo "Chez Scheme install done."
display-scheme-information

exit 0

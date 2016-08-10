#!/bin/bash

set -e
set -o pipefail

echo '[setup] Checking to see if version 1.6 or newer of the JDK is installed.'

if javac -version 2>&1 \
       | egrep 'javac (1\.([6-9]|[1-9][0-9]+)|[2-9]|[1-9][0-9]+)'; then
    echo '[setup] It looks like an appropriate version of the JDK is already installed.'
else
    echo '[setup] It looks like an appropriate version of the JDK is not installed.'
    echo '[setup] Please ignore the dialog box.'
    read -p '[setup] Press RET to continue.'
    echo '[setup] Downloading version 1.8u102 of the JDK from download.oracle.com.'
    # Specifying the filename makes wget override any existing file by that name
    wget -O jdk-8u102-macosx-x64.dmg --no-check-certificate --no-cookies --header "Cookie: oraclelicense=accept-securebackup-cookie" http://download.oracle.com/otn-pub/java/jdk/8u102-b14/jdk-8u102-macosx-x64.dmg
    echo '[setup] Mounting the JDK disk image.'
    hdiutil mount jdk-8u102-macosx-x64.dmg
    echo '[setup] Running the JDK installer.'
    open "/Volumes/JDK 8 Update 102/JDK 8 Update 102.pkg"
    echo '[setup] Please use the dialog window to install the JDK.'
    sleep 3
    read -p '[setup] Please press RET when you have finished installing the JDK.'
    echo '[setup] Unmounting the JDK disk image.'
    hdiutil unmount "/Volumes/JDK 8 Update 102"
    echo '[setup] Deleting the JDK disk image.'
    rm jdk-8u102-macosx-x64.dmg
fi

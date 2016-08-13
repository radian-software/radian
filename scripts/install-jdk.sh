#!/bin/bash

set -e
set -o pipefail

echo "[install-jdk] Please ignore any dialog box that may have appeared."
read -p "[install-jdk] Press RET to continue."
echo "[install-jdk] Downloading version 1.8u102 of the JDK from download.oracle.com."
# Specifying the filename makes wget override any existing file by that name
wget -O jdk-8u102-macosx-x64.dmg --no-check-certificate --no-cookies --header "Cookie: oraclelicense=accept-securebackup-cookie" http://download.oracle.com/otn-pub/java/jdk/8u102-b14/jdk-8u102-macosx-x64.dmg
echo "[install-jdk] Mounting the JDK disk image."
hdiutil mount jdk-8u102-macosx-x64.dmg
echo "[install-jdk] Running the JDK installer."
open "/Volumes/JDK 8 Update 102/JDK 8 Update 102.pkg"
echo "[install-jdk] Please use the dialog window to install the JDK."
sleep 3
read -p "[install-jdk] Please press RET when you have finished installing the JDK."
echo "[install-jdk] Unmounting the JDK disk image."
hdiutil unmount "/Volumes/JDK 8 Update 102"
echo "[install-jdk] Deleting the JDK disk image."
rm jdk-8u102-macosx-x64.dmg

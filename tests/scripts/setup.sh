#!/bin/bash

set -e

echo "Setting up CardDemo test environment..."

sudo apt-get update
sudo apt-get install -y build-essential wget tar gzip libdb-dev libgmp-dev libncurses5-dev flex bison

echo "Installing GnuCOBOL from source..."
ORIGINAL_DIR=$(pwd)
cd /tmp
wget -q https://ftp.gnu.org/gnu/gnucobol/gnucobol-3.2.tar.gz
tar -xzf gnucobol-3.2.tar.gz
cd gnucobol-3.2
./configure --prefix=/usr/local
make -j$(nproc)
sudo make install
sudo ldconfig
cd "$ORIGINAL_DIR"

mkdir -p carddemo-test/data/vsam carddemo-test/data/ps carddemo-test/data/init \
         carddemo-test/programs/compiled carddemo-test/programs/source \
         carddemo-test/copybooks carddemo-test/logs carddemo-test/temp carddemo-test/scripts

export CARDDEMO_HOME=$(pwd)/carddemo-test
export COB_LIBRARY_PATH=$CARDDEMO_HOME/copybooks
export COB_CONFIG_DIR=/usr/local/share/gnucobol/config

mkdir -p $CARDDEMO_HOME/data/vsam/ACCTDATA $CARDDEMO_HOME/data/vsam/CARDDATA \
         $CARDDEMO_HOME/data/vsam/CUSTDATA $CARDDEMO_HOME/data/vsam/TRANSACT \
         $CARDDEMO_HOME/data/vsam/CARDXREF $CARDDEMO_HOME/data/vsam/USRSEC \
         $CARDDEMO_HOME/data/vsam/DISCGRP $CARDDEMO_HOME/data/vsam/TRANCATG \
         $CARDDEMO_HOME/data/vsam/TRANTYPE $CARDDEMO_HOME/data/vsam/TCATBALF

echo "Environment setup complete. CARDDEMO_HOME=$CARDDEMO_HOME"

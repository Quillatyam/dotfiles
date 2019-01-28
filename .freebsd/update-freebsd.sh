#!/bin/sh

LOG_FILE="$HOME/freebsd-update.log"

echo "Starting updates: `date`" | tee ${LOG_FILE}
echo "*****************"
echo "Checking for FreeBSD patches"

freebsd-update fetch | tee ${LOG_FILE}
freebsd-update install | tee ${LOG_FILE}

echo "*****************"

echo "Updating ports tree"

portsnap fetch update | tee ${LOG_FILE}

echo "*****************"

echo "Updating Ports"

pkg version -l '<' | tee ${LOG_FILE}
pkg upgrade | tee ${LOG_FILE}

echo "*****************"

echo "Checking installed ports for known security problems"
pkg audit -F | tee ${LOG_FILE}
echo "Finished updates: `date`" | tee ${LOG_FILE}

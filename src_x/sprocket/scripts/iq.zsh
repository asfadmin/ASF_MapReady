#!/bin/bash
CLASSPATH=/ASF/dev/java:.; export CLASSPATH

# Check for no args
if [ $# -ne "1" ]
then
   echo  "Usage: iq.zsh <base name>"
   exit 1
fi

java -Xmx100000000000 pt_anal 64 16 $1

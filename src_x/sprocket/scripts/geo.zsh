#!/bin/bash
CLASSPATH=/ASF/current/java:.; export CLASSPATH

# Check for no args
if [ $# -ne "1" ]
then
   echo  "Usage: iq.zsh <base name>"
   exit 1
fi

java MeasureGeoLocationErrors $1

#!/bin/bash
CLASSPATH=/ASF/current/java:.; export CLASSPATH

# Check for no args
if [ $# -ne "1" ]
then
   echo  "Usage: target_selector.zsh  <image root>"
   exit 1
fi

##
# Select Targets
##
java MainTargetSelection $1

##
# Locate Targets
##
java Locater $1

#!/bin/sh

# To dynamically figure the path to all our Java classes we use perl to parse
# up the absolute path to this script ($0), and lop off everything after
# 'asf_tools'. We can then append the /java directory to our newly found path
# to asf_tools, and blamo, we've got a classpath. Very ugly, yes, I agree.
# example:
#    $0 = /usr/local/asf_tools/bin/linux/script_name
#    perl returns /usr/local/asf_tools
#    this script appends /java
#    and BLAMO, our classpath is /usr/local/asf_tools/java
perl_command='perl -p -e '"'"'s/((\/|\w)+asf_tools)+(\/|\w)*/$1/;'"'"
CLASSPATH_TEMP=`echo $0 | eval $perl_command`
CLASSPATH="${CLASSPATH_TEMP}/java"
export CLASSPATH

# Check for no args
if [ $# -ne "1" ]
then
   echo ""
   echo "Usage: target_selector <image root>"
   echo ""
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

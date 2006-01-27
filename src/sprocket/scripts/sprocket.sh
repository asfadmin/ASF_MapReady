#!/bin/sh

# Dynamically get the path to all our Java classes.
# Take the absolute path to this scipt ($0), and work backwards to the tools
# base directory, then append the /java directory. Gross hack, but it works.
# Example
#    $0 = /usr/local/asf_tools/bin/script_name
#    dirname returns /usr/local/asf_tools/bin
#    dirname returns /usr/local/asf_tools
#    then append /java
#    and BLAMO, our classpath is /usr/local/asf_tools/java
tempDir=`dirname $0`
tempDir=`dirname $tempDir`
CLASSPATH=${tempDir}/java
export CLASSPATH

# Call pvs (actually sprocket, but it was called pvs to begin with)
java pvs

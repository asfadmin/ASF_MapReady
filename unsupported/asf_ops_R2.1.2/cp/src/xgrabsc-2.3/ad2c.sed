#========================================================================
#
# Name - ad2c.sed
#
# Version:	1.2
#
# ccsid:	@(#)ad2c.sed	1.2 - 07/09/92 12:48:17
# from: 	ccs/s.ad2c.sed
# date: 	06/28/93 09:14:48
#
#  Name - ad2c.sed, part of the ad2c package by George Ferguson
#
#  Description:
#
#	Convert app-defaults file to C strings decls.
#
#	Invoke by: sed -n -f ad2c.sed
#
#
#  This is part of an older version of the ad2c package.
#  If you need to create fallback resources from .ad files, get the full
#  package.
#
# ========================================================================

: read
# remove comments
/^!/d
/^#/d
# remove blanks
/^$/d
# escape quotes
s/"/\\"/g
# escape backslash
s/\\/\\\\/g
# except the line continuation ones
s/\\$//g
# add leading quote
s/^/"/
#
: test
/\\$/b slash
s/$/",/
p
n
b read
#
: slash
p
n
# just like "read" only doesn't add leading quote
/^!/d
/^$/d
s/"/\\"/g
s/\\\\/\\/g
s/\\n/\\\\n/g
s/\\t/\\\\t/g
s/\\f/\\\\f/g
s/\\b/\\\\b/g
b test

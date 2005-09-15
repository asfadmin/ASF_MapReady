# popup map.tcl
#
# This section of code tries to display the current location on
# a SAR image in the Microsoft Expedia map at terraserver.
#  Note that ASF-STEP isn't affiliated with Microsoft in any way,
# so this might break randomly.
#
# The program passes this URL to a web browser:
#  http://terraserver.microsoft.com/GetSearchByMap.asp?
#      Longitude=<lon>&Latitude=<lat>&Altitude=200&DSize=1&ImgDate=1/1/1999
#
# Orion Sky Lawlor, 5/20/99
#

################## viewPixelMap ###################

proc viewPixelMap {x y} {
# Get the point's latitude and longitude
	set lat 0
	set lon 0
	if {0==[cproc_pointloc $x $y lat lon]} {return}

# Create the destination URL
	set URL "http://terraserver.microsoft.com/GetSearchByMap.asp?Longitude="
	append URL $lon
	append URL "&Latitude="
	append URL $lat
	append URL "&Altitude=200&Dsize=1&ImgDate=1/1/1999"
	
# Call a web browser with the URL
	global tcl_platform
	if {$tcl_platform(platform) == "windows"} {
# Windows machines tend to use (or at least have) Internet Exploder
		exec "iexplore" "$URL" &
	} elseif {$tcl_platform(platform) == "macintosh"} {
# Macs don't do exec
	} else {
# UNIX machines use Netscape navigator (mostly)
		exec "netscape" "$URL" &
	}
}

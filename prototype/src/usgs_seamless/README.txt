This is a tool to download a zipped DEM from the USGS "Seamless" site.

You run it with a latitude and longitude range like this:
	akdem_grab 63.1 64.5 -146.3 -143.5

By default it grabs the Alaska 2-arcsecond NED, but for the lower 48
you can also grab NED1 or SRTM1, which are both 1-arcsecond resolution.

The "zip2img.sh" UNIX Bourne shell script automates the process of unpacking
the resulting zip file, and converting it to ASF format.


At the moment, this tool requires only the osl socket and webservice routines,
not any of the plugin stuff.


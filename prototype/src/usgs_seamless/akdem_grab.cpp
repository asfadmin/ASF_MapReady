/**
 Grab a DEM of Alaska from the USGS "Seamless" site:
	http://seamless.usgs.gov/

This routine uses raw sockets to grab the elevation data
right off the USGS web server.  Underneath, it makes the same
requests as your browser, but you doesn't have to do all the 
clicking through the Seamless GUI.

Orion Sky Lawlor, olawlor@acm.org, 2006/07/11 (ASF)
*/
#include "osl/socket.h"
#include "osl/webservice.h"
/* include implementations right here for easier linking */
#include "osl/socket.cpp"
#include "osl/webservice.cpp"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>


void bail(const char *why) {
	fprintf(stderr,"FATAL ERROR> %s\n",why);
	exit(1);
}

/**
 A latitude/longitude range, which identifies a little box
 on the Earth's surface.  Latitudes and longitudes are in 
 ordinary decimal degrees.
*/
class lat_lon_box {
public:
	double lat_lo, lat_hi;
	double lon_lo, lon_hi;
	lat_lon_box(double lat_lo,double lat_hi, double lon_lo,double lon_hi);
};

lat_lon_box::lat_lon_box(double lat_lo_,double lat_hi_, double lon_lo_,double lon_hi_)
{
	lat_lo=lat_lo_; lat_hi=lat_hi_;
	lon_lo=lon_lo_; lon_hi=lon_hi_;
	
}

/* Forward declaration */
std::string download_usgs_seamless(
	const std::string postData,
	osl::network_progress &p);

/**
 Format this latitude/longitude box in the way seamless.usgs.gov expects.
*/
std::string lat_lon_to_seamless(const lat_lon_box &b)
{
	char req[1000];
	if (b.lat_lo>90.0 || b.lat_hi<-90.0) bail("download_usgs_seamless latitude and longitude interchanged");
	if (b.lon_lo>=b.lon_hi) bail("download_usgs_seamless longitude range inverted: lo>=hi");
	if (b.lat_lo>=b.lat_hi) bail("download_usgs_seamless latitude range inverted: lo>=hi");
	if (b.lon_hi-b.lon_lo>5.0) bail("download_usgs_seamless longitude range absurdly huge");
	if (b.lat_hi-b.lat_lo>5.0) bail("download_usgs_seamless latitude range absurdly huge");
	
	sprintf(req,"lft=%.14f&rgt=%.14f&top=%.14f&bot=%.14f",b.lon_lo,b.lon_hi,b.lat_hi,b.lat_lo);
	return req;
}

/**
 Download this latitude-longitude region of the Alaska 2 arc-second NED elevations.  
 Returns the downloaded .zip file *data*.
*/
std::string download_usgs_seamless_akned(
	const lat_lon_box &b,
	osl::network_progress &p) 
{
	p.status(0,"Downloading Alaska 2 arc-second NED elevations");
	return download_usgs_seamless(
		"siz=99&key=NAK&ras=1&rsp=1&pfm=GeoTIFF&imsurl=-1&ms=-1&att=-1&lay=-1&fid=-1&dlpre=&"
		+lat_lon_to_seamless(b)+
		"&wmd=1&mur=http%3A%2F%2Fextract.cr.usgs.gov%2Fdistmeta%2Fservlet%2Fgov.usgs.edc.MetaBuilder&mcd=NED&mdf=HTML&arc=ZIP&sde=ned.ak_ned&msd=NED.CONUS_NED_METADATA&zun=METERS&prj=0&csx=5.55555555556E-4&csy=5.55555555556E-4&bnd=&bndnm=",
		p);
}

/**
 Download this latitude-longitude region of the 1.0 arc-second NED elevations.  
 Returns the downloaded .zip file *data*.
*/
std::string download_usgs_seamless_ned1(
	const lat_lon_box &b,
	osl::network_progress &p) 
{
	p.status(0,"Downloading 1 arc-second NED elevations");
	return download_usgs_seamless(
		"siz=16&key=NED&ras=1&rsp=1&pfm=GeoTIFF&imsurl=-1&ms=-1&att=-1&lay=-1&fid=-1&dlpre=NED_&"
		+lat_lon_to_seamless(b)+
		"&wmd=1&mur=http%3A%2F%2Fextract.cr.usgs.gov%2Fdistmeta%2Fservlet%2Fgov.usgs.edc.MetaBuilder&mcd=NED&mdf=HTML&arc=ZIP&sde=NED.conus_ned&msd=NED.CONUS_NED_METADATA&zun=METERS&prj=0&csx=2.777777777999431E-4&csy=2.777777777999431E-4&bnd=&bndnm=",
		p);
}

/**
 Download this latitude-longitude region of the 1.0 arc-second SRTM elevations.  
 Returns the downloaded .zip file *data*.
*/
std::string download_usgs_seamless_srtm1(
	const lat_lon_box &b,
	osl::network_progress &p) 
{
	p.status(0,"Downloading 1 arc-second SRTM elevations");
	return download_usgs_seamless(
		"siz=17&key=SM3&ras=1&rsp=1&pfm=GeoTIFF&imsurl=-1&ms=-1&att=-1&lay=-1&fid=-1&dlpre=&"
		+lat_lon_to_seamless(b)+
		"&wmd=1&mur=http%3A%2F%2Fextract.cr.usgs.gov%2Fdistmeta%2Fservlet%2Fgov.usgs.edc.MetaBuilder&mcd=SRTM1FIN&mdf=HTML&arc=ZIP&sde=SRTM.C_US_1_ELEVATION&msd=SRTM.c_national_1_elevation_meta&zun=&prj=0&csx=2.777777778000001E-4&csy=2.777777778000001E-4&bnd=&bndnm=",
		p);
}


/**
 Download this POST request data from the USGS seamless site
 to zip-formatted data.   You can figure out how the POST format works
 by using a packet sniffer (like Ethereal) while hitting "Download"
 at the "USGS Seamless Request Summary Page" after selecting a layer and
 drawing a region on seamless.usgs.gov.  
 
 Returns the downloaded .zip format *data*.
*/
std::string download_usgs_seamless(
	const std::string postData,
	osl::network_progress &p)
{
	std::string host="extract.cr.usgs.gov";
	int hostPort=80; /* http port */
	std::string postURL="/diststatus/servlet/gov.usgs.edc.RequestStatus";
	
	std::string agent="ASF Tools Seamless Downloader; http://asf.alaska.edu/; ffosl@uaf.edu";
	//agent="Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.8.0.4) Gecko/20060508 Firefox/1.5.0.4"; // stealth mode!
	
/* Send HTTP "POST" request off to server, which begins the extraction process */
	osl::http_connection post(host,p,hostPort);
	char lenStr[100]; sprintf(lenStr,"%d",postData.size());
	post.send("POST "+postURL+" HTTP/1.1\r\n"
		"Host: "+host+"\r\n"
		"User-Agent: "+agent+"\r\n"
		"Content-Type: application/x-www-form-urlencoded\r\n"
		"Content-Length: "+(std::string)lenStr+"\r\n"
		"\r\n"
		+postData
	);
	/* The status page is linked from the "Refresh" HTTP header line */
	std::string refreshURL=post.receive_header("Refresh");
	refreshURL=refreshURL.substr(refreshURL.find_first_of("=")+1,std::string::npos);
	post.receive(); /* Grab (and ignore!) the post web page body */
	post.close();
	
/* Keep polling on the refresh page until it returns 302 status-- that'll be our .zip file! */
	while (1) {
		p.status(1,"Waiting for server to prepare data");
		sleep(5); /* Don't hammer the server with status requests-- give it time... */
		osl::http_connection status(host,p,hostPort);
		int code=status.send_get(refreshURL,agent);
		if (code==302)  /*  "HTTP/1.1 302 Moved Temporarily" */
		{ /* This is it!  The real .zip data!*/
			std::string zipURL=status.receive_header("Location");
			return download_url(zipURL,p);
		}
		else { /* Grab status; keep looping until we get the real data */
			status.receive();
		}
	}
}

class printf_progress : public osl::network_progress {
public:
	virtual void status(int verbosity, const std::string &what) {
		if (verbosity>3) return; /* don't bother with really verbose stuff */
		for (int i=0;i<verbosity;i++)
		  fprintf(stderr,"  ");
		fprintf(stderr,"%s\n",what.c_str());
	}
};

int main(int argc,char *argv[]) {
	printf_progress p;
	std::string d;
	if (argc==2) { /* simple http request */
	  d=osl::download_url(argv[1],p);
	} else if (argc>4) { /* USGS download */
	  double latLo,latHi, lonLo,lonHi;
	  latLo=atof(argv[1]); latHi=atof(argv[2]);
	  lonLo=atof(argv[3]); lonHi=atof(argv[4]);
	  printf("Grabbing data from %f to %f deg lat, %f to %f deg lon\n",latLo,latHi,lonLo,lonHi);
	  lat_lon_box b(latLo,latHi,lonLo,lonHi);
	  if (argc>5 && 0==strcmp(argv[5],"NED1"))
		  d=download_usgs_seamless_ned1(b,p);
	  else if (argc>5 && 0==strcmp(argv[5],"SRTM1"))
		  d=download_usgs_seamless_srtm1(b,p);
	  else
		  d=download_usgs_seamless_akned(b,p);
	} else {
	  printf("Usage: akdem_grab <lat lo> <lat hi>  <lon lo> <lon hi> [ NED1 | SRTM1 ]\n");
	  return 0;
	}
	FILE *f=fopen("saved.zip","wb");
	fwrite(&d[0],d.size(),1,f);
	fclose(f);
	printf("Wrote %d bytes of data to saved.zip\n",d.size());
	return 0;
}

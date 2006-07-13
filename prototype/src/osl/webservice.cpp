/**
 Crappy, but tiny and fully flexible web client interface.
 You can get access to the HTTP headers with this interface.
 
 Orion Sky Lawlor, olawlor@acm.org, 2006/07/11 (Public Domain)
*/
#include "osl/webservice.h"

/** Parse URL into its component parts */
osl::url_parser::url_parser(std::string URL) {
	while (isspace(URL[0])) URL=URL.substr(1,std::string::npos);
	int ds=URL.find_first_of("//"); /* double-slash */
	if (ds!=std::string::npos) {
		protocol=URL.substr(0,ds-1);
		ds+=2; /* skip over double-slash, so ds points at start of host */
	} else { /* no double-slash: no protocol */
		ds=0;
		protocol="http";
	}
	int ps=URL.find_first_of("/",ds);
	host=URL.substr(ds,ps-(ds));
	unsigned int colon=host.find_first_of(":");
	port=80;
	if (colon!=std::string::npos) { // Parse port number, like www.foo.com:80
		sscanf(&host[colon+1],"%d",&port);
		host=host.substr(0,colon);
	}
	path=URL.substr(ps,std::string::npos);
}

/**
 Retrieve HTTP data from this URL. 
*/
std::string osl::download_url(std::string URL,network_progress &p) {
	osl::url_parser pu(URL);
	http_connection c(pu.host,p,pu.port);
	c.send_get(pu.path);
	return c.receive();
}

/** Initiate an HTTP connection */
osl::http_connection::http_connection(std::string host_,network_progress &p_,int port,int timeout)
	:host(host_), p(p_), s(0)
{
	p.status(1,"Looking up IP address for "+host);
	skt_ip_t hostIP=skt_lookup_ip(host.c_str());
	p.status(1,"Connecting to "+host);
	s=skt_connect(hostIP,port,timeout);
}

/** Send a complete HTTP request, with all HTTP headers prebuilt.
    Returns HTTP status. 
*/
int osl::http_connection::send(const std::string &data)
{
	p.status(1,"Sending HTTP request to "+host);
	p.status(3,"HTTP request data: "+data);
	skt_sendN(s,&data[0],data.size());
	
	p.status(1,"Waiting for HTTP response from "+host);
	std::string status=skt_recv_line(s);
	p.status(2,"HTTP response status: "+status);
	unsigned int i=0; while (status[i]!=' ' && i<status.size()) i++;
	std::string codeDesc=status.substr(i,std::string::npos);
	int code=0;
	sscanf(codeDesc.c_str(),"%d",&code);
	
	std::string l;
	while (0!=(l=skt_recv_line(s)).size()) 
	{   /* ^ a zero-length line indicates the end of the HTTP headers */
		p.status(3,"HTTP response header line: "+l);
		int firstColon=l.find_first_of(":");
		std::string keyword=l.substr(0,firstColon);
		std::string value=l.substr(firstColon+2,std::string::npos);
		header[keyword]=value;
	}
	
	return code;
}
/** Send a simple HTTP GET request for this path name. */
int osl::http_connection::send_get(const std::string &path,std::string agent)
{
	return send("GET "+path+" HTTP/1.1\r\n"
		"Host: "+host+"\r\n"
		"User-Agent: "+agent+"\r\n"
		"\r\n");
}
std::string int2str(int i) {
	char buf[100];
	sprintf(buf,"%d",i);
	return buf;
}
int my_min(int a,int b) { if (a<b) return a; else return b; }

/** Receive all HTTP content as a single std::string */
std::string osl::http_connection::receive(void) 
{
	std::string lengthStr=receive_header("Content-Length");
	int length=0;
	sscanf(lengthStr.c_str(),"%d",&length);
	
	p.status(1,"Retrieving "+int2str(length/1024)+" KiB of HTTP data from "+host);
	std::string data(length,0x00);
	enum {chunkSize=8*1024};
	for (int start=0;start<length;start+=chunkSize) {
		int amt=my_min(data.size()-start,chunkSize);
		skt_recvN(s,&data[start],amt);
		p.status(2,"Retrieved "+int2str((start+amt)/1024)+" KiB so far from "+host);
	}
	if (length<1024)
		p.status(3,"Incoming data: "+data);
	return data;
}
/** Receive the data on the line with this HTTP header keyword */
std::string osl::http_connection::receive_header(const std::string &keyword)
{
	const std::string &value=header[keyword];
	p.status(2,"HTTP header keyword "+keyword+" has value "+value);
	return value;
}


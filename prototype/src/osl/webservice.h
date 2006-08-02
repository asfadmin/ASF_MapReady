/**
 Crappy, but tiny and fully flexible web client interface.
 You can get access to the HTTP headers with this interface.
 
 Orion Sky Lawlor, olawlor@acm.org, 2006/07/11 (Public Domain)
*/
#ifndef __OSL_WEBSERVICE_H
#define __OSL_WEBSERVICE_H

#include "osl/dll.h"
#include "osl/socket.h"
#include <string>
#include <map>

namespace osl {

/**
 A silly little status-reporter class.
 If you don't care about status, 
 	network_progress p;
 will just ignore all status messages.
*/
class OSL_DLL network_progress {
public:
	/**
	  verbosity ranges from 0 (really important) to 
	  1 (normal status) to higher debugging values.
	  what is a human-readable description of what's happening.
	*/
	virtual void status(int verbosity,const std::string &what) {}
};

/**
 Retrieve HTTP content data from this URL.  
 You only need the more complicated classes below for more complex
 stuff like custom HTTP headers, POST requests, etc.
*/
OSL_DLL std::string download_url(std::string URL,network_progress &p);


/**
 Parse an URL, like
 	http://www.foo.com:80/bar/baz/boo.phtml?fuzzy
 into a protocol, host, port, and path.
*/
class OSL_DLL url_parser {
public:
	url_parser(std::string URL);
	std::string protocol; /** e.g., http */
	std::string host; /** e.g., www.foo.com */
	std::string path; /** e.g., /bar/baz/boo.phtml?fuzzy */
	int port; /** e.g., 80 */
};

/**
 Manage the details of an HTTP client connection.
 HTTP is the protocol used on the web.
*/
class OSL_DLL http_connection {
public:
	http_connection(std::string host,network_progress &p,int port=80,int timeoutSeconds=60);
	~http_connection() { close();}
	
	/** Send a complete HTTP request, with all HTTP headers prebuilt.
	  Returns HTTP status code, like 200 (OK).
	*/
	int send(const std::string &data);
	/** Send a simple HTTP GET request for this path name. */
	int send_get(const std::string &path,std::string agent="Mozilla/5.0 (compatible; OSL web service)");
	
	/** Receive all HTTP content as a single std::string */
	std::string receive(void);
	/** Look up the value of the HTTP header line with this keyword */
	std::string receive_header(const std::string &keyword);
	
	/** Close our TCP connection.  Happens automatically when object is destroyed,
	  and can safely be repeated. */
	void close(void) { if (s) skt_close(s); s=0; }
private:
	std::string host; /**< DNS hostname we will connect to */
	network_progress &p; /**< progress indicator */
	SOCKET s; /**< connected TCP/IP socket we talk on */
	std::map<std::string,std::string> header; /**< http header names and values */
};

};

#endif

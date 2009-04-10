/***************************************************************
SKT - simple TCP and UDP socket-based network communication routines.  
  This interface exists mostly to hide the differences between 
  UNIX Berkeley sockets (which work on BSD, Solaris, Linux, AIX, 
  Mac OS X, and others) and Windows "winsock" sockets.

 SOCKET is just a #define for "int".  It's needed because
 winsock uses a "SOCKET" type instead of an int.

 skt_ip_t is a flat bytes structure to hold an IP address--
 this is either 4 bytes (for IPv4) or 16 bytes (for IPv6).
 It is always stored in network byte order.

 All port numbers are taken and returned 
 in *host* byte order.  This means you can hardcode port
 numbers in the code normally, and they will be properly 
 translated even on little-endian machines.
 
 Errors are handled in the library by calling a user-overridable
 abort function.

Written by Orion Sky Lawlor, olawlor@acm.org 1999-2006 (Public Domain)
****************************************************************/
#ifndef __SOCK_ROUTINES_H
#define __SOCK_ROUTINES_H

/*Preliminaries*/
#include <string.h>
#if defined(_WIN32) && ! defined(__CYGWIN__)
  /*For windows systems:*/
#  include <winsock.h> /* for SOCKET and others */
   static void sleep(int secs) {Sleep(1000*secs);}
#pragma comment (lib, "wsock32.lib")  /* link with winsock library */

#else
  /*For non-windows (UNIX) systems:*/
#  include <sys/types.h>
#  include <sys/time.h>
#  include <sys/socket.h>
#  include <netinet/in.h>
#  include <arpa/inet.h>
#  include <netdb.h>
#  include <unistd.h>
#  include <fcntl.h>

#  ifndef SOCKET
#    define SOCKET int
#    define INVALID_SOCKET (SOCKET)(~0)
#    define SOCKET_ERROR (-1)
#  endif /*def SOCKET*/
#endif /*platform testing*/
/** Server sockets are the same data type as regular sockets */
#define SERVER_SOCKET SOCKET


/*************** IP Addresses and DNS ******************/

/** This is an IPv4 TCP/IP address.
  16-byte IPv6 addresses aren't supported yet,
  but could be by changing this struct and these
  routines--user code should be unchanged.
*/
typedef struct { 
	unsigned char data[4];
} skt_ip_t;

/** return the IP address of the given machine (DNS or dotted decimal).
    Calls abort on failure.
*/
skt_ip_t skt_lookup_ip(const char *name);

/** Like skt_lookup_ip, but returns _skt_invalid_ip on failure.
*/
skt_ip_t skt_lookup_invalid(const char *name);

/** This is an invalid IP address, 
returned by skt_lookup_invalid on failure. */
extern skt_ip_t _skt_invalid_ip;

/** Return the IP address of the current machine. 
  Returns _skt_invalid_ip if we were unable to determine our IP address.
*/
skt_ip_t skt_my_ip(void);

/**
  - Print the given IP address to the given character buffer as
    dotted decimal.  Dest must be at least 130 bytes long, 
    and will be returned.
*/
char *skt_print_ip(char *dest,skt_ip_t addr);

/**
  - Return 1 if the given IP addresses are identical.
*/
int skt_ip_match(skt_ip_t a,skt_ip_t b);

/**
  Utility routine: create a Berkeley sockaddr_in 
  from a TCP/IP address and port.
*/
struct sockaddr_in skt_build_addr(skt_ip_t IP,int port);


/************************* UDP Communication ********************/
/**
  Creates a UDP datagram socket on the given port.  
    Since UDP is connectionless, this socket can send or receive.
    Performs the whole socket/bind/getsockname procedure.  
    Returns the actual port of the socket and
    the file descriptor.  Bufsize, if nonzero, controls the amount
    of buffer space the kernel sets aside for the socket.
*/
SOCKET skt_datagram(unsigned int *port, int bufsize);



/************************* TCP Sockets **************************/
/**
  Create a TCP server socket listening on the given port (0 for any port).  
  You must call skt_accept to actually receive a connection.
  Returns the actual port chosen for the socket in *port.
  Equivalent to a BSD sockets "socket", "bind", and "listen" call.
*/
SERVER_SOCKET skt_server(unsigned int *port);

/** Like skt_server, but only binds server to a particular IP address.
  This is only useful on a machine with several IP addresses,
  like a gateway or router machine; or pass in an invalid address
  to find the machine's IP address.
*/
SERVER_SOCKET skt_server_ip(unsigned int *port,skt_ip_t *ip);

/** Accept an incoming TCP connection request from a server socket.
	@param server_skt A server socket created by skt_server.
	@param client_ip Will be filled out with the incoming IP address.
	@param client_port Will be filled out with the incoming port number.
	@param return A new socket to communicate with that client.
*/
SOCKET skt_accept(SERVER_SOCKET server_skt, skt_ip_t *client_ip, unsigned int *client_port);

/** Create a TCP client socket, talking with this server.
  Initiates a TCP connection to this server IP address and port.
  Returns a new socket to communicate with that server.
*/
SOCKET skt_connect(skt_ip_t server_ip, int server_port, int timeout);

/** Close this socket, finishing all communication. 
   Sockets are automatically closed at program exit. */
void skt_close(SOCKET skt);

/** 
   Wait until this normal socket has some data ready to read,
   or for a server socket a client is trying to connect.
      @param skt Socket to test for data.
      @param msec Milliseconds to wait for data to arrive, or 0 to wait forever.
      @param return 1 if data is ready to be read, 0 if msec elapsed with no data.
*/
int skt_select1(SOCKET skt,int msec);

/** Send these bytes to this socket.  Returns 0 on success;
  else calls abort routine.
*/
int skt_sendN(SOCKET skt,const void *pBuff,int nBytes);

/** Receive these bytes from this socket.  Returns 0 on success;
  else calls abort routine.
*/
int skt_recvN(SOCKET skt,      void *pBuff,int nBytes);

/** Send these buffers to this socket.  Returns 0 on success;
  else calls abort routine.  It's normally faster to call skt_sendV
  with two buffers than to call skt_sendN twice, because of Nagle's
  algorithm.
*/
int skt_sendV(SOCKET skt,int nBuffers,const void **buffers,int *lengths);


/**************** Utility Routines *******************/

/**
   Set the OS kernel buffer size, in bytes, used by this socket.
   Changing the buffer size may increase performance in some cases.
   Uses setsockopt with SOL_SOCKET and SO_SNDBUF/SO_RCVBUF.
*/
void skt_setSockBuf(SOCKET skt, int bufsize);

/**
 Initialization routine.  This should be called automatically
 by everything that needs it.  But calling it multiple times 
 won't hurt.
*/
void skt_init(void);


/** An "idle function": called when waiting for the network (e.g., select, recv, send) */
typedef void (*skt_idleFn)(void);

/** Set the current idle routine to this new function. */
void skt_set_idle(skt_idleFn new_fn);


/** An "abort function": called when a serious socket error happens. 
  It's best for the abort function to never return--e.g.,
  by exiting the program, or throwing an exception.  But anything
  returned by the abort function will be passed out to the calling
  routine, if you prefer working with error codes.
*/
typedef int (*skt_abortFn)(int errCode,const char *msg);

/** Set the abort routine to this new function.  Returns the old function. */
skt_abortFn skt_set_abort(skt_abortFn new_fn);

/** Call the current skt_abort routine. */
int skt_call_abort(const char *msg);



#ifdef __cplusplus
/******** Utility routines ********/
#include <string>

/**
  Receive an STL string from this socket.  Will continue to 
  add characters to the string until a character in "term" is found.
  By default, the terminating characters include all white space.
  The terminating character is *not* added to the string.
*/
inline std::string skt_recv_string(SOCKET skt,const char *term=" \t\r\n")
{
	char c; 
	std::string str="";
	while (1) {
		skt_recvN(skt,&c,1); /* Grab next character */
		if (strchr(term,c)) {
			if (c=='\r') continue; /* will be CR/LF; wait for LF */
			else return str; /* Hit terminator-- stop. */
		}
		else str+=c; /* normal character--add to string and continue */
	}
}
/** Read a newline-terminated string from this socket. */
inline std::string skt_recv_line(SOCKET skt)
	{return skt_recv_string(skt,"\r\n");}

/******************* Communication utility classes *****************/
typedef unsigned char byte;

/**
Big-endian (network byte order) datatype.  This class is 
stored in memory as a big-endian 32-bit integer, regardless
of the endianness and integer size of the machine.

For completeness, a big-endian (network byte order) 4 byte 
integer has this format on the network:
Big32 ---------------------------------
  1 byte | Most significant byte  (&0xff000000; <<24)
  1 byte | More significant byte  (&0x00ff0000; <<16)
  1 byte | Less significant byte  (&0x0000ff00; <<8)
  1 byte | Least significant byte (&0x000000ff; <<0)
----------------------------------------------
*/
class Big32 { //Big-endian (network byte order) 32-bit integer
        byte d[4];
public:
        Big32() {}
        Big32(unsigned int i) { set(i); }
        operator unsigned int () const { return (d[0]<<24)|(d[1]<<16)|(d[2]<<8)|d[3]; }
        unsigned int operator=(unsigned int i) {set(i);return i;}
        void set(unsigned int i) { 
                d[0]=(byte)(i>>24); 
                d[1]=(byte)(i>>16); 
                d[2]=(byte)(i>>8); 
                d[3]=(byte)i; 
        }
};

/**
Big-endian (network byte order) datatype.  This class is 
stored in memory as a big-endian 16-bit integer, regardless
of the endianness and integer size of the machine.

For completeness, a big-endian (network byte order) 2 byte 
integer has this format on the network:
Big16 ---------------------------------
  1 byte | Most significant byte  (&0xff00; <<8)
  1 byte | Least significant byte (&0x00ff; <<0)
----------------------------------------------
*/
class Big16 {
        byte d[2];
public:
        Big16() {}
        Big16(unsigned int i) { set(i); }
        operator unsigned int () const { return (d[0]<<8)|d[1]; }
        unsigned int operator=(unsigned int i) {set(i);return i;}
        void set(unsigned int i) { 
                d[0]=(byte)(i>>8); 
                d[1]=(byte)i; 
        }
};
#endif /* C++ communication support */

#endif /*SOCK_ROUTINES_H*/


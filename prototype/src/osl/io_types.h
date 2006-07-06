/*
I/O-friendly types, with uniform in-memory size
and byte layout.

Orion Sky Lawlor, olawlor@acm.org, 2006/03/31 (Public Domain)
*/
#ifndef __OSL_IO_TYPES_H
#define __OSL_IO_TYPES_H

namespace osl { namespace io {

typedef unsigned char io_byte;

//Small utility classes: these classes have the memory layout of the given format. 

class Lil16 { //Little-endian (Intel byte order) 16-bit integer
	io_byte d[2];
public:
	Lil16() {}
	Lil16(unsigned int i) { set(i); }
	operator unsigned int () const { return d[0]|(d[1]<<8); }
	unsigned int operator=(unsigned int i) {set(i);return i;}
	void set(unsigned int i) { d[0]=(io_byte)i; d[1]=(io_byte)(i>>8); }
};
class Big16 { //Big-endian (network byte order) 16-bit integer
	io_byte d[2];
public:
	Big16() {}
	Big16(unsigned int i) { set(i); }
	operator unsigned int () const { return d[1]|(d[0]<<8); }
	unsigned int operator=(unsigned int i) {set(i);return i;}
	void set(unsigned int i) { d[1]=(io_byte)i; d[0]=(io_byte)(i>>8); }
};

class Lil32 { //Little-endian (Intel byte order) 16-bit integer
	io_byte d[4];
public:
	Lil32() {}
	Lil32(unsigned int i) { set(i); }
	operator unsigned int () const { return d[0]|(d[1]<<8)|(d[2]<<16)|(d[3]<<24); }
	unsigned int operator=(unsigned int i) {set(i);return i;}
	void set(unsigned int i) { 
		d[0]=(io_byte)i; 
		d[1]=(io_byte)(i>>8); 
		d[2]=(io_byte)(i>>16); 
		d[3]=(io_byte)(i>>24); 
	}
};
class Big32 { //Big-endian (network byte order) 16-bit integer
	io_byte d[4];
public:
	Big32() {}
	Big32(unsigned int i) { set(i); }
	operator unsigned int () const { return d[3]|(d[2]<<8)|(d[1]<<16)|(d[0]<<24); }
	unsigned int operator=(unsigned int i) {set(i);return i;}
	void set(unsigned int i) { 
		d[3]=(io_byte)i; 
		d[2]=(io_byte)(i>>8); 
		d[1]=(io_byte)(i>>16); 
		d[0]=(io_byte)(i>>24); 
	}
};


}; };

#endif /* defined(thisHeader) */

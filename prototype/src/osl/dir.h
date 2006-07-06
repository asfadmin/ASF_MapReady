/**
List files and subdirectories in a given directory.
Orion Sky Lawlor, olawlor@acm.org, 2005/8/5 (Public Domain)
*/
#ifndef __OSL_DIR_H
#define __OSL_DIR_H
#ifndef OSL_DLL /* used for Windows DLL generation */
#  define OSL_DLL /* empty */
#endif

namespace osl {

/**
  This class is called with each piece of an incoming directory.
  You inherit from this class to find the contents of a directory.
*/
class OSL_DLL directory_contents {
public:
	virtual ~directory_contents();
	
	/// The directory contains this file.
	/// Default: ignore files.
	virtual void hit_file(const char *dirName,const char *fileName);
	/// The directory contains this subdirectory.
	/// Default: ignore subdirectories.
	virtual void hit_directory(const char *dirName,const char *subdirName);
	
	/// Call hit_file and hit_directory with the contents of
	///  this directory.
	virtual void list(const char *dirName);
};

/**
  Recursively lists directory contents 
*/
class OSL_DLL directory_recursive : public directory_contents {
public:
	virtual void hit_directory(const char *dirName,const char *subdirName);
};

};

#endif

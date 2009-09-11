/**
List files and subdirectories in a given directory.
Orion Sky Lawlor, olawlor@acm.org, 2005/8/5 (Public Domain)
*/
#include "osl/dir.h"
#include <stdio.h> /* for sprintf */
#include <string.h>

osl::directory_contents::~directory_contents() {}
	
/// The directory contains this file.
/// Default: ignore files.
void osl::directory_contents::hit_file(const char *dirName,const char *fileName) {}
/// The directory contains this subdirectory.
/// Default: ignore subdirectories.
void osl::directory_contents::hit_directory(const char *dirName,const char *subdirName) {}

void osl::directory_recursive::hit_directory(const char *dirName,const char *subdirName)
{
	char subDir[1024]; /* total path to subdirectory */
	sprintf(subDir,"%s%c%s",dirName,'/',subdirName);
	list(subDir);
}

#ifdef WIN32
/**************** Windows *****************/
#include <windows.h>

void osl::directory_contents::list(const char *dirName)
{
	char dirNamePat[1024];
	sprintf(dirNamePat,"%s\\*",dirName); /* dirName, with trailing slash-star */
	WIN32_FIND_DATA f;
	HANDLE h=FindFirstFile(dirNamePat,&f);
	if (h==INVALID_HANDLE_VALUE) return;
	
	do {
		const char *name=f.cFileName;
		if (strcmp(name,".")==0 || strcmp(name,"..")==0) 
			continue; /* Bogus self links */
		// printf("---dirName: %s, file: %s\n",dirNamePat,name);
		if (f.dwFileAttributes&FILE_ATTRIBUTE_DIRECTORY)
			hit_directory(dirName,name);
		else
			hit_file(dirName,name);
	} while (FindNextFile(h,&f));
	FindClose(h);
}

#else
/************ UNIX dirent/stat version ****************/ 
#include <dirent.h> /* UNIX directory-list header */
#include <time.h> /* for "timespec", used in bits/stat.h (& whined about by icpc) */
#include <sys/stat.h> /* to tell if an item is a file or directory */

void osl::directory_contents::list(const char *dirName)
{
	/**
	 Problem: "readdir" returns only the filename, not including
	 the directory name (dirName).  "stat" requires the whole name,
	 including the directory.  For long pathnames, putting both together
	 would be slow.
	*/
	char fullPath[1024],*fullPathEnd;
	sprintf(fullPath,"%s%c",dirName,dirName[strlen(dirName)-1]=='/'?0:'/');
	fullPathEnd=&fullPath[strlen(fullPath)]; /* points to start of filename */
	DIR *d=opendir(dirName);
	if (d==0) return;
	/* FIXME: handle errors sensibly */
	struct dirent *de;
	struct stat s;
	while (NULL!=(de=readdir(d))) {
		const char *name=de->d_name;
		if (strcmp(name,".")==0 || strcmp(name,"..")==0) 
			continue; /* Bogus self links */
		strcpy(fullPathEnd,name);
	// printf("---dirName: %s, file: %s, full: %s\n",dirName,name,fullPath);
		if (stat(fullPath,&s)!=0) continue; /* Bogus file */
		if (S_ISDIR(s.st_mode)) hit_directory(dirName,name);
		else hit_file(dirName,name);
	}
	/* FIXME: closedir even on exception... */
	closedir(d);
}

#endif


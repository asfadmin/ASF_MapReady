/*
ASF Plugin Loader.  Loads asf::plugin objects 
from dynamically linked library files.

Orion Sky Lawlor, olawlor@acm.org, 2005/09/02.
*/
#ifndef __ASF_PLUGIN_LOADER_H
#define __ASF_PLUGIN_LOADER_H

#include "asf/plugin.h"

namespace asf {

/**
Loads a set of asf::plugin and asf::parameter objects
from dynamically linked library/shared object (.dll or .so) 
files.
*/
class ASF_COREDLL plugin_loader {
public:
	/// Create a loader using plugins from the default search paths.
	plugin_loader();

	/// Load up all plugins found in the search path.
	///  Return the new global registry.
	const asf::registry &load(void);
	
	/// Load (or re-load) any plugins found with this library
	///   base name anywhere in the search path.
	///  Returns the loaded local registry, or 
	///  NULL if the library can't be loaded.
	const asf::registry *load_library(std::string library);
	
	/// Load (or re-load) any plugins found in this library file.
	///  Returns the loaded plugins' local registry, or 
	///  NULL if the plugin can't be loaded.
	const asf::registry *load_file(std::string file);
	
	/// Return the current global registry, containing all plugins
	///   loaded so far.
	const asf::registry &get_loaded(void) const {return global;}
	
private:
	/**
	  List of directories to search for plugins, in search order.
	*/
	std::vector<std::string> dirs;
	
	/**
	  Describes a loaded library.
	*/
	class library_info {
	public:
		/// osl_dll handle
		void *dll;
		/// Registry for this library's plugins and parameters
		asf::registry local;
		
		/// Remove this library from memory.
		///   WARNING: be sure to delete all objects first!
		void unload(void);
	};
	
	/**
	  Map library name to library information.
	*/
	typedef std::map<std::string,library_info *> lib_t;
	lib_t lib;
	
	/** Global registry, containing all plugins and parameter types */
	asf::registry global;
};

/* If this is set to 1, the loader prints out lots of debugging stuff */
extern ASF_COREDLL int plugin_load_verbose;

/** Fill this registry with all system and dynamically loaded plugins */
ASF_COREDLL void dynamic_register(registry &reg);

};

#endif

/*
Command-line user interface (CLUI) scripting language.

Orion Sky Lawlor, olawlor@acm.org, 2006/03/21 (Copyright ASF)
*/
#ifndef __ASF_CLUI_H
#define __ASF_CLUI_H

#include "asf/plugin.h"
#include "asf/plugin_control.h"
#include <iostream> /* really just need istream, but it doesn't exist on gcc 2.95 */

namespace asf {

/**
  Creates fully-configured plugins by parsing a file or 
  string containing a CLUI script.  The resulting parameter_control_list
  can be executed using asf::execute_list.
*/
class ASF_COREDLL clui_parser {
	/** All loaded plugins */
	const asf::registry &reg;
	
	/* Parsing verbosity (for debugging) */
	int verbosity;
public:
	/* Global namespace full of output parameter names, like
		<plugin>.<output>
	*/
	std::map<std::string,asf::parameter *> outputs;
	
	/** Make a new parser that creates plugins from this registry */
	clui_parser(const asf::registry &reg_,int parse_verbosity=0);
	~clui_parser();
	
	/** List of "global" parameters added to all plugins */
	asf::plugin_parameter_list globals;
	
	/** Parse this CLUI-style input file, and return the list of commands. */
	asf::parameter_control_list *parse_file(const char *inFile);
	/** Parse this CLUI-style string into a list of commands. */
	asf::parameter_control_list *parse_string(const char *cmdString);
	/** Parse this CLUI-style block of commands from this stream */
	asf::parameter_control_list *parse_stream(std::istream &in);
	
	/* Parse this stream as this type */
	asf::parameter *parse_parameter(const asf::type *t,std::istream &in);
};

};


#endif

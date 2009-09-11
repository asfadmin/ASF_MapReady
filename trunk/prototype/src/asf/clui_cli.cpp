/**
CLUI = "Command-Line User Interface" 
to talk to ASF plugins.  This is a scripting
language a lot like Bourne shell scripts, used
to specify:
	- which plugin to run
	- what input values to give to plugin parameters
	- how plugin outputs connect to other plugin inputs

This is a very simple line-oriented language:
<plugin name> <param1>=<value1> [ <param2=value2> [...] ] ;

Orion Sky Lawlor, olawlor@acm.org, 2005/09/08 (copyright ASF)
*/
#include "asf/plugin.h"
#include "asf/clui.h"
#include "asf/plugin_loader.h"

void usage(std::string why) {
	fprintf(stderr,
		"Usage: clui [ -v[<level>] ] [ -l ] <script>\n"
		"  Runs ASF plugins controlled by script.\n"
		"  Use ASF_LIBRARY_PATH environment variable point to plugin directory\n");
	const char *dirs=getenv("ASF_LIBRARY_PATH");
	if (dirs!=NULL) 
		fprintf(stderr,"   or use any of these directories: %s\n",dirs);
	fprintf(stderr,
		" Error: %s\n",
		why.c_str());
	exit(1);
}


int main(int argc,const char *argv[]) {
	int argi=1;
	int verbosity=0;
	int tile_size=0; // default tile size
	while (argi<argc) {
		if (argv[argi][0]=='-') {
			switch(argv[argi][1]) {
			case 'v': { /* verbose run */
				int v=1;
				sscanf(&argv[argi][2],"%i",&v);
				printf("clui> Verbosity level set to %d\n",v);
				verbosity=v; /* per-plugin logging */
				asf::set_log_level(v); /* top-level logging */
			} break;
			case 'l': { /* DLL Loader debugging */
				asf::plugin_load_verbose=1;
			} break;
			case 't': { /* Tile size */
				tile_size=atoi(argv[++argi]);
			} break;
			default:
				usage("Unknown argument "+std::string(argv[argi]));
			}
			argi++;
		}
		else break;
	}
	if (argi!=argc-1) usage("Not enough arguments");
	const char *inFile=argv[argi++];
	
	/** Load up all the plugin types */
	asf::registry reg; asf::dynamic_register(reg);
	
	/** Parse input file and execute commands */
	asf::clui_parser parser(reg);
	//if (verbosity>0) 
	parser.globals.add("log_verbosity",new asf::parameter_int(verbosity));
	
	if (verbosity>0) printf("clui> Parsing input file '%s'\n",inFile);
	asf::parameter_control_list *list=parser.parse_file(inFile);
	
	if (verbosity>0) printf("clui> Executing plugins from '%s'\n",inFile);
	asf::execute_list(*list,tile_size);
	if (verbosity>0) printf("clui> Done executing plugins\n");
	return 0;
}


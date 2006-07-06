/*
Command-line user interface (CLUI) scripting language.

Orion Sky Lawlor, olawlor@acm.org, 2006/03/21 (Copyright ASF)
*/
#include "asf/clui.h"
#include <fstream>
#include <sstream>

asf::clui_parser::clui_parser(const asf::registry &reg_,int parse_verbosity)
	:reg(reg_),verbosity(parse_verbosity)
{
	
}
asf::clui_parser::~clui_parser() {
}

/* Parse input file into a set of created plugins */
asf::parameter_control_list *asf::clui_parser::parse_file(const char *inFile) 
{
	std::ifstream f(inFile,std::ios::in);
	return parse_stream(f);
}

/* Parse input string into a set of created plugins */
asf::parameter_control_list *asf::clui_parser::parse_string(const char *cmdString) 
{
#ifdef __FAKE_SSTREAM_H /* no real istringstream--fake it with strstream*/
	std::strstream s((char *)cmdString,strlen(cmdString));
#else /* normal modern compiler-- use istringstream */
	std::istringstream s(cmdString);
#endif
	return parse_stream(s);
}

/* Consume whitespace characters & comments */
static void eatwhite(std::istream &in) {
	char c;
	while (in.get(c)) {
		// printf("eatwhite: '%c'\n",c); // parser debugging...
		if (isspace(c)) 
			continue; /* more whitespace */
		else if (c=='#'||c=='/'||c=='%') 
			in.ignore(10000,'\n'); /* comment line */
		else { /* something useful */
			in.putback(c);
			break;
		}
	}
}

/* Return the next character in the stream without consuming it */
static char peek(std::istream &in) {
	char c; 
	if (!in.get(c)) return 0;
	in.putback(c);
	return c;
}

/**
  Read a string until it's terminated by whitespace *or* any of the "term" characters.
*/
static void getline_terminators(std::istream &in,std::string &dest,const char *term)
{
	char c;
	eatwhite(in);
	dest="";
	while (in.get(c)) {
		if (isspace(c) || strchr(term,c)) {
			in.putback(c);
			break;
		}
		else /* just another character */
			dest+=c;
	}
}

asf::parameter_control_list *asf::clui_parser::parse_stream(std::istream &in)
{
	asf::parameter_control_list *ret_list=new asf::parameter_control_list;
	while (in) {
		std::string plugin_name; 
		eatwhite(in); in>>plugin_name;
		if (plugin_name=="}" || plugin_name=="") return ret_list; /* end of block */
		
		/* Find the plugin parameter signature */
		if (verbosity>0) printf("Loading plugin '%s'\n",plugin_name.c_str());
		asf::registry::plugin_allocation_function 
			alloc_fn=reg.plugin_factory(plugin_name.c_str());
		asf::plugin_parameter_signature sig;
		delete alloc_fn(sig); /* i.e., make a plugin to collect its signature, and delete it */
		
		/* Parameters to pass to the plugin. 
		   This is dynamically allocated because it lives on inside
		   the plugin (and is used in plugin_control.cpp)
		*/
		asf::plugin_parameter_list *params=new asf::plugin_parameter_list(globals);
		
		/* Create new empty objects to hold all plugin output parameters.
		  Subtle: control flow like "for" loops may need the plugin outputs
		    while parsing the control list, so add them before parsing input
		    and optional parameters!
		*/
		for (unsigned int i=0;i<sig.outputs().size();i++) {
			const char *param_type=sig.outputs()[i]->t->name();
			std::string param_name=sig.outputs()[i]->name;
			asf::parameter *pa=reg.parameter_factory(param_type)();
			params->add(param_name.c_str(),pa);
			outputs[plugin_name+"."+param_name]=pa;
		}
		
		/* Grab the plugin input & optional parameters */
		while (1) {
			std::string param_name;
			eatwhite(in);
			if (peek(in)==';') {in.get(); break;} /* end of argument list */
			getline_terminators(in,param_name,"=#!/");
			if (param_name=="{") { /* command list */
				if (verbosity>0) printf("\tImplicit 'list' control_list parameter:\n");
				params->add("list",parse_stream(in));
				continue;
			}
			if (param_name=="") { break; /* End of file/line */
			}
			if (verbosity>0) printf("\tParameter '%s':\n",param_name.c_str());
			eatwhite(in);
			if (peek(in)=='=') in.get(); /* Skip over "=" sign */
			
			const asf::plugin_parameter_signature::parameter_info *pi=sig.lookupNULL(param_name.c_str());
			if (pi==NULL) { /* Plugin does not take such a parameter! */
				sig.print();
				asf::die("Plugin "+plugin_name+" does not take an argument called '"+param_name+"'");
			}
			if (verbosity>0) printf("\t\tType '%s' (%s)\n",pi->t->name(),asf::plugin_parameters::dir2str(pi->dir));
			asf::parameter *pa=parse_parameter(pi->t,in);
			if (pa==NULL) {
				asf::die("Plugin "+plugin_name+" argument "+param_name+": cannot parse input as "+(std::string)pi->t->name());
			}
			if (verbosity>0) pa->print();
			params->add(param_name.c_str(),pa);
		}
		
		/* Create the plugin using those parameters */
		asf::plugin *pl=alloc_fn(*params);
		ret_list->add(pl);
	}
	return ret_list;
}

asf::parameter *asf::clui_parser::parse_parameter(const asf::type *t,std::istream &in)
{
	if (peek(in)=='@') 
	{ /* "output reference" indicated using (STUPID) escape! 
		FIXME: Make parser intelligent enough to distinguish
			values like 1, 1.3, "foo" from output references like
			for.index.
	*/
		std::string outref;
		in.get(); /* skip the (STUPID) @ sign */
		getline_terminators(in,outref,"=#!/;");
		if (outputs[outref]==0) asf::die("Unrecognized output reference '"+outref+"'");
		return outputs[outref];
	}
	
	std::string match_type=t->name();
	
	if (match_type=="parameter") { /* Generic parameter-- parse exact type from file */
		in>>match_type;
	}
	if (match_type=="int") {
		int value=0;
		if (!(in>>value)) return NULL;
		return new asf::parameter_int(value);
	}
	if (match_type=="real") {
		double value=0;
		if (!(in>>value)) return NULL;
		return new asf::parameter_real(value);
	}
	if (match_type=="string") {
		std::string value;
		eatwhite(in);
		int quote=in.get();
		if (quote!='\'' && quote!='"') 
		{ /* Allow short strings to be whitespace delimited */
			value+=(char)quote;
			quote=' ';
		}
		/* Read characters until the end of quote is reached.  FIXME: backslash escapes */
		int c=in.get();
		while (!(c==quote || (quote==' '&&isspace(c)))) {
			value+=(char)c;
			c=in.get();
		}
		/* Now c==quote */
		return new asf::parameter_string(value);
	}
	if (match_type=="control_list") {
		if (verbosity>0) printf("\tParsing control_list parameter...\n");
		eatwhite(in);
		if (in.get()!='{') asf::die("Cannot parse control list: missing '{'");
		return parse_stream(in);
	}
	/* FIXME: look up arbitrary parameter types in registry & call virtual "parse" method */
	return NULL;
}

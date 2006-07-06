/*
ASF Plugin Architecture
PROTOTYPE 1-L

Orion Sky Lawlor, olawlor@acm.org, 2005/08/31.
*/
#include "asf/plugin.h"
#include <algorithm>
#include <string.h> /* Because asf::type::string_t is "char *" */
#include <stdarg.h> /* for plugin::log */

ASF_COREDLL void debug_here(void) {
	/* Set debugger breakpoint here */
}

/** Abort routine used by pup */
extern "C" void CmiAbort(const char *why) { asf::current_abort_fn(why); }

/** Call abort function. */
ASF_COREDLL void asf::die(const std::string &why) {
	asf::current_abort_fn(why.c_str());
}

static int log_level=0;

/** Top-level logging.  Plugins should use plugin::log, not this routine. */
ASF_COREDLL void asf::log(int level,const std::string &why)
{
	if (log_level>=level) {
		printf("%s",why.c_str());
	}
}

/** Top-level logging verbosity. */
ASF_COREDLL void asf::set_log_level(int level)
{
	log_level=level;
}

/** Default abort function.  Useful for command-line apps. */
void asf_default_abort(const char *why) {
	debug_here();
	fprintf(stderr,"FATAL ERROR: %s\n",why);
	fflush(stdout);
	fflush(stderr);
	exit(1);
}

asf::abort_fn asf::current_abort_fn=asf_default_abort;

/******************************** type ************************/

void asf::type::print(bool verbose,FILE *dest) const
{
	if (!verbose) fprintf(dest,"%s v%f",name(),version());
	else fprintf(dest,
	"Name: %s    Version: %.0f\n"
	"   Description: %s\n"
	"   Authors: %s\n"
	"   Flags: %x\n",
		name(),version(), description(), authors(), flags());
}
asf::type_match::~type_match() {}
bool asf::type_match::matches(const type *s) const
{
	if (0==strcmp(s->name(),m_name)) return true; /* OK! */
	else return false; /* Not OK. */
}
bool asf::type_match::matches_any(const type *s) const
{
	do {
		if (matches(s)) return true;
		s=s->parent(); /* That didn't work-- try the parent */
	} while (s!=NULL);
	return false; 
}

asf::type_version_match::type_version_match(type::string_t name,double version)
	:type_match(name)
{
	/* The typecast-to-float business is to prevent roundoff from
	   screwing up the floating-point comparison for version numbers
	   like "1.1" 
	*/
	version_lo=(float)version;
	version_hi=1+(int)version;
}

bool asf::type_version_match::matches(const type *s) const {
	float v=(float)s->version();
	if (0==strcmp(s->name(),m_name) && (version_lo<=v) && (v<version_hi))
		return true; /* OK! */
	return false; /* Not OK. */
}

/**************************** Parameter ********************/
/* static */ const asf::type asf::parameter::static_type(
  /* parent  */ NULL, /* parameter has no parent type */
  /* name    */ "parameter",
  /* desc    */ "Anything that can be passed into or out of an asf::plugin",
  /* authors */ "v1.0 by Orion Lawlor (olawlor@acm.org) 2005/08/31",
  /* version */ 1.0
);

void asf::parameter::pup(PUP::er &p) {
	PUP::able::pup(p);
	p.comment("ASF Parameter");
}

void asf::parameter::read(FILE *f) {
	PUP::fromTextFile p(f);
	pup(p);
}
void asf::parameter::write(FILE *f) {
	PUP::toTextFile p(f,true);
	pup(p);
}
void asf::parameter::print(FILE *f) {
	if (f==NULL) f=stdout;
	fprintf(f,"%s:\n",get_type()->name());
	write(f);
}
const char *asf::parameter::differences(const parameter &cp2) {
	parameter &p2=*(parameter *)&cp2; /* cast away constness */
	if (!type_match(get_type()->name()).matches_any(p2.get_type()))
		return "different types";
	int size1=PUP::size(*this);
	int size2=PUP::size(p2);
	if (size1!=size2) return "sizes different";
	char *c1=new char[size1], *c2=new char[size2];
	PUP::toMemBuf(*this,c1,size1);
	PUP::toMemBuf(p2,c2,size2);
	int cmp=memcmp(c1,c2,size1);
	delete[] c1; delete[] c2;
	if (cmp!=0) return "data are different";
	return NULL; /* Identical! */
}

ASF_parameter_def(asf::parameter);

/* static */ const asf::type asf::parameter_int::static_type(
  /* parent  */ &asf::parameter::static_type,
  /* name    */ "int",
  /* desc    */ "An integer parameter value",
  /* authors */ "v1.0 by Orion Lawlor (olawlor@acm.org) 2005/08/31",
  /* version */ 1.0
);
void asf::parameter_int::pup(PUP::er &p) {
	asf::parameter::pup(p);
	PUPt(p,value);
}
ASF_parameter_def(asf::parameter_int);

/* static */ const asf::type asf::parameter_real::static_type(
  /* parent  */ &asf::parameter::static_type,
  /* name    */ "real",
  /* desc    */ "A floating-point parameter value",
  /* authors */ "v1.0 by Orion Lawlor (olawlor@acm.org) 2005/08/31",
  /* version */ 1.0
);
void asf::parameter_real::pup(PUP::er &p) {
	asf::parameter::pup(p);
	PUPt(p,value);
}
ASF_parameter_def(asf::parameter_real);

/* static */ const asf::type asf::parameter_string::static_type(
  /* parent  */ &asf::parameter::static_type,
  /* name    */ "string",
  /* desc    */ "A string parameter value",
  /* authors */ "v1.0 by Orion Lawlor (olawlor@acm.org) 2005/08/31",
  /* version */ 1.0
);
asf::parameter_string::parameter_string(std::string _value)
	:value(_value) {}
void asf::parameter_string::pup(PUP::er &p) {
	asf::parameter::pup(p);
	PUPt(p,value);
}
ASF_parameter_def(asf::parameter_string);

/********** Array support *********/
asf::parameter_array_generic::parameter_array_generic() {}
asf::parameter_array_generic::~parameter_array_generic() {
	resize(0,0); /* throws away all parameters */
}

/// Extract the i'th element of our array.
///   Will abort if i is out of bounds.
asf::parameter *asf::parameter_array_generic::get(int i) const {
	if (i<0 || i>=(int)size()) asf::die("parameter_array_generic passed out-of-bounds array index!\n");
	return elts[i];
}
	
/// Add this parameter to our array.  Note this is the only way to 
///   add data to this array.
void asf::parameter_array_generic::push_back(parameter *p) {
	elts.push_back(p);
}
	
/// Resize our array to contain n elements.  The new elements will get value p.
void asf::parameter_array_generic::resize(unsigned int n,parameter *p) {
	for (unsigned int i=n;i<size();i++) /* dispose of parameters being trimmed off */
		delete elts[i];
	elts.resize(n,p);
}

/** parameter boilerplate */
void asf::parameter_array_generic::pup(PUP::er &p) {
	PUP::able::pup(p);
	p.comment("ASF parameter array of length:");
	int length=elts.size();
	PUPt(p,length);
	if (p.isUnpacking()) { /* fill up empty array */
		resize(0);
		for (int i=0;i<length;i++) {
			asf::parameter *e=asf::pup_type_unpack_parameter(p)();
			e->pup(p);
			push_back(e);
		}
	} 
	else { /* not unpacking-- leave data where it is. */
		for (int i=0;i<length;i++) {
			asf::parameter *e=elts[i];
			asf::pup_type_packing(p,e->get_type());
			e->pup(p);
		}
	}
}

ASF_parameter_def(asf::parameter_array_generic);

/* static */ const asf::type asf::parameter_array_generic::static_type(
  /* parent  */ &asf::parameter::static_type,
  /* name    */ "array_generic",
  /* desc    */ "An array of other parameters.  Basically std::vector.",
  /* authors */ "v1.0 by Orion Lawlor (olawlor@acm.org) 2006/06/09",
  /* version */ 1.0
);

/************************** Parameter Constraints ********************/
asf::parameter_constraint::~parameter_constraint() {}

/** The value of the parameter must lie between lo and hi */
asf::parameter_int_constraint::parameter_int_constraint(int lo_,int hi_)
	:type(constraint_range),lo(lo_),hi(hi_) {}

/** The value of the parameter must be one of these listed values.
	End the table of values with a NULL name.*/
asf::parameter_int_constraint::parameter_int_constraint(const value_t *values_,constraint_t type_)
	:type(type_), lo((~0U)>>1), hi((~0)^((~0U)>>1)), values(values_)
{
	/* Walk list of values, to determine lo/hi bounds */
	for (int i=0;values[i].name!=0;i++) {
		int v=values[i].value;
		if (lo>v) lo=v;
		if (hi<v) hi=v;
	}
}

const asf::type *asf::parameter_int_constraint::get_type(void) {
	return &asf::parameter_int::static_type;
}
void asf::parameter_int_constraint::print(void) {
	const char *type_str="Unknown";
	if (type==constraint_range) type_str="Range";
	if (type==constraint_value) type_str="Value";
	if (type==constraint_bitmask) type_str="Bitmask";
	
	printf("     %s constraint\n",type_str);
	printf("       Range (inclusive): %d to %d\n",lo,hi);
	if (values)
	for (int i=0;values[i].name!=0;i++) {
		printf("      Value %d: %s  ",values[i].value,values[i].name);
		if (values[i].desc)
			printf("  (%s)\n",values[i].desc);
		else
			printf("\n");
	}
}

/*************************** Plugin Support ***********************/
asf::plugin_parameters::~plugin_parameters() {}
asf::plugin_parameter_signature::plugin_parameter_signature() 
	:plugin_parameters(false)
{}
asf::plugin_parameter_signature::~plugin_parameter_signature() 
{
	/** Free memory for all allocated parameter_info objects */
	for (int dir=0;dir<dir_last;dir++) {
		for (unsigned int i=0;i<bydir[dir].size();i++)
			delete bydir[dir][i];
		bydir[dir].empty();
	}
	byname.empty();
}

/// Return a string describing this direction
const char *asf::plugin_parameters::dir2str(dir_t d)
{
	static const char *dir_names[dir_last]={"input","optional","output"};
	if (d<0 || d>=dir_last) return "invalid direction (!?)";
	else return dir_names[d];
}

/**
  Store this parameter into our list.  
  Sets all parameter pointers to NULL.
*/
bool asf::plugin_parameter_signature::param(type::string_t name,const type *t,parameter **ptr,
	
	parameter_constraint *constraint,type::string_t long_description,
	dir_t d)
{
	parameter_info *pi=new parameter_info; /* freed in destructor */
	pi->name=name;
	pi->t=t;
	pi->constraint=constraint;
	pi->long_description=long_description;
	pi->dir=d;
	bydir[(int)d].push_back(pi);
	byname[name]=pi;
	*ptr=0; /* MUST set value of *ptr */
	return false;
}

/// Print out our list of parameters, in a human-friendly format.
void asf::plugin_parameter_signature::print(FILE *dest)
{
	for (int d=0;d<dir_last;d++) {
		if (bydir[d].size()==0u) continue;
		printf("%s parameters:\n",dir2str((dir_t)d));
		for (unsigned int i=0;i<bydir[d].size();i++) {
			const char *type=bydir[d][i]->t->name();
			const char *name=bydir[d][i]->name;
			printf("\t%s  %s\n",type,name);
		}
	}
}
const asf::plugin_parameter_signature::parameter_info *asf::plugin_parameter_signature::lookupNULL(type::string_t name) const
{
	byname_t::const_iterator it=byname.find(name);
	if (it==byname.end()) return NULL;
	else return (*it).second;
}

asf::plugin_parameter_list::plugin_parameter_list() 
	:plugin_parameters(true) {}
asf::plugin_parameter_list::~plugin_parameter_list() {}

/**
  Add this parameter under this name to our table.
*/
void asf::plugin_parameter_list::add(type::string_t name,parameter *p)
{
	table.insert(std::make_pair(std::string(name),p));
}

/**
  Set the value of this parameter from our table.
  If the types don't match, die.
  If the parameter doesn't exist in our table,
    die unless d==dir_optional, when we set *ptr=0.
*/
bool asf::plugin_parameter_list::param(type::string_t name,const type *t,parameter **ptr,
		parameter_constraint *constraint,type::string_t long_description,
		dir_t d)
{
	table_t::const_iterator it=table.find(name);
	if (it!=table.end()) { /* Parameter exists-- stash it*/
		parameter *param=(*it).second;
		if (!type_match(t->name()).matches_any(param->get_type())) 
			die("Typecheck failed on plugin parameter "+std::string(name));
		*ptr=param;
	} else { /* Parameter name not present! */
		if (d==dir_optional) *ptr=0; /* optional-- just set to zero */
		if (d==dir_input) die("Plugin is missing required input parameter "+std::string(name));
		if (d==dir_output) die("Plugin is missing required output parameter "+std::string(name));
	}
	return (*ptr)!=0;
}

/*************************** Plugin Base Class ***********************/
/* static */ const asf::type asf::plugin::static_type(
  /* parent  */ NULL, /* Plugin itself has no parent type */
  /* name    */ "plugin",
  /* desc    */ "A module that does some computation on some parameters",
  /* authors */ "v0.1 by Orion Lawlor (olawlor@acm.org) 2005/09/01",
  /* version */ 0.1
);

asf::plugin::plugin(plugin_parameters &param) 
	:parameters(param)
{
	asf::optional(param,"log_verbosity",&log_verbosity);
	asf::optional(param,"log_name",&log_name);
	log_dest=stdout;
	if (log_name) {
		log_dest=fopen(log_name->c_str(),"a");
		if (log_dest==NULL) die("Can't append to log file "+(std::string)*log_name);
	}
}
asf::plugin::~plugin() {
	if (log_name) fclose(log_dest);
}
/**
 Return a new location_function that outputs 
 src image coordinates given dest image coordinates.
 Default implementation returns NULL, meaning no images used.
*/
asf::location_function *asf::plugin::image_in_from_out(int i,int o) {
	return (asf::location_function *)0;
}

/* By default, a plugin has side effects *only* if it has no outputs */
bool asf::plugin::has_side_effects(void) const {
	return 0==get_signature()->outputs().size();
}

/**
 Initialize the output variables' metadata based on the 
 input variables' metadata.  Default implementation does nothing.
*/
void asf::plugin::meta_execute(void) { }

/** Common-to-all-plugins logging operation.
    Level is the first verbosity level to show this print;
      so lower levels are more important.
*/
void asf::plugin::log(int level,const char *format ...) {
	va_list va;
	va_start(va,format);
	if (log_verbosity) {
		if (*log_verbosity>=level) {
			fprintf(log_dest,"%s: ",get_type()->name());
			vfprintf(log_dest,format,va);
			fflush(log_dest);
		}
	}
	va_end(va);
}

/** Like above, but at level 1 by default.  Drop-in replacement for printf. */
void asf::plugin::log(const char *format ...) {
	va_list va;
	va_start(va,format);
	if (log_verbosity) {
		/* FIXME: figure out how to call normal "log(1,...)" here to avoid code duplication*/
		if (*log_verbosity>=1) {
			fprintf(log_dest,"%s: ",get_type()->name());
			vfprintf(log_dest,format,va);
			fflush(log_dest);
		}
	}
	va_end(va);
}

/************************ Registration **********************/
/* Declaring the registry constructor and destructor in this .cpp file
  prevents code bloat due to the std::map templates. */
asf::type_registry::type_registry() {}
asf::type_registry::~type_registry() {}

/** Register this type and allocation function with us. */
void asf::type_registry::add(const type *t,generic_allocate_function_t f) {
	byname.insert(std::make_pair((std::string)t->name(),entry(t,f)));
	byparent[(std::string)t->parent()->name()].push_back(t->name());
}

/** Return the unique entry that matches this request.  If more than
  one entry matches, abort. */
const asf::type_registry::entry *asf::type_registry::lookupNULL(const type_match &m) const
{
	std::pair<name_t::const_iterator, name_t::const_iterator> p 
		= byname.equal_range((std::string)m.get_match_name());
	const entry *ret=NULL;
	for (name_t::const_iterator it=p.first ; it != p.second; ++it) {
		const entry *e=&(*it).second;
		if (m.matches_any(e->get_type())) {
			if (ret!=NULL) die("asf::registry> More than one match for "+std::string(m.get_match_name()));
			else ret=e;
		}
	}
	return ret;
}
const asf::type_registry::entry *asf::type_registry::lookup(const type_match &m) const
{
	const entry *ret=lookupNULL(m);
	if (ret==NULL) 
		die("asf::registry> No such plugin '"+std::string(m.get_match_name())+"'.\n"
		"If this plugin should exist, you may need to set the ASF_LIBRARY_PATH\n"
		"environment variable before running this program, like this:\n"
		"     export ASF_LIBRARY_PATH=`pwd`/lib\n");
	return ret;
}

/** Return a list of all the entries that match this request. */
asf::type_registry::entry_list asf::type_registry::multilookup(const type_match &m,const char *mName) const
{
	if (mName==NULL) mName=m.get_match_name();
	std::pair<name_t::const_iterator, name_t::const_iterator> p 
		= byname.equal_range((std::string)mName);
	asf::type_registry::entry_list ret;
	for (name_t::const_iterator it=p.first ; it != p.second; ++it) {
		const entry *e=&(*it).second;
		if (m.matches_any(e->get_type())) ret.push_back(*e);
	}
	return ret;
}

/** Return a list of all the children (and grandchildren, etc.) of this parent class */
asf::type_registry::entry_list asf::type_registry::children(const type_match &m) const
{
	asf::type_registry::entry_list kids;
	parent_t::const_iterator it=byparent.find((std::string)m.get_match_name());
	if (it==byparent.end()) return kids; /* Not in parent list */
	const childlist_t &stringKids=(*it).second;
	for (unsigned int i=0;i<stringKids.size();i++) {
		entry_list ik=multilookup(m,stringKids[i].c_str());
		std::copy(ik.begin(),ik.end(),std::back_inserter(kids));
	}
	return kids;
}

/** Add this registry's entries to ours.
 Where entries overlap, use r's, and throw away ours.
*/
void asf::type_registry::merge(const type_registry &r)
{
	for (name_t::const_iterator it=r.byname.begin();it!=r.byname.end();++it) {
		const entry &e=(*it).second;
		if (lookupNULL(e.get_name())!=NULL)
			byname[(std::string)e.get_name()]=e; /* Existing entry: overwrite */
		else /* New entry: add */
			add(e.get_type(),e.get_function());
	}
}

/** Print out the type information in this registry */
void asf::registry::print_types(FILE *dest)
{
	fprintf(dest,"Parameter classes:\n");
	parameters.print_types("parameter",dest);
	fprintf(dest,"Plugin classes:\n");
	plugins.print_types("plugin",dest);
}

/** Print out a description of the types derived from this class. */
void asf::type_registry::print_types(const type_match &m,FILE *dest,int tab_level)
{
	/* Tab over enough */
	for (int t=0;t<tab_level;t++) fprintf(dest,"\t");
	/* Print info. about this type */
	const entry *e=lookupNULL(m);
	if (e!=0) {
		e->get_type()->print(true,dest); 
		printf("   Allocate routine: %p\n",e->get_function());
	}
	/* Recursively expand all children: */
	entry_list l=children(m);
	for (unsigned int i=0;i<l.size();i++)
		print_types(l[i].get_name(),dest,tab_level+1);
}

/** Pup the name and version of this type.
  Only works with packing or sizing PUP::er's. */
ASF_COREDLL void asf::pup_type_packing(PUP::er &p,const asf::type *t) {
	if (p.isUnpacking()) asf::die("asf::pup_type_packing only works on packing or sizing PUP::er's!\n");
	int len=strlen(t->name());
	PUPt(p,len);
	p((char *)t->name(),len);
}

/** Look up this type in the system registry. */
ASF_COREDLL asf::registry::parameter_allocation_function asf::pup_type_unpack_parameter(PUP::er &p) {
	char buf[100];
	int len;
	PUPt(p,len);
	if (len>=(int)sizeof(buf)-1) asf::die("Invalid type name length from pup!");
	p(buf,len);
	buf[len]=0; /* nul terminator */
	return asf::system_registry().parameter_factory(buf);
}


/***************** Control Flow *******************/
#include "asf/plugin_control.h"
/* static */ const asf::type asf::parameter_control_list::static_type(
  /* parent  */ &asf::parameter::static_type,
  /* name    */ "control_list",
  /* desc    */ "A list of plugins to execute",
  /* authors */ "v1.0 by Orion Lawlor (olawlor@acm.org) 2005/09/14",
  /* version */ 1.0
);
void asf::parameter_control_list::pup(PUP::er &p) {
	asf::parameter::pup(p);
	asf::die("FIXME: asf::parameter_control_list::pup won't work until it stores plugins and their parameters by strings or IDs or some other persistent method.");
}
ASF_parameter_def(asf::parameter_control_list);

void asf::parameter_control_list::print(FILE *f) {
	if (f==NULL) f=stdout;
	fprintf(f,"FIXME: can't print an asf::parameter_control_list yet...\n");
}

/*** Plugin_control_flow */
/* static */ const asf::type asf::plugin_control_flow::static_type(
  /* parent  */ &asf::plugin::static_type,
  /* name    */ "control_flow",
  /* desc    */ "Abstract superclass of all control-flow plugins",
  /* authors */ "v1.0 by Orion Lawlor (olawlor@acm.org) 2005/09/14",
  /* version */ 1.0
);

asf::plugin_control_flow::plugin_control_flow(plugin_parameters &param)
	:asf::plugin(param)
{
	asf::parameter_control_list *list;
	asf::input(param,"list",&list);
	lists.push_back(list);
}
asf::plugin_control_flow::~plugin_control_flow() {
	for (int i=0;i<get_list_count();i++)
		delete get_list(i);
}

void asf::plugin_control_flow::meta_execute(void)
{
	for (int i=0;i<get_list_count();i++)
		get_list(i)->meta_execute();
}

void asf::plugin_control_flow::execute(void)
{
	int step=0, list=-1;
	while (0<=(list=execute_iteration(step++))) {
		const parameter_control_list *pl=get_list(list);
		pl->execute();
	} 
}
int asf::plugin_control_flow::execute_iterations(void)
{
	return 1;
}

/*** Plugin_for */
/* static */ const asf::type asf::plugin_for::static_type(
  /* parent  */ &asf::plugin_control_flow::static_type,
  /* name    */ "for",
  /* desc    */ "'for' loop.  Executes 'list' with 'index'=='min' to just below 'max', in units of 'step'.",
  /* authors */ "v1.0 by Orion Lawlor (olawlor@acm.org) 2005/09/14",
  /* version */ 1.0
);

asf::plugin_for::plugin_for(plugin_parameters &param) 
	:asf::plugin_control_flow(param)
{
	asf::input(param,"min",&min);
	asf::input(param,"max",&max);
	asf::optional(param,"step",&step_param);
	asf::output(param,"index",&index);
}
int asf::plugin_for::execute_iterations(void) {
	int step=1; if (step_param) step=*step_param;
	return (*max-*min)/step;
}
int asf::plugin_for::execute_iteration(int stepNo) {
	int step=1; if (step_param) step=*step_param;
	*index=*min+stepNo*step;
	if (*index<*max)
		return 0; /* always use list 0 */
	else
		return -1; /* end of loop */
}

ASF_plugin_def(asf::plugin_for)

/**** Plugin_if */
/* static */ const asf::type asf::plugin_if::static_type(
  /* parent  */ &asf::plugin_control_flow::static_type,
  /* name    */ "if",
  /* desc    */ "'if' statement.  If condition is nonzero, executes 'list' argument; otherwise executes 'else'.",
  /* authors */ "v1.0 by Orion Lawlor (olawlor@acm.org) 2005/09/15",
  /* version */ 1.0
);

asf::plugin_if::plugin_if(plugin_parameters &param) 
	:asf::plugin_control_flow(param)
{
	asf::input(param,"condition",&condition);
	asf::parameter_control_list *pelse=NULL;
	asf::optional(param,"else",&pelse);
	if (pelse!=NULL) lists.push_back(pelse);
}
int asf::plugin_if::execute_iteration(int stepNo) {
	if (stepNo>0) return -1; /* Only once through */
	if (condition->value!=0) return 0; /* condition passes--return main list */
	else if (get_list_count()==2) return 1; /* condition fails--else branch */
	else return -1; /* condition fails, no else branch.  Do nothing. */
}

ASF_plugin_def(asf::plugin_if)

#include "asf/image.h"

/** 
  The "system registry", used by the routines below.
  This keeps track of all parameter and plugin types.
*/
static asf::registry *cur_system_registry;

/** Return the current central system registry.  Aborts if none is set. */
ASF_COREDLL asf::registry &asf::system_registry(void) {
	if (!cur_system_registry) asf::die("No system registry set up yet!  Do you need to call system_register?");
	return *cur_system_registry;
}

/** Registers all builtin types **/
ASF_COREDLL void asf::system_register(asf::registry &r) {
	/* Note: "parameter" itself is abstract (never created), and so is not registered */
	ASF_parameter_reg(r,asf::parameter_int);
	ASF_parameter_reg(r,asf::parameter_real);
	ASF_parameter_reg(r,asf::parameter_string);
	ASF_parameter_reg(r,asf::parameter_array_generic);
	ASF_parameter_reg(r,asf::parameter_float_image);

	/* Note: "plugin" itself is never created either */
	// ASF_plugin_reg(r,asf::plugin_system);
	ASF_plugin_reg(r,asf::plugin_for);
	ASF_plugin_reg(r,asf::plugin_if);
	
	/** Set up this registry as the new system registry */
	cur_system_registry=&r;
}

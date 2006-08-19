/*
ASF Plugin Architecture
PROTOTYPE 1-L

Includes the definitions of the key "asf::type",
"asf::parameter", "asf::plugin", and "asf::registry"
classes.

Orion Sky Lawlor, olawlor@acm.org, 2005/08/31.
*/
/**
\mainpage ASF C++ Source Code Documentation
<!-- This HTML is generated from src/asf/plugin.h -->

\section classes Crucial runtime classes

These are the main C++ classes you'll deal with when you write ASF C++ code.
<ul>
<li>asf::plugin is the central computation class in the framework.  Plugins are normally loaded from a DLL.  You will probably write lots of plugins, and combine existing plugins in even more ways.
<li>asf::parameter is the central data class in the framework.  Parameters are how plugins get their inputs and outputs.
<li>asf::parameter_float_image is how images are represented as parameters.
<li>asf::metadata_source is how you find out information about images, like geographic locations or orbit numbers.
<li>asf::type is used to document the machine and human-readable names of plugins and prototypes.
</ul>

\section flow Overall flow of control

See asf::plugin for the life cycle of a plugin.  Outside a plugin:

<ul>
<li>The "clui" executable starts in src/asf/clui_cli.cpp's main routine.
<li>Plugin DLLs are loaded by src/asf/plugin_loader.cpp's asf::dynamic_register routine.
<li>Loaded plugins and parameters are listed in an asf::registry.
<li>asf::clui_parser parses the input file, creating asf::plugin and asf::parameter objects via the asf::registry.
<li>Plugins are actually executed by src/asf/plugin_execute.cpp's asf::execute_list routine.
</ul> 


*/


#ifndef __ASF_PLUGIN_H
#define __ASF_PLUGIN_H

/* Grab ASF_COREDLL macro */
#include "asf/dll_support.h"

/** stdio FILE *'s are used for I/O. 
One could argue for using std::ostream here eventually.
*/
#include <stdio.h>
/** The pup system is used to keep track of parameter datatypes */
#include "pup/pup.h"
#include "pup/pup_stl.h"
/** STL vector, map, and string classes are used extensively */
#include <vector>
#include <map>
#include <string>


#include "asf/util.h"


/** Give user an opportunity to enter debugger */
extern "C" ASF_COREDLL void debug_here(void);

/** Everything's in the "asf" namespace.
This lets us not worry about clashes from common names 
like "plugin", "output", etc.
*/
namespace asf {

/** Abort function: must either exit (default) or throw an exception. */
typedef void (*abort_fn)(const char *why);
extern abort_fn current_abort_fn;

/** Call abort function. */
ASF_COREDLL void die(const std::string &why);

/** Top-level logging.  Plugins should use plugin::log, not this routine. */
ASF_COREDLL void log(int level,const std::string &why);

/** Top-level logging verbosity. */
ASF_COREDLL void set_log_level(int level);

/******************************** Type ************************/
/**
An asf::type describes the type of a particular object, like
a plugin or parameter.  This type information is used for typechecking
throughout the system, for debugging, for version control, and to display
this information to the user.
*/
class ASF_COREDLL type {
public:
	/** I expect type strings will always be compile-time constants.
	  "const char *" lets those strings stay in readonly memory. 
	  The other logical choice here, "std::string", would cause string
	  constants to be copied into heap memory, which is a waste.
	*/
	typedef const char *string_t;
	
	/**
	  Create a new type with these fields.
	*/
	type(const type *parent,string_t name,
		string_t description="Unknown",string_t authors="Unknown",
		double version=1.0,int flags=0)
		:m_parent(parent),
		m_name(name), m_description(description), m_authors(authors), 
		m_version(version), m_flags(flags) {}
	
	/// Return the type of this object's superclass (parent class).
	///   You can keep following parent links until you reach a NULL pointer.
	inline const type *parent(void) const {return m_parent;}

	/// Short, machine-matched type name of the object, like "int", or "multilook".
	inline string_t name(void) const {return m_name;}

	/// Long, human-readable description of what the object does.
	inline string_t description(void) const {return m_description;}

	/// Long, human-readable description of the object's author and change dates.
	inline string_t authors(void) const {return m_authors;}
	
	/// Implementation-dependent integer flags.
	inline int flags(void) const {return m_flags;}

	/// Floating point version number of this object, like 1.0 or 2.4.
	/// Version numbers should compare like real numbers, so 2.11 comes before 2.2.
	/// Only increment the integer part of the version number when backward compatability
	///   has been lost (i.e., fields or functionality have been removed).
	inline double version(void) const {return m_version;}
	
	/// Print this type to the screen.  If verbose is false, is a one-line
	///   thing like "int v1.0"; if true, it's got everything in a multi-line format.
	void print(bool verbose=false,FILE *dest=stdout) const;
private:
	const type *m_parent;
	string_t m_name,m_description,m_authors; 
	double m_version;
	int m_flags;
};

/**
An asf::type_match determines if a particular type is 
acceptable to us.  By default, all the matcher does is check names.
*/
class ASF_COREDLL type_match {
public:
	type_match(type::string_t name) :m_name(name) {}
	virtual ~type_match();
	/** Return true if this type is acceptable to us. */
	virtual bool matches(const type *s) const;
	/** Return true if this type, or any parent type, matches. */
	bool matches_any(const type *s) const;
	/** Return the typename we will accept. This isn't strictly needed,
	   but dramatically accelerates matching over large databases. */
	type::string_t get_match_name(void) const {return m_name;}
protected:
	type::string_t m_name; ///< Data typename we're trying to match.
};
/**
A kind of asf::type_match that also checks version numbers.
*/
class ASF_COREDLL type_version_match : public type_match {
public:
	/** Check the type name, that the version number is at least this,
	    and that the major version number isn't greater than this. */
	type_version_match(type::string_t name,double version);
	virtual bool matches(const type *s) const;
protected:
	float version_lo, version_hi; ///< Version number range we're trying to match
};

/***************************** Parameters ****************************/


/** 
An asf::parameter is a value passed in to or out of a plugin.
In the current design, the plugin library and the plugins themselves
*always* reference plugins via pointers, but *never* allocate or 
delete the parameters.  Allocation is done from "outside" (during
program parsing) using asf::registry::parameter_factory.

All parameters inherit from PUP::able (see pup.h) so that they can 
be saved to disk and read back, sent across networks (see pup_mpi.h),
and displayed to the user.  

Every subclass of asf::parameter MUST include the following:
	- A "const asf::type my_subclass::static_type", used for debugging and 
	  static type checking.  You've got to define this in your .cpp file.
	- A working migration constructor (taking PupMigrateMessage, and preparing for call to pup)
	- A working pup routine (see pup/pup.h)
	- A call to the "ASF_parameter_class" macro in the class
	- A call to the "ASF_parameter_header" macro in the header file *outside* the namespace. 
	- A call to the "ASF_parameter_def" macro in the .cpp file.
	- A call to the "ASF_parameter_reg" macro at registration time.
Yes, this is indeed ugly.  But luckily most plugins won't need new parameter types.
*/
class ASF_COREDLL parameter : public PUP::able {
public:
	/** Normal constructor.  May take parameters. */
	parameter() {}
	
	/** "Migration constructor" used before pup routine. */
	parameter(PupMigrateMessage *m) :PUP::able(m) {}

	/** Pup routine: used to save/restore, send/recv this object.  
	   You MUST call your superclass's pup from here!  See pup.h */
	virtual void pup(PUP::er &p);
	
	/** Read and write routines.  By default, these use PUP::toTextFile, but
	   you can override them with routines to read older file formats as well. */
	virtual void read(FILE *f);
	virtual void write(FILE *f);
	virtual void print(FILE *f=NULL);
	/* PUP::able has a "clone" method that returns a new copy of this object */
	
	/** Return NULL if these objects are totally equal, or a human-readable string
	   describing the differences (in general terms). */
	virtual const char *differences(const parameter &p2); 
	
/** Use this macro inside your parameter class */
#define ASF_parameter_class(className) public: \
	static const type static_type; /* name and author of class (defined by user) */ \
	inline static const asf::type *static_get_type(void) {return &static_type;} \
	virtual const asf::type *get_type(void) const; /* dynamic typechecking */ \
	static asf::parameter *call_parameter_constructor(void); \
	inline className *clone(void) const {return (className *)PUP::able::clone();} \
	PUPable_decl_inside(className) /* PUP::able support */

/** Use this macro inside your header file, but outside your namespace */
#define ASF_parameter_header(classWithNamespace) \
	PUPable_decl_outside(classWithNamespace) /* PUP::able support */

/** Use *this* macro inside your .cpp file */
#define ASF_parameter_def(classWithNamespace) \
	const asf::type *classWithNamespace::get_type(void) const { return &classWithNamespace::static_type; } \
	asf::parameter *classWithNamespace::call_parameter_constructor(void) \
		{return new classWithNamespace((PupMigrateMessage *)0);} \
	PUPable_def(classWithNamespace) 

/** Use this macro to register your parameter during your register routine */
#define ASF_parameter_reg(registrar,classWithNamespace) \
	(registrar).parameters.add(&classWithNamespace::static_type, \
		(asf::generic_allocate_function_t)(classWithNamespace::call_parameter_constructor));\
	PUPable_reg(classWithNamespace);
	
	ASF_parameter_class(parameter)
};


/**
  An integer parameter.
*/
class ASF_COREDLL parameter_int : public parameter {
public:
	/** Real useful work of class: pretend to be an actual integer */
	int value; /**< The value of our integer */
	parameter_int(int v=0) :value(v) {} /**< Create like an integer */
	inline int operator=(int v) {value=v; return value;} /**< Assign like an integer */
	inline operator int (void) const {return value;} /**< Read like an integer */
	
	/** parameter boilerplate */
	parameter_int(PupMigrateMessage *m) :parameter(m) {value=-123456789;}
	virtual void pup(PUP::er &p);
	ASF_parameter_class(parameter_int)
};

/**
  A floating-point parameter.
*/
class ASF_COREDLL parameter_real : public parameter {
public:
	/** Real useful work of class: pretend to be an actual double */
	double value; /**< The value we store */
	parameter_real(double v=0) :value(v) {} /**< Create like a double */
	inline double operator=(double v) {value=v; return value;} /**< Assign like a double */
	inline operator double (void) const {return value;} /**< Read like a double */
	
	/** parameter boilerplate */
	parameter_real(PupMigrateMessage *m) :parameter(m) {value=-12345.6789;}
	virtual void pup(PUP::er &p);
	ASF_parameter_class(parameter_real)
};

/**
  A string parameter.
*/
class ASF_COREDLL parameter_string : public parameter {
public:
	/** Real useful work of class: pretend to be an actual string */
	std::string value; /**< The value we store */
	parameter_string(std::string v=""); /**< Create like a string */
	inline std::string operator=(std::string v) {value=v; return value;} /**< Assign like a string */
	inline operator std::string (void) const {return value;} /**< Read like a string */
	inline const char * c_str(void) const {return value.c_str();} /**< Convert to a const char * */
	
	/** parameter boilerplate */
	parameter_string(PupMigrateMessage *m) :parameter(m) {value="__uninitialized__";}
	virtual void pup(PUP::er &p);
	ASF_parameter_class(parameter_string)
};

/** 
 Someday, parameter_filename will be more than just a string.  Not yet, tho'.
*/
typedef parameter_string parameter_filename;

/**
  An array of generic parameter objects.  Basically a wrapper around
  std::vector that interfaces properly with the type registration system.
  This class should not be used by itself--only use the typesafe 
  templated version below.
*/
class ASF_COREDLL parameter_array_generic : public parameter {
	/// Here are the elements of our array.
	///   These pointers are owned by this class.
	std::vector<parameter *> elts;
public:
	/** Create a new empty array. */
	parameter_array_generic();
	virtual ~parameter_array_generic();
	
/** Wrappers around std::vector functionality. */
	/// Return the number of elements in our array.
	unsigned int size(void) const {return elts.size();}
	
	/// Extract the i'th element of our array.
	///   Will abort if i is out of bounds.
	parameter *get(int i) const;
	
	/// Add this parameter to our array.  Note this is the only way to 
	///   add data to this array.
	void push_back(parameter *p);
	
	/// Resize our array to contain n elements.  The new elements will get value p.
	void resize(unsigned int n,parameter *p=0);
	
	/// Reserve space for up to n elements.
	void reserve(unsigned int n) {elts.reserve(n);}

/** parameter boilerplate */
	virtual void pup(PUP::er &p);
	parameter_array_generic(PupMigrateMessage *m) :parameter(m) {}
	ASF_parameter_class(parameter_array_generic)
};

/**
  This class provides a typesafe wrapper around parameter_array_generic.
  It is easier to use than parameter_array_generic, and safer too.
*/
template <class T>
class ASF_COREDLL parameter_array : public parameter_array_generic {
	typedef parameter_array<T> self;
	typedef parameter_array_generic super;
	
/* WARNING: because the registration system will allocate a parameter_array_generic
and cast it to this class, this class CANNOT contain any data members, virtual methods, etc.
This enum will become a divide-by-zero if you mistakenly add data to this class. */
	enum {assert_same_size=1/(sizeof(self)==sizeof(super))};
public:
	/// Extract the i'th element of our array.
	///   Will abort if i is out of bounds.
	inline T *get(int i) const {return (T *)super::get(i);}
	
	/// Extract the i'th element of our array; or abort if i is out of bounds.
	inline T *operator[](int i) const {return (T *)super::get(i);}
	
	/// Add this parameter to our array.
	inline void push_back(T *p) {super::push_back(p);}
	
	/// Resize our array to contain n elements.  The new elements will get value p.
	inline void resize(unsigned int n,T *p=0) {super::resize(n,p);}
	
	/// FIXME: need to somehow be able to uniquely identify array of T's.
	///   Current choices are to say we're "an array" (of anything) or "we're T".
	inline static const type *static_get_type(void) {return &super::static_type;}
};



/************************** Parameter Constraints ********************
Used for:
   1.) parameter_int:
Indicating the range of values acceptable in an "int" (parameter_int_constraint)
   2.) parameter_image:
Indicating the image tile sizes acceptable (parameter_image_constraint)
*/
class ASF_COREDLL parameter_constraint {
public:
	virtual ~parameter_constraint();
	/* Return the types of parameter this constraint can apply to */
	virtual const asf::type *get_type(void) =0;
	
	virtual void print(void) =0;
	/* depending on the type, other virtual methods are available... */
};

/**
  Constrains value of a parameter_int.
*/
class ASF_COREDLL parameter_int_constraint {
public:
	virtual const asf::type *get_type(void);
	virtual void print(void);
	
	/** A value_t describes one possible value for the integer parameter.
	  You would normally declare a static array of value_t's like this:
	  
	  const static asf::parameter_int_constraint::value_t my_vals[]={
	  	{0,"Foozis", "Enable the Foozis-1882 defronking method"},
	  	{1,"Whazis", "Enable the Whazis-1936 defronker"},
	  	{2,"Wupmann", "Enable the Wup-Mann 2005 defronker"},
		{-1,0,0} // NULL terminated list
	  };
	*/
	struct value_t {
		/// Value parameter would take, or 0 for the last value.
		int value;
		/// Short human-readable name of value.  Use NULL to indicate last value.
		const char *name;
		/// Longish human-readable description of value, or NULL if no description.
		const char *desc;
	};
	
	/// Describes the sort of constraint we apply
	typedef enum {
		constraint_range=1, /* only a range constraint */
		constraint_value=2, /* one of the specifically listed values */
		constraint_bitmask=3 /* any combination of selected values */
	} constraint_t;

	/** The value of the parameter must lie between lo and hi */
	parameter_int_constraint(int lo,int hi=((~0U)>>1));
	/** The value of the parameter must be one of these listed values.
		End the table of values with a NULL name.*/
	parameter_int_constraint(const value_t *values,constraint_t type=constraint_value);
	
	/* The kind of constraint we represent */
	constraint_t type;
	/* Range of acceptable values (inclusive range, lo<=x && x<=hi) */
	int lo,hi;
	/* List of acceptable values */
	const value_t *values;
};


/************************** Plugins *********************/
/**
"plugin_parameters" is called will each parameter needed by a plugin.
Plugins keep track of their parameters using simple pointers.
*/
class ASF_COREDLL plugin_parameters {
public:
	/// Return true if we will actually get an "execute" call after creation.
	///  This is false when we're just collecting a list of parameters from the plugin.
	inline bool will_execute(void) {return m_execute;}
	
	typedef enum {
		dir_input=0, /**< Input parameter; will be read by program */
		dir_optional, /**< Input parameter; if not NULL, will be read by program */
		dir_output,   /**< Output parameter, will be set by program */
		dir_last /**< no more parameter types */
	} dir_t;
	/// Return a short string describing this direction
	static const char *dir2str(dir_t d);
	
	/**
	  The plugin takes a parameter with this name and type.
	  The parameter will be pointed to from *ptr, which MUST 
	    be set by this routine, but may be set to NULL.
	  Normally, this routine is called by "input", "optional", or "output", below.
	*/
	virtual bool param(type::string_t name,const type *t,parameter **ptr,
		parameter_constraint *constraint,type::string_t long_description,
		dir_t d)=0;
	
	virtual ~plugin_parameters();
protected:
	plugin_parameters(bool execute) :m_execute(execute) {}
private:
	bool m_execute;
};

/** Plugins call this routine exactly once for each of their 
input parameters, which will always be read.  You call this routine
with a pointer to your parameter class pointer, like "&my_foo",
where my_foo is declared like "parameter_int *my_foo".
*/
template <class parameter_subclass>
inline bool input(plugin_parameters &p,type::string_t name,parameter_subclass **ptr,
	parameter_constraint *constraint=NULL,type::string_t long_description=NULL) {
	return p.param(name,parameter_subclass::static_get_type(),(parameter **)ptr,
		constraint,long_description,plugin_parameters::dir_input);
}
/** Plugins call this routine exactly once for each of their 
optional input parameters, which will be used iff it is nonzero.
*/
template <class parameter_subclass>
inline bool optional(plugin_parameters &p,type::string_t name,parameter_subclass **ptr,
	parameter_constraint *constraint=NULL,type::string_t long_description=NULL) {
	return p.param(name,parameter_subclass::static_get_type(),(parameter **)ptr,
		constraint,long_description,plugin_parameters::dir_optional);
}
/** Plugins call this routine exactly once for each of their 
output parameters, which will be filled out by the program.
*/
template <class parameter_subclass>
inline bool output(plugin_parameters &p,type::string_t name,parameter_subclass **ptr,
	parameter_constraint *constraint=NULL,type::string_t long_description=NULL) {
	return p.param(name,parameter_subclass::static_get_type(),(parameter **)ptr,
		constraint,long_description,plugin_parameters::dir_output);
}


/**
 Collects a list of parameters needed *by* the plugin.
 Doesn't actually give the plugin anything back in return, tho'.
*/
class ASF_COREDLL plugin_parameter_signature : public plugin_parameters {
public:
	plugin_parameter_signature();
	~plugin_parameter_signature();

	/// Information about one parameter.
	class parameter_info {
	public:
		/// Name the plugin uses for this parameter
		type::string_t name;
		
		/// Data type of the plugin's parameter.
		///   FIXME: should this be a type_match object?
		const type *t;
		
		/// Constraints on this parameter.
		parameter_constraint *constraint;
		
		/// Long human-readable description of the parameter's function
		type::string_t long_description;
		
		/// Input, optional, or output direction
		dir_t dir;
	};
	/// A list of parameters.
	typedef std::vector<parameter_info *> params;
	/// Information about parameters, indexed by direction
	params bydir[dir_last];
	const params &inputs(void) const 
		{return bydir[dir_input];}
	const params &optionals(void) const 
		{return bydir[dir_optional];}
	const params &outputs(void) const 
		{return bydir[dir_output];}
	
	/// Information about parameters, indexed by name
	typedef std::map<std::string,parameter_info *> byname_t;
	byname_t byname;
	
	/**
	  Store this parameter into our list.  
	  Sets all parameter pointers to NULL.
	*/
	virtual bool param(type::string_t name,const type *t,parameter **ptr,
		parameter_constraint *constraint,type::string_t long_description,
		dir_t d);
	
	/// Print out our list of parameters, in a human-friendly format.
	void print(FILE *dest=stdout);
	
	/**
	  Return info about this parameter.  Returns NULL if the 
	  parameter does not exist.
	*/
	const parameter_info *lookupNULL(type::string_t name) const;
};

/**
 Provides all parameters to a plugin.
*/
class ASF_COREDLL plugin_parameter_list : public plugin_parameters {
public:
	plugin_parameter_list();
	~plugin_parameter_list();
	
	/**
	  Add this parameter under this name to our table.
	*/
	void add(type::string_t name,parameter *p);
	
	/**
	  Look up and set the value of this parameter from our table.
	  If the types don't match, die.
	  If the parameter doesn't exist in our table,
	    die unless d==dir_optional, when we set *ptr=0.
	*/
	virtual bool param(type::string_t name,const type *t,parameter **ptr,
		parameter_constraint *constraint,type::string_t long_description,
		dir_t d);
	
private:
	typedef std::map<std::string,parameter *> table_t;
	/* The big table of parameters, indexed by name */
	std::map<std::string,parameter *> table;
};

/** Use this macro inside your plugin class (unless you have an abstract class, in which case nothing is needed) */
#define ASF_plugin_class(className) public: \
	static const asf::type static_type; \
	virtual const asf::type *get_type(void) const  { return &className::static_type; } /* dynamic typecheck */ \
	static asf::plugin *call_plugin_constructor(asf::plugin_parameters &p); /* Creation support */ \
	virtual const asf::plugin_parameter_signature *get_signature(void) const;

/** Use *this* macro inside your .cpp file (unless you have an abstract class, in which case nothing is needed)*/
#define ASF_plugin_def(classWithNamespace) \
	asf::plugin *classWithNamespace::call_plugin_constructor(asf::plugin_parameters &p) \
		{return new classWithNamespace(p);} \
	const asf::plugin_parameter_signature *classWithNamespace::get_signature(void) const { \
		static asf::plugin_parameter_signature *sig=0; \
		if (sig==0) { \
			asf::plugin_parameter_signature *s=new asf::plugin_parameter_signature; \
			delete new classWithNamespace(*s); \
			sig=s; \
		} \
		return sig; \
	}

/** Use *this* macro inside your .cpp file to make a single-plugin DLL. 
It actually will work OK even if you don't use it as a DLL and link statically.
*/
#define ASF_plugin_dll(classWithNamespace,dllName) \
	ASF_PLUGIN_EXPORT int asf_plugin_init_##dllName(asf::registry *registrar) { \
		ASF_plugin_reg(*registrar,classWithNamespace); \
		return asf::plugin_init_function_ok; \
	}

/** Use this macro to register your parameter during your register routine (unless you have an abstract class). */
#define ASF_plugin_reg(registrar,classWithNamespace) \
	(registrar).plugins.add(&classWithNamespace::static_type, \
		(asf::generic_allocate_function_t)(classWithNamespace::call_plugin_constructor));
		
class ASF_COREDLL location_function; /* forward declaration; really in asf/image.h */

/**
A "plugin" is a piece of code that actually does some work.
Specifically, a plugin converts a set of input parameters 
  (some of which are optional) into output parameters.
Every plugin requires:
	- A "const asf::type my_subclass::static_type", used for debugging and 
	  static type checking.  You've got to define this in your .cpp file.
	- A set of parameters, stored as pointers in the class.
	- A constructor taking "plugin_parameters &", where
	   the plugin makes calls to asf::input, asf::optional, and asf::output
	   for each parameter.  Be sure to pass the "plugin_parameters&"
	   up to your superclass's constructor.
	- An implementation of the "execute" method, which reads from the
	   input parameters and writes to ALL the output parameters.
	- A destructor, if any memory was allocated in the constructor.
	- A call to the ASF_plugin_class, ASF_plugin_def, and ASF_plugin_reg macros.
	
From the point of view of an asf::plugin, you request your asf::parameter objects in your class constructor using the asf::input, asf::output, and asf::optional routines.  In your asf::plugin::execute routine, you compute all your output parameters from your input parameters. 

If your plugin has image parameters, you need to set up the image sizes and metadata in your asf::plugin::meta_execute routine, which runs exactly once.  
*/
class ASF_COREDLL plugin {
public:
	/**
	  Bitwise-OR'd flags stored in plugin's type.flags() field.
	*/
	typedef enum {
		flag_control_flow=1<<0 /* If set, this is a plugin_control_flow */
	} flag_plugins;

	static const type static_type; /* For type checking */
	plugin(plugin_parameters &param); /* Constructor */
	plugin_parameters &get_parameters(void) const {return parameters;}
	virtual ~plugin(); /* Destructor */
	
	/** Return the asf::type of this plugin.  This routine is declared
	  automatically by the ASF_plugin_class and ASF_plugin_def macros. */
	virtual const asf::type *get_type(void) const =0;
	/** Return the parameter signature of this plugin.  This routine is declared
	  automatically by the ASF_plugin_class and ASF_plugin_def macros. */
	virtual const asf::plugin_parameter_signature *get_signature(void) const =0;
	
	/**
	 Initialize the output variables' metadata based on the 
	 input variables' metadata.  This routine is called exactly once
	 before any execute() call.
	 
	 Default implementation does nothing.
	*/
	virtual void meta_execute(void);
	
	/**
	  Execute this plugin--compute its output variables based
	  on its input variables.  This routine may be called several times
	  for different input and output values, or different tiles of the
	  input and output images. 
	  
	  This is where most of your plugin code will go.
	*/
	virtual void execute(void) =0;
	
	/** Return true if the execute method of this class has side effects:
	  for example, if it changes files, or accumulates some value.
	  
	  Plugins that have side effects are called "sinks", and
	  are always executed.   Typical sinks include file output 
	  statements and file output plugins, or array accumulators.
	  
	  Plugins without side effects are called "pure", and hence 
	  may be skipped or executed multiple times for time and 
	  space efficiency, or for parallelism.  All typical image-to-image 
	  processing plugins are pure. 
	  
	  By default, a plugin that has output parameters is assumed to be pure.
	  A plugin without any output parameters is assumed to be a sink, 
	  since it must be modifying the outside world.
	*/
	virtual bool has_side_effects(void) const;
	
	/**
	 Return a new location_function that gives 
	 input image i coordinates given output image o coordinates.
	 (i and o are zero-based image parameter counts, listed in 
	 parameter declaration order.)  For example, image_in_from_out(1,0)
	 should return a mapping from your first output image's meta coordinates
	 to your second input image's meta coordinates.
	 
	 The location function is used by execute_plugin::render_inputs
	 to determine which input image tiles need to be passed to 
	 your execute routine in order to compute a given output image tile.
	 
	 The default implementation always returns NULL, meaning no images are used.
	*/
	virtual location_function *image_in_from_out(int i,int o);
	
	/** Common-to-all-plugins logging operation.
	    Level is the first verbosity level to show this print;
	      so lower levels are more important.
	*/
	void log(int level,const char *format ...);
	/** Like above, but at level 1 by default.  Drop-in replacement for printf. */
	void log(const char *format ...);
	
	/// FIXME: Is progress updating needed here?  Or can it be done outside?
	
protected:
	/* optional */ parameter_int *log_verbosity; /* Not set: no printouts.  Set: print if level<=log_verbosity  */
	/* optional */ parameter_string *log_name; /* Logfile name (default is stdout) */
	FILE *log_dest; /* Destination file for logging: NULL, stdout, or a file */
	/* Parameters we were created with. */
	plugin_parameters &parameters;
};


/************************** Registration **********************
Helps keep track of all the plugins and parameters in a real program.
*/
/** Standin for a real function that allocates an object. 
   You MUST typecast this function pointer to a real allocate 
   function before calling it.
*/
typedef void *(*generic_allocate_function_t)(void *alloc_parameter);

/**
Associates "type" objects with allocation functions for one particular
kind of object.

NOTE: This has nothing to do with the Microsoft Windows registry.
*/
class ASF_COREDLL type_registry {
public:
	/** One entry in the registry: a type, and an allocation function. */
	class entry {
	public:
		entry() :t(0), f(0) {}
		entry(const type *t_,generic_allocate_function_t f_)
			:t(t_), f(f_) {}
		const type *get_type(void) const {return t;}
		type::string_t get_name(void) const {return t->name();}
		generic_allocate_function_t get_function(void) const {return f;}
	private:
		const type *t;
		generic_allocate_function_t f;
	};
	
	type_registry(); ~type_registry();
	
	/** Register this type and allocation function with us. */
	void add(const type *t,generic_allocate_function_t f);
	
	/** Return the unique entry that matches this request.  If more than
	  one entry matches, abort; but if none matches, return NULL. */
	const entry *lookupNULL(const type_match &m) const;
	
	/** Return the unique entry that matches this request, or abort. */
	const entry *lookup(const type_match &m) const;
	
	typedef std::vector<entry> entry_list;
	
	/** Return a list of all the entries that match this request. */
	entry_list multilookup(const type_match &m,const char *mName=NULL) const;
	
	/** Return a list of all the direct children of this parent class */
	entry_list children(const type_match &m) const;
	
	/** Print out a description of the types derived from this class. */
	void print_types(const type_match &m,FILE *dest=stdout,int tab_level=0);
	
	/** Add all this registry's entries to ours. 
	  Where entries overlap, use r's, and throw away ours.
	*/
	void merge(const type_registry &r);
private:
	
	/** A list of all registered types, indexed by their own name.
	   Does not include entries under parent names, and so must be unique. 
	*/
	typedef std::map<std::string,entry> name_t;
	name_t byname;
	
	/** A list of children of a class */
	typedef std::vector<std::string> childlist_t;
	
	/** A list of all registered child types, indexed by their parents'
	 names.  For example, "parameter" might have many entries here, 
	 because it has many subclasses; but "int" has no entry here, 
	 since it has no subclasses.
	*/
	typedef std::map<std::string,childlist_t> parent_t;
	parent_t byparent;
};

/**
The registry keeps track of all registered plugins and parameter values.
For each plugin or parameter, the registry stores:
	- asf::type information (name, author, version).
	- A creation function, that allocates a new plugin or parameter object.
*/
class ASF_COREDLL registry {
public:
	/** Allocation routine for "parameter" objects */
	typedef parameter *(*parameter_allocation_function)(void);
	/** All existing "parameter" objects, and their allocation functions. */
	type_registry parameters;
	/** Look up the allocation routine (calls new) for this kind of parameter. */
	parameter_allocation_function parameter_factory(const type_match &m) const {
		return (parameter_allocation_function)parameters.lookup(m)->get_function();
	}
	/** Throw away this parameter.  Currently just calls delete, but could
	   eventually do some recycling. */
	void parameter_free(parameter *p) { delete p; }
	
	/** Allocation routine for "plugin" objects */
	typedef plugin *(*plugin_allocation_function)(plugin_parameters &p);
	/** All existing "plugin" objects, and their allocation functions. */
	type_registry plugins;
	/** Look up the allocation routine for this plugin. */
	plugin_allocation_function plugin_factory(const type_match &m) const {
		return (plugin_allocation_function)plugins.lookup(m)->get_function();
	}
	/** Look up the signature of this plugin */
	inline plugin_parameter_signature plugin_signature(const type_match &m) const {
		plugin_parameter_signature param;
		delete plugin_factory(m)(param);
		return param;
	}
	
	/** Print out the type information in this registry */
	void print_types(FILE *dest=stdout);

	/** Add all this registry's entries to ours. */
	void merge(const registry &r) {
		parameters.merge(r.parameters);
		plugins.merge(r.plugins);
	}
};

/** Pup the name and version of this type.
  Only works with packing or sizing PUP::er's. */
ASF_COREDLL void pup_type_packing(PUP::er &p,const type *t);
/** Look up this parameter type in the system registry. 
  This is the counterpart of pup_type_packing. */
ASF_COREDLL registry::parameter_allocation_function pup_type_unpack_parameter(PUP::er &p);


/** Register all builtin types to this registrar, and start using 
  this registry as the new central system registry. */
ASF_COREDLL void system_register(asf::registry &registrar);

/** Return the current central system registry.  Aborts if none is set. */
ASF_COREDLL asf::registry &system_registry(void);


/**
This initialization function is called by the dynamic loader
asf::plugin_loader to register everything in a dynamically-linked 
library.  For a single-plugin library, use ASF_plugin_dll to define
this routine.

For a more complicated library, define an init routine like this:
<pre>
ASF_PLUGIN_EXPORT int asf_plugin_init(asf::registry *r) {
	...
	return asf::plugin_init_function_ok;
}
</pre>

This routine will be called exactly once when the library is loaded.
It should register all plugins (using ASF_plugin_reg) and parameters
(using ASF_parameter_reg) that you define.
*/
typedef int (*plugin_init_function_t)(asf::registry *r);
enum {plugin_init_function_ok=321};



} /* End ASF namespace */

ASF_parameter_header(asf::parameter);
ASF_parameter_header(asf::parameter_int);
ASF_parameter_header(asf::parameter_real);
ASF_parameter_header(asf::parameter_string);
ASF_parameter_header(asf::parameter_array_generic);


#endif

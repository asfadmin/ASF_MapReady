/*
ASF Plugin Architecture
PROTOTYPE 1-L

Plugins used to manage control flow.

Orion Sky Lawlor, olawlor@acm.org, 2005/09/14.
*/
#ifndef __ASF_PLUGIN_CONTROL_H
#define __ASF_PLUGIN_CONTROL_H

#include "asf/plugin.h"

namespace asf {

/**
Represents a "basic block"--a list of plugins to execute.
   FIXME: Should keep track of plugins (with parameters) by name
      or global ID or something for parallel execution.
*/
class ASF_COREDLL parameter_control_list : public parameter {
public:
	/** Add a plugin to our list */
	void add(plugin *p) {list.push_back(p);}

	/** Return the size of our list of plugins */
	inline unsigned int size(void) const {return list.size();}
	/** Return our i'th plugin */
	inline plugin *index(int i) const {return list[i];}
	
	/** meta_execute all our plugins */
	void meta_execute(void) const {
		for (unsigned int i=0;i<size();i++)
			index(i)->meta_execute();
	}
	
	/** Execute all our plugins */
	void execute(void) const {
		for (unsigned int i=0;i<size();i++)
			index(i)->execute();
	}
	
	/** parameter boilerplate */
	parameter_control_list() {}
	parameter_control_list(PupMigrateMessage *m) :parameter(m) {}
	virtual void pup(PUP::er &p);
	ASF_parameter_class(parameter_control_list)
	
	virtual void print(FILE *f);
private:
	/** Plugins to execute, in execution order */
	std::vector<plugin *> list;
};

/**
 Execute the plugins in this list in some sensible order,
 creating tiles as needed, and pruning useless branches.
*/
void ASF_COREDLL execute_list(const asf::parameter_control_list &list);

/**
Superclass of all control-flow managing plugins
(loops, if statements, switches, etc.).
*/
class ASF_COREDLL plugin_control_flow : public plugin {
public:
	/**
	 Initialize the output variables' metadata based on the 
	 input variables' metadata.  Recurses into each sub-plugin
	 in all our control lists.
	*/
	virtual void meta_execute(void);
	
	/**
	  Default "execute" method, used to run the control
	   flow in serial single-threaded mode. 
	  Subclasses should *never* override this method--
	   only use "execute_iteration", which may be called 
	   from *outside* for parallel or multithreaded execution.
	*/
	virtual void execute(void);
	
	/**
	 Return the number of iterations that we are sure we
	  can execute.  This controls the degree of task parallelism
	  available.  Note that the actual number of iterations may
	  be larger than this *if* execute_iteration continues to return
	  nonnegative values.
	 This routine is called once before calling execute_iteration.
	 The default implementation returns "1".
	*/
	virtual int execute_iterations(void);
	
	/**
	  Return the list of commands to execute on this step.
	  The returned value must be between 0 and get_list_count-1
	  to continue execution.  When the returned value is -1, 
	  execution stops.
	  
	  In serial, this is always called with step==0, then with 
	  larger and larger steps until it returns -1.
	*/
	virtual int execute_iteration(int stepNo)=0;
	
	/**
	  Return the i'th list of commands.  0<=i && i<get_list_count().
	*/
	inline const parameter_control_list *get_list(int i) const 
		{return lists[i];}
	
	/**
	  Return the total number of lists we contain
	*/
	inline int get_list_count(void) const 
		{return lists.size();}

	static const type static_type; /* For type checking */
	plugin_control_flow(plugin_parameters &param); /* constructor */
	~plugin_control_flow();

protected:
	/** Lists of commands we might execute. 
	   lists[0] is prepared by the superclass from the "list" parameter.
	*/
	std::vector<parameter_control_list *> lists;
};

/**
"for loop" control-flow plugin.
*/
class ASF_COREDLL plugin_for : public plugin_control_flow {
public:
	plugin_for(plugin_parameters &param);
	virtual int execute_iterations(void);
	virtual int execute_iteration(int stepNo);
	ASF_plugin_class(plugin_for)
protected:
	parameter_int *min, *max, *step_param, *index;
};

/**
"if" conditional control-flow plugin.
*/
class ASF_COREDLL plugin_if : public plugin_control_flow {
public:
	plugin_if(plugin_parameters &param);
	virtual int execute_iteration(int stepNo);
	ASF_plugin_class(plugin_if)
protected:
	parameter_int *condition;
};

} /* End ASF namespace */

ASF_parameter_header(asf::parameter_control_list);


#endif

PPS Server

1. Parse command line options
	pps_server [-c <config_file_name>] [-t <number_of_threads>]
   	* Assign default values (for PPS and IMS server, user, password, etc)
          when necessary
	* max number of threads = 10
	* default number of threads = 10
	* default config file = .pps_config

2. Create and initilize a mutex lock 
   for gathering statistics
   (has to be done before anything else)

3. Create and initilize a pool of dbproc structures
	* one entry in the table per thread
	* each table entry contains the structures used for
		- PPS database queries
		- IMS database queries
		- transaction TM processing

4. Create and initilize a mutex << locks >> used for
	* accessing PPS database tables
	* dying flag (inform the server is a kill rpc has been received)
	* counting number of rpcs in progress

5. DCE server initialization

6. Listen for rpcs ...

/* 
-- commented out because in our make files, we search first here 
-- and then in the ~aps/aps/include directory.  we want to make sure 
-- that this ASF_PERF table is disabled by making this file be the 
-- one that is included during compilation.  and we want to comment 
-- this code out:  

	#define ASF_PERF_T_AIM	0
	#define NUM_ASF_PERF_COLS 1

	#define CAST_ASF_PERF_T_AIM	*( DBREAL *)

*/

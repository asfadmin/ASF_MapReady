/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*--------------------------------------------------*/
/*   usage: prog_name [entry_name]                  */
/*                                                  */
/*           if entry_name is not specified,        */
/*           then RPC_DEFAULT_ENTRY is used.        */
/*--------------------------------------------------*/
#include <stdio.h>
#include <dce/rpc.h>
#include <dce/dce_error.h>

#include "messages.h"

static char SccsFileId[] = "@(#)stopserver.c	1.1  11/21/96";

/* dummy */
rpc_binding_vector_t 	*binding_vector;
unsigned_char_t 	*entry_name;

char ProgName[80];

main(
int    argc,
char** argv)
{
	const char* entry_name=0;
	const char* uuid_string="000e4f7a-dba1-1fd4-ad1d-894f6cdeaa77";
	uuid_t                 server_uuid;
	extern rpc_if_handle_t        messages_v1_0_s_ifspec;
        unsigned long          status;
	rpc_ns_handle_t        import_context;
	rpc_binding_handle_t   binding;
	boolean32              is_listening=0;
	unsigned char          error_string[dce_c_error_string_len];
	int                    err_status;

#if 0
	/*------------------------------------------------*/
	/* if server_uuid is used => "no more binding"    */
	/*------------------------------------------------*/
	/* get UUID from string */
	uuid_from_string((unsigned char*)uuid_string, &server_uuid, &status);
	if (status != uuid_s_ok)
	{
		fprintf(stderr, "Can't get UUID from string\n");
		exit(1);
	}
#endif

	if (argc >1)
		entry_name = argv[1];
        
	/* create an import context in the name service database */
	rpc_ns_binding_import_begin(
                            rpc_c_ns_syntax_default,
                            (unsigned_char_p_t) entry_name,
                            messages_v1_0_s_ifspec,
                            0,
                            &import_context,
                            &status);
	if (status != rpc_s_ok)
	{
		dce_error_inq_text(status, error_string, &err_status);
		fprintf(stderr, "Can't bind import: %s\n", error_string);
		exit(1);
	}

	/*------------------------------------------------------------*/
	/* search next compatible server in the name service database */
	/*------------------------------------------------------------*/
	rpc_ns_binding_import_next(import_context, &binding, &status);
	if (status == rpc_s_no_more_bindings)
	{
		if ( ! is_listening)
			fprintf(stderr, "Server is NOT RUNNING\n");

		/*--------------------------------------------*/
		/* free the binding handle and import context */
		/*--------------------------------------------*/
		rpc_binding_free(&binding, &status);
		rpc_ns_binding_import_done(&import_context, &status);

		return(0);
	}
	else if (status != rpc_s_ok)
	{
		dce_error_inq_text(status, error_string, &err_status);
		fprintf(stderr, "Bind next import failed: %s\n",
				error_string);
		exit(1);
	}

	/*-----------------------------------------------------*/
	/* found one, now resolve it into a fully bound handle */
	/*-----------------------------------------------------*/
	rpc_ep_resolve_binding(binding, messages_v1_0_s_ifspec, &status);
	if (status != rpc_s_ok)
	{
		dce_error_inq_text(status, error_string, &err_status);
		fprintf(stderr, "Can't resolve the bindind: %s\n",
					error_string);
		exit(1);
	}

	/*---------------------*/
	/* now stop the server */
	/*---------------------*/
	rpc_mgmt_stop_server_listening(binding, &status);
	if (status != rpc_s_ok)
	{
		dce_error_inq_text(status, error_string, &err_status);
		fprintf(stderr, "Stop server failed: %s\n", error_string);
		exit(1);
	}
	fprintf(stdout, "Server is stopped.\n"); fflush(stdout);

	/*---------------------------------*/
	/* free the current binding handle */
	/*---------------------------------*/
	rpc_binding_free(&binding, &status);

	return(0);

}/*main*/

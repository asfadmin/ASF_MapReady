#ifndef __CLA_H
#define __CLA_H

#define CHECK_ARG(num_args) if (currArg+num_args>argc) \
 {printf("   *****You need %i arguments for the keyword %s.\n\n",currArg-argc+num_args,argv[currArg-1]);\
 usage(argv[0]);} else currArg+=num_args;
#define GET_ARG(arg_num) argv[currArg-arg_num]

extern int currArg;

int strmatch(const char *key, const char *match);
void remove_args(int start, int end, int *argc, char **argv[]);

// flags
int detect_flag_options(int argc, char **argv, ...);
int extract_flag_options(int *argc, char ***argv, ... );

// doubles
int parse_double(const char * str, double * val);
int parse_double_option(int *i, int argc, char *argv[], int *specified,
                        double *value);
int extract_double_options(int *argc, char **argv[], double *val, ... );

// integers
int parse_int(const char * str, int * val);
int parse_int_option(int *i, int argc, char *argv[], int *specified,
		     int *value);
int extract_int_options(int *argc, char **argv[], int *val, ... );

// strings
int parse_string_option(int *i, int argc, char *argv[], int *specified,
			char *value);
int detect_string_options(int argc, char *argv[], char *val, ... );
int extract_string_options(int *argc, char **argv[], char *val, ... );

// log
void parse_log_options(int *argc, char **argv[]);

// parse out some common options: -license, -version, -log, -quiet
void handle_common_asf_args(int *argc, char **argv[], const char *prog_name);

#endif

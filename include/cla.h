#ifndef __CLA_H
#define __CLA_H

#define CHECK_ARG(num_args) if (currArg+num_args>argc) \
 {printf("   *****You need %i arguments for the keyword %s.\n\n",currArg-argc+num_args,argv[currArg-1]);\
 usage(argv[0]);} else currArg+=num_args;
#define GET_ARG(arg_num) argv[currArg-arg_num]

extern int currArg;

int strmatch(const char *key, const char *match);
int detect_flag_options(int argc, char **argv, ...);
int extract_flag_options(int *argc, char ***argv, ... );
int extract_double_options(int *argc, char **argv[], double *val, ... );

#endif

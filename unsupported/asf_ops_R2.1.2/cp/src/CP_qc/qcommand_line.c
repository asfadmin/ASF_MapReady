#include "qinclude.h"
#include "qdefines.h"     /*  contains all the #define's  */
#include "qfunc_decl.h"   /*  contains all the function declarations  */
#include "qglobal_var.h"  /*  contains all the global variables  */

#include "qversion.h"

static char sccsid_qcommand_line_c[] = "@(#)qcommand_line.c	1.21 97/05/20 18:57:40";

/*  This function parses the given command line, 
    and sets up a few global variables that might
    be altered by command line arguments */

void
parse_command_line(argument_count, argument_vector) 
   int   argument_count;
   char  *argument_vector[];
{
   int i;

   /* Initialize -- change later, if needed */
   _verbose_option   = NOT_VERBOSE;
   _colormap_option  = NEW_CMAP;

   _order_id_string = (char *) malloc(13 * sizeof(char));
   sprintf(_order_id_string, "UNKNOWN");

   _avg_file_flag = 0;
   _complex_scale_parameter = 0.0;

   /* If the command was invoked all by itself, */
   if (argument_count == 1) {
      print_usage();
      exit(QC_HELP);
   }

   /* parse command line */
   for (i = 1; i < argument_count; i++) {
     /* -imagefile denotes the file name  */
     if (! strcmp(argument_vector[i], "\-imagefile")) {    
       /* set up _filename, used later */
       _filename = (char *) malloc(200 * sizeof(char));
       sprintf(_filename, "%s", argument_vector[++i]);
       cut_path_off_filename(argument_vector[i]);
     } 
     else if (! strcmp(argument_vector[i], "\-leaderfile")) {   
       /* -leaderfile denotes the leader file */
       _leader_file = (char *) malloc(200 * sizeof(char));
       sprintf(_leader_file, "%s", argument_vector[++i]);
     } 
     else if (! strcmp(argument_vector[i], "\-avgfile")) {   
       /* -average denotes the average file */
       _average_in_file = (char *) malloc(200 * sizeof(char));
       sprintf(_average_in_file, "%s", argument_vector[++i]);
       _avg_file_flag = 1;
     } 
     else if (! strcmp(argument_vector[i], "\-newcmap")) {   
       /* -l denotes the leader file */
       _colormap_option = NEW_CMAP;
     } 
     else if (! strcmp(argument_vector[i], "\-bottomcolor")) {   
       /* -l denotes the leader file */
       ++i;
     } 
     else if (! strcmp(argument_vector[i], "\-defaultcmap")) {   
       /* -l denotes the leader file */
       _colormap_option = DEFAULT_CMAP;
     } 
     else if (! strcmp(argument_vector[i], "\-verbose")) {   
       /* -verbose denotes turn on verbose mode */
       _verbose_option = VERBOSE;
     } 
     else if (! strcmp(argument_vector[i], "\-resourcefile")) {   
       /* -resourcefile denotes the name of the resouce file */
       _resource_file = (char *) malloc(200 * sizeof(char));
       sprintf(_resource_file, "%s", argument_vector[++i]);
     } 
     else if (! strcmp(argument_vector[i], "\-job")) {    
       /* -job denotes job ID  */
       sprintf(_order_id_string, "%12s", argument_vector[++i]);
     }
     else if (! strcmp(argument_vector[i], "\-r")) {    
       /* -r denotes resource  */
       make_announcement("Resource-value option entered", NO_LOG);
     }
     else if (! strcmp(argument_vector[i], "\-help")) {
       print_usage();
       exit(QC_HELP);
     }
     else if (! strcmp(argument_vector[i], "\-asfn")) {  
       /* These are for initASFLog */
       /* Ignore this one */
       i++;	
     }
     else if (! strcmp(argument_vector[i], "\-asflogloc")) {
       make_announcement("-asflogloc not supported", LOG);
       i++;
     }
     else if (! strcmp(argument_vector[i], "\-complex_scale_fac")) {
       /* -complex_scale_fac denotes (100.0 / mean) for complex image  */
       _complex_scale_parameter = atof(argument_vector[++i]) ;
     }
     else if (! strcmp(argument_vector[i], "\-V")) {
       printf("\n %s version %s\n\n", argument_vector[0], CP_QC_VERSION); ;
     }
     else {
       char problem[400];
       sprintf(problem, "unknown command line option %s\n", argument_vector[i]);
       make_announcement(problem, LOG);
     }
   }

  /* note:  all other arguments are ignored */
     
  if (! _leader_file) {
     make_announcement("Leader file not given", LOG);
     exit(QC_NO_LEADER_FILE);
  }

  if (! _filename) {
     make_announcement("Ceos file not given", LOG);
     exit(QC_NO_CEOS_FILE);
  }

  if ((! strcmp(_order_id_string, "UNKNOWN")) &&
      (_verbose_option == VERBOSE))
     make_announcement("Order ID not given", NO_LOG);
}


/* This function prints out legal command line options.  */

void
print_usage()
{

   printf("Usage: qc -imagefile <ceos-file>\n");
   printf("          -leaderfile <leader-file>\n");
   printf("          [-job <Job-ID>]\n");
   printf("          [-avgfile <average-in-file>]\n");
   printf("          [-complex_scale_fac <scale>]\n");
   printf("          [-verbose]\n");
   printf("          [-help]\n");
   printf("          [-V]\n");
   printf("          [-X toolkit options ...]\n");
   printf("\n");
}


void
cut_path_off_filename(file_argument) 
   char *file_argument;
{

   char  *filename_without_path;
  
   /* set up filename_without_path, used for metadata */
   filename_without_path = strrchr(file_argument, '/');
  
   if (filename_without_path == NULL)
      /* if there was no slash in the filename... */
      filename_without_path = file_argument;
   else
      /* if there was a slash, point to the first char after the slash */
      filename_without_path++;

   if (filename_without_path[12] == 'R')
      _is_sys_ramp = 1;
   else if (filename_without_path[12] == 'X')
      _is_sys_ramp = 2;
   else
      _is_sys_ramp = 0;
  
   /* now set up metadata values */
   /* These are now taken from the data set summary record */
   /* * there is no need for satellite/revolution/frame strings */
   /* *
   _satellite_string = (char *) malloc(3 * sizeof(char));
   strncpy(_satellite_string, filename_without_path, 2);
   _satellite_string[2] = 0;
  
   _revolution_string = (char *) malloc(6 * sizeof(char));
   strncpy(_revolution_string, filename_without_path + 2, 5);
   _revolution_string[5] = 0;
   * */

   _frame_string = (char *) malloc(4 * sizeof(char));
   strncpy(_frame_string, filename_without_path + 7, 3);
   _frame_string[3] = 0;
}

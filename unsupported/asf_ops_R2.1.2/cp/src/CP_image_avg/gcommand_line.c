#include "ginclude.h"
#include "gdefines.h"     /*  contains all the #define's  */
#include "gfunc_decl.h"   /*  contains all the function declarations  */
#include "gglobal_var.h"  /*  contains all the global variables  */
#include "gversion.h"

static char sccsid_gcommand_line_c[] = "@(#)gcommand_line.c	1.11 97/05/20 19:02:49";

/*  This function parses the given command line, 
    and sets up a few global variables that might
    be altered by command line arguments */

void
parse_command_line(argument_count, argument_vector) 
   int   argument_count;
   char  *argument_vector[];
{
   int i, len;

   /* Initialize -- change later, if needed */
   _verbose_option   = NOT_VERBOSE;
   _colormap_option  = NEW_CMAP;

   _complex_scale_parameter = 0.0;

   _order_id_string = (char *) malloc(13 * sizeof(char));
   sprintf(_order_id_string, "UNKNOWN");

   /* If the command was invoked all by itself, */
   if (argument_count == 1) {
      printfLLog(LOG_ERR, "ERROR:  %s", "Missing name of input image file");
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
      else if (! strcmp(argument_vector[i], "\-outputfile")) {   
      /* -outputfile denotes the output file */
         _output_to_file = (char *) malloc(200 * sizeof(char));
	 sprintf(_output_to_file, "%s", argument_vector[++i]);
      } 
      else if (! strcmp(argument_vector[i], "\-complex_scale_fac")) {
	/* -complex_scale_fac denotes (100.0 / mean) for complex image  */
	_complex_scale_parameter = atof(argument_vector[++i]) ;
      }
      else if (! strcmp(argument_vector[i], "\-V")) {
	printf("%s version %s\n", argument_vector[0], version_id);
      }
      else if (! strcmp(argument_vector[i], "\-help")) {
	print_usage();
	exit(QC_HELP);
      }
      else {
	 char problem[400];
	 sprintf(problem, "unknown option %s\n", argument_vector[i]);
	 printfLLog(LOG_ERR, "ERROR:  %s", problem);
      }
   }
     
   if (! _leader_file) {
      printfLLog(LOG_ERR, "ERROR:  %s", "Missing name of leader file");
      exit(QC_NO_LEADER_FILE);
   }

   if (! _filename) {
      printfLLog(LOG_ERR, "ERROR:  %s", "Missing name of input image file");
      exit(QC_NO_CEOS_FILE);
   }

   if (! _output_to_file) {
      _output_to_file = (char *) malloc(200 * sizeof(char));
      strcpy(_output_to_file, _filename);
      len = strlen(_output_to_file);
      for (i=len; i>0; i--) {
         if (_output_to_file[i] == '\.') {
            _output_to_file[i+1] = 'a';
            _output_to_file[i+2] = 'v';
            _output_to_file[i+3] = 'g';
            _output_to_file[i+4] = '\0';
            break;
         }
      }
      printfLLog(LOG_INFO, "%s",
	  "Creating name of output file from name of input image file");
   }
}


/* This function prints out legal command line options.  */

void
print_usage()
{
   printf("Usage: CP_image_avg -imagefile <ceos-image-file>\n");
   printf("                    -leaderfile <leader-file>\n");
   printf("                    [-outputfile <output-file>]\n");
   printf("                    [-help]\n");
   printf("                    [-V]\n");
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
   /* there is no need for satellite/revolution/frame strings */

   _frame_string = (char *) malloc(4 * sizeof(char));
   strncpy(_frame_string, filename_without_path + 7, 3);
   _frame_string[3] = 0;
}

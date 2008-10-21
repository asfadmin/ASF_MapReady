
#include <asf.h>
#include <asf_meta.h>
#include <cla.h>
#include <asf_contact.h>

// Includes for the stat() function
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>


// Define strings for usage and help info
#define ASF_NAME_STRING \
 "chgndv"

#define ASF_USAGE_STRING \
 "Usage:\n"\
 "    "ASF_NAME_STRING" [-log <file>] [-quiet] [-license] [-version] [-help]\n"\
 "           <infile> <outfile> <oldval>[+|-] <newval>\n"

#define ASF_HELP_STRING \
 "\n"\
 ASF_USAGE_STRING \
 "\n"\
 "Description:\n"\
 "   Creates outfile based on the infile but with oldval\n"\
 "   (the stated old 'no data value') having been replaced by newval\n"\
 "   (the requested new 'no data value').\n"\
 "\n"\
 "Arguments:\n"\
 "   <infile>        Name of the  file to be read. Please use the full file\n"\
 "                   name and not just the base name. (e.g. infile.img)\n"\
 "   <outfile>       File created with the new 'no data value' in place.\n"\
 "                   Please use the full file name and not just the base\n"\
 "                   name. (e.g. outfile.img)\n"\
 "   <oldval>[+|-]   The 'no data value' for the input file. If it is\n"\
 "                   followed by a '+', then that value and anything\n"\
 "                   greater will be interpreted as a no data value.\n"\
 "                   If there is a '-' following the value, then anything\n"\
 "                   less than or equal will be counted as a no data value.\n"\
 "   <newval>        The 'no data value' to be in the output file.\n"\
 "\n"\
 "Options:\n"\
 "   -log <logFile>\n"\
 "        Output will be written to a specified log file.\n"\
 "   -quiet\n"\
 "        Supresses all non-essential output.\n"\
 "   -license\n"\
 "        Print copyright and license for this software then exit.\n"\
 "   -version\n"\
 "        Print version and copyright then exit.\n"\
 "   -help\n"\
 "        Print a help page and exit.\n"\
 "\n"\
 "Contact:\n"\
 ASF_CONTACT_STRING\
 "\n"


// Functions to inform the user about the program
void usage(void)
{
	quietflag=0;
	asfPrintStatus(ASF_USAGE_STRING);
	exit(EXIT_FAILURE);
}

void help(void)
{
	quietflag=0;
	asfPrintStatus(ASF_HELP_STRING);
	exit(EXIT_FAILURE);
}


typedef enum {
	LESS_THAN_OR_EQUAL    = -1,
	EQUAL                 =  0,
	GREATER_THAN_OR_EQUAL =  1
} compare_t;

int ndv_compare(float ndv, float pixel, compare_t comp)
{
	switch (comp) {
		case LESS_THAN_OR_EQUAL:
			return pixel <= ndv;
		case EQUAL:
			return (fabs(ndv-pixel)) < 0.0001;
		case GREATER_THAN_OR_EQUAL:
			return pixel >= ndv;
		default:
			asfPrintError("%s: Invalid comparison.\n",__func__);
	}
}

// True if difference is less than a really small fraction
#define FLOATS_EQUAL(x,y) (fabs((x)-(y))<0.0001)




// The program itself!
int main (int argc, char **argv)
{
	char		*in_fname	= NULL;
	char		*out_fname	= NULL;
	FILE		*in_fp		= NULL;
	FILE		*out_fp		= NULL;
	meta_parameters *in_meta	= NULL;
	meta_parameters *out_meta	= NULL;
	float		*in_buf		= NULL;
	float		*out_buf	= NULL;
	float		newval, oldval;
	int		ii, jj;
	int		itmp;
	compare_t	comp;

	// Search the command line for help & report if requested
	if (extract_flag_options(&argc, &argv, "-h", "-help", "--help", NULL)) {
		help();
	}

	// Grab the common asf options (eg -version, -quiet, etc)
	handle_common_asf_args(&argc, &argv, ASF_NAME_STRING);
	
	// Since handle_common_asf_args reduces argc for all the options
	// we can check argc to see if we've got 3 arguments & the progname
	if (argc != 5) {
		usage();
	}

	// Fetch required arguments
	in_fname = (char*)CALLOC(strlen(argv[1])+2,sizeof(char));
	strcpy(in_fname,argv[1]);
	out_fname = (char*)CALLOC(strlen(argv[2])+2,sizeof(char));
	strcpy(out_fname,argv[2]);
	itmp = strlen(argv[3]) - 1;
	switch (argv[3][itmp]) {
		case '-':
			comp = LESS_THAN_OR_EQUAL;
			argv[3][itmp] = '\0';
			break;
		case '+':
			comp = GREATER_THAN_OR_EQUAL;
			argv[3][itmp] = '\0';
			break;
		default:
			comp = EQUAL;
	}
	oldval = atof(argv[3]);
	newval = atof(argv[4]);

	// Open up the files
	in_fp = FOPEN(in_fname, "rb");
	out_fp = FOPEN(out_fname, "wb");

	// get some info about the file
	in_meta = meta_read(in_fname);
	out_meta = meta_read(in_fname);
	out_meta->general->no_data = newval;
	
	// Allocate the buffers
	in_buf = (float*)CALLOC(in_meta->general->sample_count,sizeof(float));
	out_buf = (float*)CALLOC(out_meta->general->sample_count,sizeof(float));

	// do the work
	asfLineMeter(0,in_meta->general->line_count);
	for (ii=0; ii<in_meta->general->line_count; ++ii) {
		get_float_line(in_fp, in_meta, ii, in_buf);
		for (jj=0; jj<in_meta->general->sample_count; ++jj) {
			if (ndv_compare(oldval,in_buf[jj], comp)) {
				out_buf[jj] = out_meta->general->no_data;
			} else {
				out_buf[jj] = in_buf[jj];
			}
		}
		out_meta->general->line_count = ii + 1;
		put_float_line(out_fp, out_meta, ii, out_buf);
		meta_write(out_meta, out_fname);
		asfLineMeter(ii,in_meta->general->line_count);
	}

	// cleanup and exit
	FREE(in_fname);
	FREE(out_fname);
	FCLOSE(in_fp);
	FCLOSE(out_fp);
	meta_free(in_meta);
	meta_free(out_meta);
	FREE(in_buf);
	FREE(out_buf);
	exit(EXIT_SUCCESS);
}

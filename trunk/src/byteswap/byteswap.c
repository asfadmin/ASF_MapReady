// Byteswap: A program for swapping endian order of a flat binary file

#include <asf.h>
#include <cla.h>
#include <asf_endian.h>
#include <asf_contact.h>

// Includes for the stat() function
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>


// Define strings for usage and help info
#define ASF_NAME_STRING \
 "byteswap"

#define ASF_USAGE_STRING \
 "Usage:\n"\
 "    "ASF_NAME_STRING" [-log <file>] [-quiet] [-license] [-version] [-help]\n"\
 "             <wordsize> <infile> <outfile>\n"

#define ASF_HELP_STRING \
 "\n"\
 ASF_USAGE_STRING \
 "\n"\
 "Description:\n"\
 "   Creates a binary file with its words in the opposite endian order than\n"\
 "   the given source file. If there is an accompanying metadata file, it\n"\
 "   will need to be manually copied for the new byte swapped binary file.\n"\
 "\n"\
 "Input:\n"\
 "   <wordsize>   Size of the 'words' being byte swapped. This is in number\n"\
 "                of bits, not bytes. Choices are 16, 32, and 64.\n"\
 "   <infile>     Name of the  file to be read. Please use the full file\n"\
 "                name and not the base name.\n"\
 "\n"\
 "Output:\n"\
 "   <outfile>    File created with words in the opposite endian order\n"\
 "                than the input file.\n"\
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


// Expected number of bytes for data types
#define SHORT_INT_SIZE	2
#define INT_SIZE	4
#define LL_INT_SIZE	8
#define FLOAT_SIZE	4
#define DOUBLE_SIZE	8
#define MEGABYTE	1048576


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


// The program itself!
int main (int argc, char **argv)
{

	struct stat	 stat_buf;
	size_t		 wordsize=0;
	size_t		 bytesread=0;
	size_t		 byteswritten=0;
	size_t		 nitems=0;
	size_t		 ret=0;
	int		 done=FALSE;
	char		*in_fname=NULL;
	char		*out_fname=NULL;
	FILE		*in_fp=NULL;
	FILE		*out_fp=NULL;
	void		*buf=NULL;
	void		(*byteswap)(unsigned char *);
	
	// Search the command line for help & report if requested
	if (extract_flag_options(&argc, &argv, "-h", "-help", "--help", NULL)) {
		help();
	}

	// Grab the common asf options (eg -version, -quiet, etc)
	handle_common_asf_args(&argc, &argv, ASF_NAME_STRING);
	
	// Since handle_common_asf_args reduces argc for all the options
	// we can check argc to see if we've got 3 arguments & the progname
	if (argc != 4) {
		usage();
	}
/*
	// Make sure data types are of the size we expect
	asfRequire( sizeof(short) == SHORT_INT_SIZE,
	            "Short integers need to be %d bytes (%d-bits),"
		    " they appear to be %d bytes.\n",
		    SHORT_INT_SIZE, SHORT_INT_SIZE*8, sizeof(short));
	asfRequire( sizeof(int) == INT_SIZE,
	            "Regular integers need to be %d bytes (%d-bits),"
		    " they appear to be %d bytes.\n",
		    INT_SIZE, INT_SIZE*8, sizeof(int));
	asfRequire( sizeof(long long) == LL_INT_SIZE,
	            "Long long integers need to be %d bytes (%d-bits),"
		    " they appear to be %d bytes.\n",
		    LL_INT_SIZE, LL_INT_SIZE*8, sizeof(long long));
	asfRequire( sizeof(long long) == FLOAT_SIZE,
	            "Floats need to be %d bytes (%d-bits),"
		    " they appear to be %d bytes.\n",
		    FLOAT_SIZE, FLOAT_SIZE*8, sizeof(long long));
	asfRequire( sizeof(long long) == DOUBLE_SIZE,
	            "Doubles need to be %d bytes (%d-bits),"
		    " they appear to be %d bytes.\n",
		    DOUBLE_SIZE, DOUBLE_SIZE*8, sizeof(long long));
*/
	// Fetch our required arguments
	wordsize = atoi(argv[1]);
	if (  ! (wordsize==16 || wordsize==32 || wordsize==64) ) {
		asfPrintError("Wordsize must be 16, 32, or 64 bits, you gave "
		              "%d... Exiting.\n", wordsize);
	}
	wordsize /= 8;  // change wordsize from bits to bytes
	in_fname = (char*)CALLOC(strlen(argv[2])+3,sizeof(char));
	strcpy(in_fname,argv[2]);
	out_fname = (char*)CALLOC(strlen(argv[3])+3,sizeof(char));
	strcpy(out_fname,argv[3]);

	// Set the byteswap function for the proper number of bytes
	switch (wordsize) {
	  case 2: byteswap = (void(*)(unsigned char*))swap16; break;
	  case 4: byteswap = (void(*)(unsigned char*))swap32; break;
	  case 8: byteswap = (void(*)(unsigned char*))swap64; break;
	  default: asfPrintError("Unrecognized wordsize %d\n",wordsize*8);
	}

	// Open the needed files
	in_fp = FOPEN(in_fname,"rb");
	out_fp = FOPEN(out_fname,"wb");

	// Prepare for the byteswapping loop
	bytesread = byteswritten = ret = 0;
	nitems = MEGABYTE / wordsize;
	buf = CALLOC(MEGABYTE,sizeof(char));
	done = FALSE;

	// Actual byteswapping routine
	asfPrintStatus("Swapping bytes (each '.' represents one megabyte)\n");
	while (!done) {
		int ii;
		if (nitems != (ret=fread(buf, wordsize, nitems, in_fp))) {
			if (feof(in_fp)) {
				done=TRUE;
			}
		}
		bytesread += ret*wordsize;
		for (ii=0; ii<nitems; ii++) {
			byteswap((unsigned char *)(buf+ii*wordsize));
		}
		ret = fwrite(buf, wordsize, ret, out_fp);
		byteswritten += ret*wordsize;
		asfPrintStatus(".");
	}

	// Make sure things look right before quitting
	stat(in_fname, &stat_buf);
	asfRequire((long long)bytesread==(long long)stat_buf.st_size,
	           "(%lld==%lld)\n"
		   "Problem likely occurred during file reading.\n",
		   (long long)bytesread, (long long)stat_buf.st_size);
	asfRequire((long long)bytesread==(long long)byteswritten,
	           "(%lld==%lld)\n"
	           "Problem likely occurred during file writing.\n",
		   (long long)bytesread, (long long)byteswritten);
	
	asfPrintStatus("\n"
	               "  Successfully wrote file '%s'\n"
	               "  in reverse endian order than '%s'.\n",
	               out_fname, in_fname);

	// Yay, success!
	exit(EXIT_SUCCESS);
}

/*******************************************************************************
<documentation>
<name>
   asf_convert
</name>

<synopsis>
asf_convert [-format <output_format>] <in_data> <in_meta> <out_full>
</synopsis>

<description>
   This program ingests level one CEOS data, geocodes and resamples them
   (both optional) and exports them to a variety of output formats. It can
   handle input from a configuration file as well as directly from the command
   line. asf_convert is able to run in batch mode. Running the program with a
   configuration file allows for a more detailed processing flow, including
   resampling and geocoding as intermediate processing steps.
</description>

<input>
   The basic configuration file is defined as follows:

   asf_convert configuration file

   [General]
   input data file = < name of input data file (with extension) >
   input metadata file = < name of input metadata file (with extension) >
   input format = < CEOS | ASF >
   data type = < amplitude | power | sigma | gamma | beta >
   output file = < basename of the output file >
   output format = < ASF | GEOTIFF | JPEG | ENVI | ESRI | PPM >
   resampling = < flag for subsampling: 0 | 1 >
   browse image = < flag for subsampling to browse image size: 0 | 1 >
   geocoding = < flag for geocoding: 0 | 1 >
   batch mode = < flag for batch mode processing: 0 | 1 >
   batch file = < batch file name >
   log file = < log file name >

   For command line input see OPTIONS.
</input>

<output>
   For configuration file output see INPUT.
   For command line output see OPTIONS.
</output>

<options>
   -config        runs asf_convert from a configuration file.
   -init_config   creates only a configuration file and exits.
   -input         input format and input data and metadata files.
   -output        output format and output filename.
</options>

<examples>
   To convert the CEOS format file1 to a jpeg format in file2.jpg
      asf_convert -input CEOS file1.img file1.meta -output jpeg file2.jpg
   To run the program from a configuration file:
      asf_convert -config configFile
</examples>

<limitations>
   None known.
</limitations>

<see_also>
   asf_import, asf_export
</see_also>

<copyright>
*******************************************************************************
*                                                                             *
* Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks   *
* All rights reserved.                                                        *
*                                                                             *
* Redistribution and use in source and binary forms, with or without          *
* modification, are permitted provided that the following conditions are met: *
*                                                                             *
*    * Redistributions of source code must retain the above copyright notice, *
*      this list of conditions and the following disclaimer.                  *
*    * Redistributions in binary form must reproduce the above copyright      *
*      notice, this list of conditions and the following disclaimer in the    *
*      documentation and/or other materials provided with the distribution.   *
*    * Neither the name of the Geophysical Institute nor the names of its     *
*      contributors may be used to endorse or promote products derived from   *
*      this software without specific prior written permission.               *
*                                                                             *
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" *
* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE   *
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE  *
* ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE    *
* LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR         *
* CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF        *
* SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS    *
* INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     *
* CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)     *
* ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE  *
* POSSIBILITY OF SUCH DAMAGE.                                                 *
*                                                                             *
*       For more information contact us at:                                   *
*                                                                             *
*       Alaska Satellite Facility                                             *
*       Geophysical Institute                   http://www.asf.alaska.edu     *
*       University of Alaska Fairbanks          uso@asf.alaska.edu            *
*       P.O. Box 757320                                                       *
*       Fairbanks, AK 99775-7320                                              *
*                                                                             *
*******************************************************************************
</copyright>
</documentation>

PROGRAM HISTORY:

    VERS:   DATE:   AUTHOR:   PURPOSE:
    ----------------------------------------------------------------------
    0.1     10/02   R. Gens   Original development
    0.2     10/03   R. Gens   Extended output formats
    0.3     05/04   R. Gens   Overhaul for new metadata, added resampling
    0.4     05/04   R. Gens   Added batch mode
    0.5     06/04   R. Gens   Renamed to asf_convert, added alternative
                              command line

*******************************************************************************/

#include "asf.h"
#include "ceos.h"
#include "asf_meta.h"
#include "asf_convert.h"
#include "functions.h"
#include "proj.h"
#include <unistd.h>


#define VERSION 0.5
#define REQUIRED_ARGS 1

void usage()
{
	printf("USAGE:\n"
		"   asf_convert [-format <output_format>] <in_data_name> <in_meta_name> <out_full_name>\n");
	exit(EXIT_FAILURE);
}


void config_usage()
{
	printf(
		"USAGE:\n"
		"   asf_convert -config <config_file> |\n"
		"               -init_config <config_file>\n");
}


void check_return(int ret, char *msg)
{
	if (ret != 0)
	{
		sprintf(errbuf, "\n   \033[31;1mERROR:\033[0m %s\n\n", msg);//I made "ERROR:" red...Yay! :D
		printErr(errbuf);
	}
}

//Check to see if an option was supplied or not
//If it was found, return its argument number
//Otherwise, return -1
int checkForOption(char* key, int argc, char* argv[])
{
	int ii = 0;
	while(ii < argc)
	{
		if(strmatch(key, argv[ii]))
			return(ii);
		++ii;
	}
	return(-1);
}


int main(int argc, char *argv[])
{
	FILE *fBatch;
	s_config *cfg;
	char configFile[255], cmd[255], options[255], in[25], out[25];
	char proj[25], line[255], fileName[255], batchConfig[255];
	char format_in[255], format_out[255];
	char out_file[255], data_file[255], meta_file[255];
//	int inFlag = FALSE, outFlag = FALSE, formatFlag = FALSE, configFlag = FALSE, configInitFlag = FALSE;
	int formatFlag, configFlag, configInitFlag, quietFlag;
	int ii;
	file_format_t type_in, type_out;

//*********************BEGIN COMMAND LINE PARSING STUFF*********************//
	//Super-secret hidden options :)
	if(checkForOption("-batch", argc, argv) != -1)
	{
		config_usage();
		exit(0);
	}

	//Normal options
	formatFlag = checkForOption("-format", argc, argv);
	configFlag = checkForOption("-config", argc, argv);
	configInitFlag = checkForOption("-init_config", argc, argv);
	logflag = checkForOption("-log", argc, argv);
	quietFlag = checkForOption("-quiet", argc, argv);


	if(configFlag != -1 && configInitFlag != -1)//One or the other is fine, but not both
		usage();//This exits with a failure
	if((configFlag != -1 || configInitFlag != -1) && formatFlag)//format is mutually exclusive with these
		usage();//This exits with a failure

	//So, at this point, we know our options don't conflict...now we need
	//to know how many arguments we ought to have.
	int neededArgs = 1;//command
	if(configFlag != -1 || configInitFlag != -1)
		neededArgs += 2;//option & parameter
	else
		neededArgs += 3;//in_data_file, in_meta_file, out_file
	if(logflag != -1)
		neededArgs += 2;//option & parameter
	if(formatFlag != -1)
		neededArgs += 2;//option & parameter
	if(quietFlag != -1)
		neededArgs += 1;//option

	//make sure we've got enough arguments
	if(argc < neededArgs)
		usage();//This exits with a failure

	//We also need to make sure the last three options are close to what we expect
	if(argv[argc - 1][0] == '-' || argv[argc - 2][0] == '-' || argv[argc - 3][0] == '-')
		usage();//This exits with a failure

	//Next we're going to make sure the options are specified according to the usage
	//That is, -option <parameter> -option <parameter> and so on...if an option requires
	//a parameter, we need to make sure it's followed by a parameter!
	if(logflag != -1)
		if(argv[logflag + 1][0] == '-')
			usage();//This exits with a failure
	if(formatFlag != -1)
		if(argv[formatFlag + 1][0] == '-')
			usage();//This exits with a failure

	//We must be good enough at this point...start processing with assumptions that are
	//*supposedly* guaranteed from above :)
	if(formatFlag != -1)
		strcpy(format_out, argv[formatFlag + 1]);
	else
		strcpy(format_out, "geotiff");//default behavior is geotiff
		
	if(logflag != -1)
		strcpy(logFile, argv[logflag + 1]);
	else
		sprintf(logFile, "tmp%i.log", (int)getpid());//default behavior: log to tmp<pid>.log
		
	if(configFlag != -1)
		strcpy(configFile, argv[configFlag + 1]);
	else
		sprintf(configFile, ".tmp%i.config", (int)getpid());//default behavior: config to tmp<pid>.config
		
	if(configInitFlag != -1)
		strcpy(configFile, argv[configInitFlag + 1]);

	if(configFlag == -1 && configInitFlag == -1)
	{
		strcpy(data_file, argv[argc - 3]);
		strcpy(meta_file, argv[argc - 2]);
		strcpy(out_file, argv[argc - 1]);
		//WARNING: THIS IS A TOTAL HACK :D
		//Find the end of the string
		for(ii = 0; ii < 255; ++ii)
			if(argv[argc - 2][ii] == '\0')
				break;
		if(argv[argc - 2][ii - 1] == 'L')//If it ends in .L, it's a CEOS image
			strcpy(format_in, "CEOS");
		else if(strcmp("meta", argv[argc-2][ii - 4]))
			strcpy(format_in, "ASF");
		else
			check_return(-1, "Unrecognized input file type\n");
	}
//**********************END COMMAND LINE PARSING STUFF**********************//

	if(quietFlag == -1)//If we're not quiet
	{
		sprintf(logbuf, "\nCommand line: ");
		for (ii = 0; ii < argc; ii++)
		{
			sprintf(cmd, " %s",argv[ii]);
			strcat(logbuf, cmd);
		}
		sprintf(cmd, "\nDate: %s\nProgram: asf_convert\nPid: %i\n", date_time_stamp(), (int)getpid());
		strcat(logbuf, cmd);
		printf(logbuf);
	}

	
	/* If we're working from a config file, read configuration file */
	if (configFlag != -1)//configFlag has been set
	{
		if (!fileExists(configFile))//Does the specific config file already exist? If not...
		{
			check_return(init_config(configFile),
			"basic configuration file could not be initialized");
			exit(0);
    	}
		else if (check_resample_flag(configFile) && configInitFlag != -1)//config init has been set
		{
			check_return(init_resample_config(configFile),
				"extended resampling configuration file "
				"could not be initialized");
			exit(0);
		}
		else if (check_geocode_flag(configFile) && configInitFlag != -1)//config init has been set
		{
			check_return(init_projection_config(configFile),
				"extended geocoding configuration file "
				"could not be initialized");
			exit(0);
		}
		//If everything else succeeded, do this...
		cfg = read_config(configFile);
		strcpy(format_in, uc(cfg->general->in_format));
		strcpy(format_out, uc(cfg->general->out_format));
		if (strcmp(cfg->general->logFile, "") != 0)
			strcpy(logFile, cfg->general->logFile);
		fLog = FOPEN(logFile, "a");
		printLog(logbuf);
	}

	/* Get input from command line arguments */
	else
	{
		cfg = init_cfg();
		strcpy(cfg->comment, "asf_convert temporary configuration file");
		strcpy(cfg->general->in_data_name, data_file);
		strcpy(cfg->general->in_meta_name, meta_file);
		strcpy(cfg->general->in_format, uc(format_in));
		strcpy(cfg->general->data_type, "amplitude");
		strcpy(cfg->general->out_name, out_file);
		strcpy(cfg->general->out_format, uc(format_out));
		cfg->general->resample = 0;
		cfg->general->browse = 0;
		cfg->general->batch = 0;
		strcpy(cfg->general->batchFile, "");
		strcpy(cfg->general->logFile, logFile);
		check_return(write_config(configFile, cfg),
			"writing a temporary configuration file");
	}

	/* Handle data types */
	//Input types:
	if (strncmp(uc(format_in),"CEOS",4) == 0) type_in = CEOS;
	else if (strncmp(uc(format_in),"ASF",3) == 0) type_in = ASF;
	//Output types:
	if (strncmp(uc(format_out),"ASF",3) == 0) type_out = ASF;
	else if (strncmp(uc(format_out),"CEOS",4) == 0) type_out = CEOS;
	else if (strncmp(uc(format_out),"GEOTIFF",3) == 0) type_out = GEOTIFF;
	else if (strncmp(uc(format_out),"JPEG",3) == 0) type_out = JPEG;
	else if (strncmp(uc(format_out),"ENVI",3) == 0) type_out = ENVI;
	else if (strncmp(uc(format_out),"ESRI",3) == 0) type_out = ESRI;
	else if (strncmp(uc(format_out),"PPM",3) == 0) type_out = PPM;
	else if (strncmp(uc(format_out),"PNG",3) == 0) type_out = PNG;
	else if (strncmp(uc(format_out),"LAS",3) == 0) type_out = LAS;

	/* Batch mode processing */
	if (cfg->general->batch != 0)
	{
//		cfg->general->batch = 0;
//		logflag = 0;
		fBatch = FOPEN(cfg->general->batchFile, "r");
		while (fgets(line, 255, fBatch) != NULL)
		{
			sscanf(line, "%s", fileName);
			sprintf(cfg->general->in_data_name, "%s.D", fileName);
			sprintf(cfg->general->in_meta_name, "%s.L", fileName);
			strcpy(cfg->general->out_name, fileName);
			sprintf(batchConfig, "%s.config", fileName);
			check_return(write_config(batchConfig, cfg),
				"Could not write individual configuration file "
				"for batch mode processing");
			check_return(asf_convert(batchConfig),
				"Processing image in batch mode (asf_convert)");
		}
	}
	else
	{

		/* Prepare processing */
		if (configFlag == -1)
		{
			if (!fileExists(cfg->general->in_data_name))
				check_return(1, "input data file does not exist");
			if (!fileExists(cfg->general->in_meta_name))
				check_return(1, "input metadata file does not exist");
		}

		/* Ingest CEOS image */
		switch (type_in)
		{
			case CEOS:
				sprintf(in, ".tmp%i", (int)getpid());
				if (strncmp(cfg->general->data_type, "amplitude", 9) == 0)
				{
					check_return(asf_import(cfg->general->in_data_name,
						cfg->general->in_meta_name, "-amplitude", in),
						"Importing CEOS data (asf_import)");
					sprintf(out, ".tmp%i_amp", (int)getpid());
				}
				else if (strncmp(cfg->general->data_type, "power", 5) == 0)
				{
					check_return(asf_import(cfg->general->in_data_name,
						cfg->general->in_meta_name, "-power", in),
						"Importing CEOS data (asf_import)");
					sprintf(out, ".tmp%i_power", (int)getpid());
				}
				else if (strncmp(cfg->general->data_type, "sigma", 5) == 0)
				{
					check_return(asf_import(cfg->general->in_data_name,
						cfg->general->in_meta_name, "-sigma", in),
						"Importing CEOS data (asf_import)");
					sprintf(out, ".tmp%i_sigma", (int)getpid());
				}
				else if (strncmp(cfg->general->data_type, "gamma", 5) == 0)
				{
					check_return(asf_import(cfg->general->in_data_name,
						cfg->general->in_meta_name, "-gamma", in),
						"Importing CEOS data (asf_import)");
					sprintf(out, ".tmp%i_gamma", (int)getpid());
				}
				else if (strncmp(cfg->general->data_type, "beta", 4) == 0)
				{
					check_return(asf_import(cfg->general->in_data_name,
						cfg->general->in_meta_name, "-beta", in),
						"Importing CEOS data (asf_import)");
					sprintf(out, ".tmp%i_beta", (int)getpid());
				}
				sprintf(cmd, "cp %s.meta %s.meta", out, cfg->general->out_name);
				system(cmd);
				break;
			case ASF:
				strcpy(out, cfg->general->in_data_name);
				break;
			default: check_return(1, "Unsupported input format type!");
				break;
		}

		/* Determine the corner coordinates of the image */
		if (cfg->general->browse)
		{
			check_return(corner_coords(out),
				"determining geographic coordinates of corner points "
				"(corner_coords)");
			sprintf(cmd, "mv %s.corners %s.corners", out, cfg->general->out_name);
			system(cmd);
		}

		/* Resampling */
		if (cfg->general->resample || cfg->general->browse)
		{
			sprintf(in, "%s", out);
			sprintf(out, ".tmp%i_small", (int)getpid());
			if (cfg->general->browse)
				sprintf(options, "-browse");
			else
				sprintf(options, "-resample %d", cfg->resampling->kernel);
			check_return(filter(options, in, out),
				"subsampling image (filter)");
		}

		/* Geocoding */
		if (cfg->general->geocoding) {

		/* Creating projection parameter file */
		sprintf(proj, ".tmp%i.proj", (int)getpid());

		/*** Polar Stereographic ***/
		if (strncmp(uc(cfg->geocoding->projection), "POLAR", 5) == 0)
		{
			sprintf(options, "-l %lf -p %lf -g %s -d %d",
				cfg->polar->center_lon, cfg->polar->center_lat,
				cfg->polar->units, cfg->polar->datum);
			check_return(projprm("plstereo", "key", proj, options),
				"generating projection parameter file (projprm)");
		}
		/*** Universal Transverse Mercator ***/
		if (strncmp(uc(cfg->geocoding->projection), "UTM", 3) == 0)
		{
			sprintf(options, "-d %i -z %i", cfg->utm->datum, cfg->utm->zone);
			check_return(projprm("utm", "key", proj, options),
				"generating projection parameter file (projprm)");
		}

		/*** Albers Conic Equal Area ***/
		if (strncmp(uc(cfg->geocoding->projection), "ALBERS", 6) == 0)
		{
			sprintf(options, "-a %lf -b %lf -c %lf -o %lf -g %s -d %d",
				cfg->albers->first_parallel, cfg->albers->second_parallel,
				cfg->albers->center_meridian, cfg->albers->orig_latitude,
				cfg->albers->units, cfg->albers->datum);
			check_return(projprm("albers", "key", proj, options),
				"generating projection parameter file (projprm)");
		}

		/*** Lambert Conformal Conic ***
		if (strncmp(uc(cfg->geocoding->projection), "LAMBERT_CC", ) == 0)
		{
			sprintf(options, "-x %lf -y %lf -g %s -d %d",
				cfg->lambert1->latitude, cfg->lambert1->longitude,
				cfg->lambert1->units, cfg->lambert1->datum);
			check_return(projprm("lambert", "key", proj, options),
				"generating projection parameter file (projprm)");
		} ***/

		/*** Lambert Azimuthal Equal Area ***
		if (strncmp(uc(cfg->geocoding->projection), "LAMBERT2", 8) == 0)
		{
		*** still needs to be figured out ***
		} ***/

		sprintf(in, "%s", out);
		sprintf(out, ".tmp%i_geo", (int)getpid());
		check_return(geocode(in, proj, "key", cfg->geocoding->pixel,
			cfg->geocoding->height, out),
			"geocoding image (geocode)");
		}

		/* Exporting image */
		switch (type_out)
		{
			case CEOS:
				break;
			case ASF:
				sprintf(cmd, "mv %s.img %s.img", out, cfg->general->out_name);
				system(cmd);
				sprintf(cmd, "mv %s.meta %s.meta", out, cfg->general->out_name);
				break;
			case GEOTIFF:
				check_return(asf_export(type_out, out, cfg->general->out_name),
					"exporting image to GEOTIFF format (asf_export)");
				break;
			case JPEG:
				check_return(asf_export(type_out, out, cfg->general->out_name),
					"exporting image to JPEG format (asf_export)");
				break;
			case ENVI:
				check_return(asf_export(type_out, out, cfg->general->out_name),
					"exporting image to ENVI format (asf_export)");
				break;
			case ESRI:
				check_return(asf_export(type_out, out, cfg->general->out_name),
					"exporting image to ESRI format (asf_export)");
				break;
			case PPM:
				check_return(asf_export(type_out, out, cfg->general->out_name),
					"exporting image to PPM format (asf_export)");
				break;
			case PNG:
				/*
				check_return(asf_export(type_out, out, cfg->general->out_name),
					"exporting image to PNG format (asf_export)");
				*/
				break;
			case LAS: break;
			default:
				check_return(1, "Unsupported output format!");
				break;
		}
	}

	/* Remove temporary files */
	sprintf(cmd, "rm -f .tmp%i*", (int)getpid());
	system(cmd);
	return(0);
}





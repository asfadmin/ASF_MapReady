#!/bin/csh -f
#
#===============================================================================
# Copyright(c) 1996, California Institute of Technology.
#             ALL RIGHTS RESERVED.
# U.S. Government Sponsorship acknowledged.
#===============================================================================
# Filename:	PPS_startserver.csh
# Description:	PPS server start-up script	
# Creator:	Thuy Tran	
# Notes:	
# 
#            Usage:  PPS_startserver.csh [-d] [runlog_file] [-c config_file]
#
#   If -d option is specified, the pps_server_debug executable will be run which 
#   runs the server in debug mode.  The pps_server_debug prints to stdout all 
#   ODL messages received and sent by the PPS Server, as well as other miscellaneous 
#   informational messages used for debugging purposes. If a file name is 
#   provided with the -d option, stdout and stderr are redirected to this file; 
#   otherwise, stdout and stderr are redirected to $LOG which is currently set 
#   to /LOCAL/pps/logs/PPSrun.log by this script.
#
#   If -d option is not specified, the pps_server executable will be run.  Stdout
#   and stderr will be output to the console.
#
#   If -c option is specified, the argument for this option which is required
#   specifies the configuration file to use.  
#
#   If -c option is not specified, the default configuration file 
#   $LOCAL/pps/config/pps.config will be used.
# 
# SCCS Info:
#               @(#)PPS_startserver.csh	1.4  06/11/97
#===============================================================================


set SERVER = pps_server
set LOG    = "/LOCAL/pps/logs/PPSrun.log"
set CONFIG = 
set print_usage = 0

# Get the input arguments
#
while ($#argv)
        switch ($argv[1])
        case -[dD]:
		set SERVER = pps_server_debug
		echo "Running PPS Server in debug mode" 
                if ($#argv > 1) then
                        shift
                        if ("$argv[1]" != "-c" && "$argv[1]" != "-C") then
                                set LOG = "$argv[1]"
				echo "PPS Server run log is $LOG"
				shift
                        endif
		else
			echo "PPS Server run log is $LOG"
			break	
		endif
                breaksw

        case -[cC]:
                if ($#argv > 1) then
                        shift
                        if ("$argv[1]" != "-d" && "$argv[1]" != "-D") then
				if (! -e "$argv[1]") then
					echo "Configuration file $argv[1] not found"
					exit
				endif
                                set CONFIG = "$argv[1]"
				echo "Using configuration file $argv[1]"
                                shift
                        else
                                echo "Configuration file not specified"
				set print_usage = 1
                               	break 
                        endif
                else
                        echo "Configuration file not specified"
			set print_usage = 1
                       	break 
                endif
                breaksw
        default:
                echo "Unrecognized argument $argv[1]"
		set print_usage = 1 
               	break 
        endsw
end

if ($print_usage == '1') then
	echo "        Usage:  PPS_startserver.csh [-d] [runlog_file] [-c config_file]"
	exit
endif

#DCE entry must be defined.
if ($?RPC_DEFAULT_ENTRY) then
        echo RPC_DEFAULT_ENTRY is $RPC_DEFAULT_ENTRY
else
        echo RPC_DEFAULT_ENTRY not set, Bye.
	exit
endif

# ASF must be set first
if ($?ASF) then
	echo ASF = $ASF
else
	echo ASF not set, exiting...
	exit 1
endif

# set common PPS environment variables
source $ASF/bin/PPS_env.csh

echo "Check if PPS Server is listening..."
# check if server is already listening
set num = `PPS_pingserver.csh |& grep -c 'is listening'`
if ($num > 0) then
	echo "PPS server is listening."
        echo "Will not start another PPS Server."
	exit
else
	echo "PPS server is not listening."
endif

# Construct the command to start the server
set cmd = $ASF/bin/$SERVER
if ($CONFIG != "") then
	set cmd = "$cmd -c $CONFIG"
endif

if ("$SERVER" == "pps_server_debug") then
	set cmd = "$cmd |& $ASF/bin/line_at_a_time.sh $LOG"
endif
	
# Run the server 
echo $cmd
/bin/csh -f -c "$cmd"

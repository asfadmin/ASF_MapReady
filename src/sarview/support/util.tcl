# util.tcl
#
# TCL/TK Utility routines for the SARview application.
#
# Orion Sky Lawlor, 5/7/99
#

################## getLogName ###################
# getLogName asks the user for the name of a log file,
# and returns it, or "" if they cancel.
proc getLogName {} {
	set types {
	    {{TEXT Log File}   {.txt}        }
	    {{TEXT Log File}   {}        TEXT}
	    {{All Files}        *            }
	}
	return [tk_getSaveFile -defaultextension ".txt" -filetypes $types ]
}


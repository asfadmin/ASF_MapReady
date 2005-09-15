# popup window.tcl
#
# This is a popup window of the SARview application.
#  It displays information about the selected pixel.
#
# Orion Sky Lawlor, 5/7/99
#

proc infoShift {dx dy} {
	global link_info_x link_info_y
	viewPixelInfo [expr $link_info_x + $dx] [expr $link_info_y + $dy]
}

################## viewPixelInfo ###################
set pop_info_logfile ""

proc viewPixelInfo {x y} {
	global link_info_x link_info_y
	set w .pop_info
# Get the point information
	set pt_info [cproc_pointinfo $x $y]
	set link_info_x $x
	set link_info_y $y
	
# Log it if asked
	global pop_info_logfile
	cproc_log $pop_info_logfile $pt_info

#Display info to the screen-- check to see if window already exists
	set exists [winfo exists $w]
	if {$exists==1} {
# Window already exists-- just update information
		$w.text delete 0.0 end
		$w.text insert 0.0 $pt_info
	} else {
# We have to create the entire window from scratch
		catch "destroy $w"
		toplevel $w
		wm title $w "Location Information"
		wm iconname $w "Location"
		wm group $w .
		bind $w <Destroy> {adornCanvases}

# Set up "northfinder" canvas
		canvas $w.arrows -width 100 -height 100 \
			-scrollregion  "-50 -50 50 50"
		pack $w.arrows -side left

# Set up buttons-- OK, Log, and log file name display
		frame $w.frame

		button $w.frame.dismiss -text "OK" -command "destroy $w"
		pack $w.frame.dismiss -side right

		button $w.frame.map -text "Map" -command {
			global link_info_x link_info_y
			viewPixelMap $link_info_x $link_info_y
		}
		pack $w.frame.map -side left
		
		button $w.frame.log -text "Log..." -command {
			set pop_info_logfile [getLogName]
			# Update the logname label with the new file name
			.pop_info.frame.logname configure -text $pop_info_logfile
		}
		pack $w.frame.log -side left

		label $w.frame.logname -text $pop_info_logfile
		pack $w.frame.logname -side left
		
		pack $w.frame -side bottom -fill x -padx 20 -pady 4
	
# Set up text field
		text $w.text -wrap word -height 6 -width 50
		$w.text insert 0.0 $pt_info
		pack $w.text -fill both -expand yes
	}
	
#Update the direction-finder arrows
	cproc_ne_arrows $w.arrows $x $y
	
#Update the little info cross
	adornCanvases
}

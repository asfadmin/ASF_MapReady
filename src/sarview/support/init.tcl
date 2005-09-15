# init.tcl
#
# This is the initialization portion of SARview.
#
# Orion Sky Lawlor, 5/6/99
#

# Run Program
set programResult [catch {
# Initialize variables
	# (set from C) set link_path "support"
	# (set from C) set link_imagewidth 489
	# (set from C) set link_imageheight 389
	
	set link_screenx [winfo screenwidth .]
	set link_screeny [winfo screenheight .]
	set link_zoom 1.0
	set link_blowup_x -1
	set link_blowup_y -1
	set link_blowup_size 300
	set link_blowup_zoom 1
	
# Create the main photo images
	image create photo img_main
	image create photo img_blowup
	image create photo img_bcpreview
	image create photo img_hist
	
# Create and name the main window
	wm title . "SARview Application"
	wm iconname . "SARview"
	
# Start all the "support/*.tcl" scripts that make up the user interface
	source [file join $link_path util.tcl]
	source [file join $link_path menu.tcl]
	source [file join $link_path canvas.tcl]
	source [file join $link_path tools.tcl]
	source [file join $link_path mainwin.tcl]
	source [file join $link_path events.tcl]
	source [file join $link_path cursor.tcl]
	source [file join $link_path p_about.tcl]
	source [file join $link_path p_image.tcl]
	source [file join $link_path p_sel.tcl]
	source [file join $link_path p_blow.tcl]
	source [file join $link_path p_info.tcl]
	source [file join $link_path p_bc.tcl]
	source [file join $link_path p_hist.tcl]
	source [file join $link_path f_map.tcl]
	source [file join $link_path f_poly.tcl]

#	doOpenFile "D:\\Code\\tk\\test\\flt_ramp.img"
#	doOpenFile "D:\\Code\\tk\\las_image.img"
#	doOpenFile "D:\\ASF\\CEOS\\E105791160G4S100.D"
}]

# Check for abnormal termination
if {$programResult!=0} {
	set code $errorCode
	set info $errorInfo
	set errtext "Sorry, a TCL error occured in SARview:\n"
	append errtext "Code: " $code "\n"
	append errtext "Info: " $info "\n"
	append errtext {
For support information, contact 
rguritz@images.alaska.edu or visit the
Alaska SAR Facility STEP Lab web site at
http://www.images.alaska.edu/
}
	tk_dialog .err_dialog "SARview TCL Error!" $errtext {} 0 "OK"
	exit 1
}

# popup window.tcl
#
# This is a popup window of the SARview application.
#
# Orion Sky Lawlor, 5/6/99
#

################## doAbout ###################
proc doAbout {} {
	set w .pop_about
	catch {destroy $w}
	toplevel $w
	wm title $w "About SARview"
	wm iconname $w "About"
	
	button $w.dismiss -text "OK" -command "destroy $w"
	pack $w.dismiss -side bottom 
	
	label $w.top -font "Times 24 bold" -text "About SARview" 
	pack $w.top -side top
	
	label $w.middle -justify left -text {
    SARview is a Tcl/Tk and C program used to view
LAS 6.0 and CEOS Synthetic Aperture Radar satellite
images.  It was developed in 1999 at the Alaska
SAR Facility's STEP Lab by Orion Lawlor.

Visit the STEP web site at
      http://www.images.alaska.edu/

or contact Rick Guritz at
      rguritz@images.alaska.edu
}
	pack $w.middle -side bottom
}


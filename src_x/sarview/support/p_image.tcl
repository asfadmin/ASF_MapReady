# popup window.tcl
#
# This is a popup window of the SARview application.
#  It displays information about the entire image.
#
# Orion Sky Lawlor, 5/7/99
#

################## viewImageInfo ###################
proc viewImageInfo {} {
	set w .pop_image
	catch {destroy $w}
	toplevel $w
	wm title $w "Image Info"
	wm iconname $w "Image Info"
	
	button $w.dismiss -text "OK" -command "destroy $w"
	pack $w.dismiss -side bottom 
	
	label $w.top -font "Times 24 bold" -text "Image Information" 
	pack $w.top -side top
	
	text $w.text -wrap word 
	$w.text insert 0.0 [cproc_imageinfo]
	pack $w.text
}

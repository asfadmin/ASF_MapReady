# popup window.tcl
#
# This is a popup window of the SARview application.
#  It displays information about the selected region.
#
# Orion Sky Lawlor, 5/7/99
#

################## viewImageInfo ###################
proc viewSelectionInfo {} {
	set w .pop_image
	catch {destroy $w}
	toplevel $w
	wm title $w "Selection Info"
	wm iconname $w "Selection Info"
	
	button $w.dismiss -text "OK" -command "destroy $w"
	pack $w.dismiss -side bottom 
	
	label $w.top -font "Times 24 bold" -text "Selection Information" 
	pack $w.top -side top
	
	set t $w.text
	scrollbar $w.vscroll -command "$t yview"
	pack $w.vscroll -side right -fill y
	
	text $w.text -wrap word -yscrollcommand "$w.vscroll set"
	$w.text insert 0.0 [cproc_polyinfo]
	pack $w.text -expand yes -fill both
}

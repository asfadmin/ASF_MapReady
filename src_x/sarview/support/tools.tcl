# tools.tcl
#
# This is the toolbar portion of SARview.
#
# Orion Sky Lawlor, 5/6/99
#

set link_poly_x ""
set link_poly_y ""

################## Define Toolbar "changeTool" command ############
proc changeTool toWhat {
#DBUG	tk_dialog .annoy ToolChange "Tool changed to $toWhat" {} 0 OK
	global last_tool
	global link_tool link_poly_x link_poly_y
	set link_tool $toWhat
# Re-set polygon elements to empty, unless zoom or info tool was selected
	set isBenign [lsearch {zoom info} $toWhat]
	set wasBenign [lsearch {zoom info} $last_tool]
	if {[expr (-1==$isBenign)&&(-1==$wasBenign)]} {
	#Eliminate the user's selected polygon/rectangle
		set link_poly_x ""
		set link_poly_y ""
	}
	adornCanvases
	set last_tool $toWhat
}

################## Create Toolbar by reading "images/tools.gif" ############
set t .toolbar
frame $t
image create photo img_toolset \
	-file [file join $link_path "images/tools.gif" ]

set link_tool "info"
set tstart 0
foreach tool {zoom info rect poly} {
# Tool icons are 16x16 pixel images
	image create photo img_tool_$tool -width 16 -height 16
	img_tool_$tool copy img_toolset \
		-from 0 [expr 16*$tstart] 15 [expr 16*($tstart+1)-1]
	incr tstart
	set command changeTool
	append command " " $tool
	radiobutton $t.$tool -variable link_tool \
		-value $tool -indicatoron false \
		-image img_tool_$tool -command $command
	pack $t.$tool -side top
}

pack $t -side left -fill y
set last_tool "info"
changeTool "info"

# Menu bar section

################ Utilities ###############
set currMenu 0
menu .menubar
# I'm using TCL-8.0 style menus, so I can attach .menubar to .
. configure -menu .menubar

if {$tcl_platform(platform) == "macintosh"} {
    set modifier Command; set mod_user Command
} elseif {$tcl_platform(platform) == "windows"} {
    set modifier Control; set mod_user Ctrl
} else {
    set modifier Meta;set mod_user Meta
}

proc makeMenu {name title under} {
	global currMenu 
	set currMenu .menubar.$name
	.menubar add cascade -label $title -menu $currMenu -underline $under
	menu $currMenu -tearoff 0
}

proc makeCommand {command label letter underlineNo} {
	global currMenu modifier mod_user
	$currMenu add command -label $label -command $command \
		-accelerator $mod_user+[string toupper $letter] \
		-underline $underlineNo
	bind . <$modifier-$letter> $command
}

############## File Menu (always there) ###############
makeMenu file "File" 0

makeCommand {doOpen} "Open..." o 0
makeCommand {doExport} "Export..." e 0
$currMenu add separator
makeCommand {doAbout} "About SARview" a 0
$currMenu add separator
if {$tcl_platform(platform) == "macintosh"} {
	makeCommand {doQuit} "Quit" q 0
} else {makeCommand {doQuit} "Exit" x 1}

#### When the first image is loaded, create the other menus
set menu_beenUpdated 0
proc updateMenus {} {
#This is called by "fileOpen"
	global menu_beenUpdated currMenu
	if {$menu_beenUpdated!=0} return
	set menu_beenUpdated 1
	############### View Menu ###############
	makeMenu view "View" 0
	
	makeCommand {busy "changeZoom sqrt(0.5)"} "Zoom In" a 5
	makeCommand {busy "changeZoom [expr 1.0/sqrt(0.5)]"} "Zoom Out" s 5
	$currMenu add separator
	makeCommand {viewSelectionInfo} "Selection Info" l 2
	makeCommand {viewImageInfo} "Image Info" n 7
	
	# Set the drop-down menu with various blow-up sizes
	set m .menubar.view.size
	.menubar.view add cascade -label "Blow-Up Size" -menu $m -underline 1
	menu $m
	global link_blowup_size link_blowup_zoom
	$m add radio -label "200x200" -variable link_blowup_size -value 200 -command {blowSize}
	$m add radio -label "300x300" -variable link_blowup_size -value 300 -command {blowSize}
	$m add radio -label "500x500" -variable link_blowup_size -value 500 -command {blowSize}
	$m add radio -label "800x800" -variable link_blowup_size -value 800 -command {blowSize}
	
	# Set the drop-down menu with various blow-up zoom factors
	set m .menubar.view.zoom
	.menubar.view add cascade -label "Blow-Up Zoom" -menu $m -underline 1
	menu $m
	$m add radio -label "0.5x" -variable link_blowup_zoom -value 2.0 -command {blowSize}
	$m add radio -label "1x" -variable link_blowup_zoom -value 1.0 -command {blowSize}
	$m add radio -label "4x" -variable link_blowup_zoom -value 0.25 -command {blowSize}
	$m add radio -label "10x" -variable link_blowup_zoom -value 0.1 -command {blowSize}
	set link_blowup_zoom 1.0
	
	############### Tools Menu ###############
	makeMenu tools "Tools" 0
	
	makeCommand {createBC} "Brightness & Contrast" b 0
	makeCommand {busy doHistogram} "Histogram" h 0
	
	$currMenu add separator
	makeCommand {changeTool zoom} "Blow Up" u 5
	makeCommand {changeTool info} "Point Info" i 6
	makeCommand {changeTool rect} "Select Rectangle" r 7
	makeCommand {changeTool poly} "Select Polygon" p 7
}

######### Called when user changes the blow-up window size 
# via the drop-down menu under "view".
proc blowSize {} {
# Blow away the old pop-up window & hide its traces
	catch {destroy .pop_blow}
	adornCanvases
}

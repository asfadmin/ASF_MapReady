# events.tcl
#
# This file handles the major application events in the SARview application.
#
# Orion Sky Lawlor, 5/6/99


################ doOpen command-- read an image ###############
proc doOpen {} {
	set types {
	    {{LAS Images}        {.img .amp .phase .dem .ht}        }
	    {{LAS Images}        {}        {BINA ????}}
	    {{CEOS Images}       {.D .dat}     }
            {{CEOS Images}       {}        {BINA CEOS ????}}
	    {{All Files}        *             }
	}
	
	set inName [tk_getOpenFile -defaultextension ".img" -filetypes $types ]
	if {$inName==""} return
	busy "doOpenFile $inName"
}

proc doOpenFile inName {
	set ret [cproc_loadimage $inName]
	if {$ret==0} return
#otherwise, we sucessfully loaded a new image-- weee!!!
# update various parts of the user interface.
	wm title . [list "SARview:" $inName]
	updateMenus

#Re-set global state variables
	global link_poly_x link_poly_y
	set link_poly_x ""
	set link_poly_y ""
	global bc_theta bc_grey
	set bc_theta 50.0
	set bc_grey 50.0
	
#Blow away old windows
	catch {destroy .pop_blow}
	catch {destroy .pop_image}
	catch {destroy .pop_info}
	catch {destroy .pop_bc}
	catch {destroy .pop_hist}

#Load up the new image
	imageChanged all
}

################ doExport command-- write image ###############
proc doExport {} {
	set types {
	    {{JPEG Images}        {.jpg}        }
	    {{JPEG Images}        {}        JPEG}
	    {{All Files}        *             }
	}
	set outName [tk_getSaveFile -defaultextension ".jpg" -filetypes $types ]
	if {$outName==""} return
	cproc_saveimage $outName
}

################ doQuit command-- quit program ###############
proc doQuit {} {
# This one's pretty simple...
	exit 1
}

############### doDown command-- mouse has been pressed ###########
# doDown accepts (x,y) in the actual image coordinates, and
# dispatches depending on the currently selected tool (link_tool).
proc doDown {x y} {
	global link_tool
#	tk_dialog .dbg "Mouse Down" "Mouse down at $x $y" {} 0 OK
	switch $link_tool {
	zoom {createBlowup $x $y}
	info {viewPixelInfo $x $y}
	poly {addPolyPoint $x $y}
	rect {addRectPoint $x $y}
	
	default {error "DoDown: Dunno what to do with the '$link_tool' tool ..."}
	}
}

############### changeZoom command-- re-set main image zoom factor ###########
proc changeZoom byFactor {
	global link_zoom
	set link_zoom [ expr $link_zoom*$byFactor ]
	imageChanged main
}

############### imageChanged command-- re-load main image ##########
# imageChanged upates the main window, etc.
# The "what" parameter indicates which windows need to be updated.
proc imageChanged what {
	
#Update the main view
	global link_imagewidth link_imageheight link_zoom
	cproc_drawtophoto img_main 0 0 $link_zoom \
		[expr int($link_imagewidth /$link_zoom)] \
		[expr int($link_imageheight/$link_zoom)]
	createMainCanvas
	if {[winfo exists .pop_blow]==1} {
		global link_blowup_x link_blowup_y link_blowup_size link_blowup_zoom
	#Update the img_blowup image
		cproc_drawtophoto img_blowup \
			$link_blowup_x $link_blowup_y $link_blowup_zoom \
			$link_blowup_size $link_blowup_size
	}
}

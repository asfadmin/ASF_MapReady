# p_bc.tcl
#
# This is the brightness & contrast pop-up window. 
# It allows the user to make the image brighter or darker.
#
# Orion Sky Lawlor, 5/16/99
#

################### Create the brightness & contrast window ####################
proc createBC {} {
#Set the output window name
	set w .pop_bc

# We have to create the entire window from scratch
	catch "destroy $w"
	toplevel $w
	wm title $w "Brightness & Contrast"
	wm iconname $w "Brt & Cnt"
	wm group $w .

	frame $w.frame
# Create go-away & apply buttons
	button $w.frame.dismiss -text "Cancel" -command "destroy $w"
	pack $w.frame.dismiss -side right
	button $w.frame.apply -text "Apply" -command "changeBC"
	pack $w.frame.apply -side left
	button $w.frame.restore -text "Restore" -command {
		global bc_theta bc_grey
		set bc_theta 50
		set bc_grey 50
		updateBC -99
		changeBC
	}
	pack $w.frame.restore -side bottom
	pack $w.frame -side bottom -fill x -expand yes -padx 20 -pady 4

# Create labels & sliders
	scale $w.bright -label "Brightness" -length 200 -orient horizontal -from 0 -to 100 \
		-command "updateBC" -variable bc_grey -tickinterval 25 -resolution 0.1 -showvalue false
	pack $w.bright -side top -expand yes -fill x

	scale $w.cont -label "Contrast" -orient horizontal -from 0 -to 100\
		-command "updateBC" -variable bc_theta -tickinterval 25 -resolution 0.1 -showvalue false
	pack $w.cont -side top -expand yes -fill x

# Determine size & create preview image
	global bc_previewzoom bc_prevX bc_prevY link_imagewidth link_imageheight
	set maxPreview 150
	set xZoom [expr double($link_imagewidth)/$maxPreview]
	set yZoom [expr double($link_imageheight)/$maxPreview]
	if {$xZoom>$yZoom} { # X is bigger-- use X zoom-out factor
		set bc_previewzoom $xZoom
	} else { # Y is bigger-- use Y zoom-out factor
		set bc_previewzoom $yZoom
	}
	set bc_prevX [expr int($link_imagewidth/$bc_previewzoom)]
	set bc_prevY [expr int($link_imagewidth/$bc_previewzoom)]
	updateBC 0
	label $w.preview -image img_bcpreview
	pack $w.preview -side bottom
}


############### updateBC: called above when brightness/contrast changes
# need to be applied to the preview image.  It takes a dummy parameter
# because the "scale widgets" above call it directly.

proc updateBC {ignored} {
	global link_slope link_offset
	set slope_saved $link_slope
	set offset_saved $link_offset
	computeBC
#Redraw preview image
	global bc_previewzoom bc_prevX bc_prevY
	cproc_drawtophoto img_bcpreview 0 0 $bc_previewzoom $bc_prevX $bc_prevY
#Restore old slope and offset
	set link_slope  $slope_saved
	set link_offset $offset_saved
}

############### changeBC: called above when brightness/contrast changes
# need to be applied to the main image.

proc changeBC {} {
	computeBC
#Redraw all images
	imageChanged all
}

############### computeBC: converts the global bc_theta and bc_grey
# into link_slope and link_offset; but does not update any images.
# These variables control what actually gets drawn next.
proc computeBC {} {
	global bc_theta bc_grey
	global link_slope link_offset link_r_slope link_r_offset
#Convert bc_theta and bc_grey from (0..100) into (0..pi/2) and (0..256)
	set theta [expr $bc_theta/100.0*3.14159265358979323/2]
	set grey [expr (100.0-$bc_grey)/100.0*512.0-128.0]
	
#Slope and offset are derived from theta and grey, and
#describe the transformation to be applied to the byte-scaled values 
	set slope [expr tan($theta*0.999)]
	set offset [expr 128-$slope*$grey]
	
#link_slope and link_offset combine the above with link_r_*, which
#describe how to convert image pixels into bytes.
	set link_slope [expr $slope*$link_r_slope]
	set link_offset [expr $slope*$link_r_offset+$offset]
}

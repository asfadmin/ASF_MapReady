# canvas.tcl
#
# This is a main interface portion of SARview.
#  It's responsible for maintaining the canvases, and
# their adornments.
#
# Orion Sky Lawlor, 5/13/99
#

######## Events relating to canvas #######
# doCanvasDown takes (x,y) in canvas coordinates, converts 
# them to the actual image coordinates, and calls doDown.
proc doCanvasDown {canvasNo x y} {
	cproc_fromCanvas $canvasNo x y
	doDown $x $y
}

############# adornCanvases #############
# This is called whenever the user's actions require
# the adornments on a canvas to be redrawn.
# It calls adornCanvas for each canvas that exists.

proc adornCanvases {} {
	adornCanvas .canvas 0
	if {1==[winfo exists .pop_blow]} {adornCanvas .pop_blow.canvas 1}
}

######## draw_poly_dex: draws a line between the polygon
# point index i_a & i_b
proc draw_poly_dex {canv canvasNo color i_a i_b} {
	global link_poly_x link_poly_y
	set x1 [lindex $link_poly_x $i_a]
	set y1 [lindex $link_poly_y $i_a]
	cproc_toCanvas $canvasNo x1 y1
	set x2 [lindex $link_poly_x $i_b]
	set y2 [lindex $link_poly_y $i_b]
	cproc_toCanvas $canvasNo x2 y2
	
	$canv create line $x1 $y1 $x2 $y2 -tags poly -fill $color -width 2.0
}

######## adornCanvas #############
proc adornCanvas {canv canvasNo} {
	global link_blowup_x link_blowup_y link_blowup_size link_blowup_zoom
	global link_tool
# Set color for blow-up box
	set blowColor "#26F"
# Set color for selected regions (polygon, rectangle)
	set selColor "#2F2"
	

# Update the canvas' currently selected polygon
	catch "$canv delete poly"
	global link_poly_x
	if {[llength $link_poly_x]>0} {
# Draw each segment of the polygon
		for {set i 1} {$i<[llength $link_poly_x]} {incr i} {
			draw_poly_dex $canv $canvasNo $selColor $i [expr $i - 1] 
		}
# Close the polygon, using a *darker* green line
		draw_poly_dex $canv $canvasNo "#080" [expr $i - 1] 0
	}

	
#Update the canvas' BlowUp window bounding box
	catch "$canv delete blow"
	if {1==[winfo exists .pop_blow]} {
#Set bounding box corners and convert to canvas coordinates
		set x1 [expr $link_blowup_x-1]
		set y1 [expr $link_blowup_y-1]
		cproc_toCanvas $canvasNo x1 y1
		set x2 [expr $link_blowup_x+$link_blowup_size*$link_blowup_zoom+1]
		set y2 [expr $link_blowup_y+$link_blowup_size*$link_blowup_zoom+1]
		cproc_toCanvas $canvasNo x2 y2
		$canv create rectangle $x1 $y1 $x2 $y2 -tags blow -outline $blowColor -width 2.0
	}

#Update the canvas' info cross
	catch "$canv delete info"
	if {1==[winfo exists .pop_info]} {
		global link_info_x link_info_y
#Set bounding box corners and convert to canvas coordinates
		set x1 $link_info_x
		set y1 $link_info_y
		cproc_toCanvas $canvasNo x1 y1
		set x1 int($x1)
		set y1 int($y1)
		$canv create line [expr $x1] [expr $y1+3] [expr $x1] [expr $y1+15] -tags info -fill $selColor -width 1.0
		$canv create line [expr $x1] [expr $y1-3] [expr $x1] [expr $y1-15] -tags info -fill $selColor -width 1.0
		$canv create line [expr $x1+3] [expr $y1] [expr $x1+15] [expr $y1] -tags info -fill $selColor -width 1.0
		$canv create line [expr $x1-3] [expr $y1] [expr $x1-15] [expr $y1] -tags info -fill $selColor -width 1.0
	}
	
}

# f_poly.tcl
#
# This is the polygon & rectangle selection handler
#
# Orion Sky Lawlor, 5/14/99
#

################ addPolyPoint is called by doDown when the user
## clicks with the polygon tool.

proc addPolyPoint {x y} {
	global link_poly_x link_poly_y
	lappend link_poly_x $x
	lappend link_poly_y $y
	
#Update the canvases with the expanded polygon
	adornCanvases
}


################ addRectPoint is called by doDown when the user
## clicks with the rectangle tool.  Note that the rectangle is just
## a polygon with a bit more state.

proc addRectPoint {x y} {
	global link_poly_x link_poly_y
	global rect_x rect_y
	if {[llength $link_poly_x]==0} {
	# This is the user's virgin click-- set one corner of the rectangle
		set rect_x $x
		set rect_y $y
		set link_poly_x $x
		set link_poly_y $y
	} else {
	# This is the user's second (or subsequent) click-- draw the
	# boundary of the rectagle
		set link_poly_x "$rect_x $rect_x $x      $x $rect_x"
		set link_poly_y "$rect_y      $y $y $rect_y $rect_y"
	}
	
#Update the canvases with the expanded rectangle
	adornCanvases
}


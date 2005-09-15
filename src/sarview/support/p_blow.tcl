# p_blow.tcl
#
# This is the blow-up pop-up window.  It shows a full-res version
#  of the image being veiwed.
#
# Orion Sky Lawlor, 5/12/99
#

################### Create the blow-up image window ####################
proc createBlowup {x y} {
	global link_blowup_x link_blowup_y link_blowup_size link_blowup_zoom
	global link_imagewidth link_imageheight
	
#Button was clicked at (original) x,y.  Create blowup window centered
# at these coordinates, unless out of bounds.
#blow_size is the size of the blow-up window in the original image coordinates
	set blow_size [expr int($link_blowup_size*$link_blowup_zoom)]
	if {$blow_size > $link_imagewidth} {set blow_size $link_imagewidth}
	if {$blow_size > $link_imageheight} {set blow_size $link_imageheight}
	set x [expr $x-int($blow_size/2.0)]
	set y [expr $y-int($blow_size/2.0)]
	if {$x<0} {set x 0}
	if {$y<0} {set y 0}
	if {$x>[expr $link_imagewidth-$blow_size-1]} {
		set x [expr $link_imagewidth-$blow_size-1]}
	if {$y>[expr $link_imageheight-$blow_size-1]} {
		set y [expr $link_imageheight-$blow_size-1]}
	set link_blowup_size [expr int($blow_size/$link_blowup_zoom)]
	set link_blowup_x [expr int($x)]
	set link_blowup_y [expr int($y)]

#Set the output window name
	set w .pop_blow

#Update the img_blowup image
	cproc_drawtophoto img_blowup $link_blowup_x $link_blowup_y $link_blowup_zoom \
		$link_blowup_size $link_blowup_size

#Update the canvas' coordinate system
	cproc_initCanvas 1 $link_blowup_x $link_blowup_y $link_blowup_zoom
	
#Display info to the screen-- check to see if window already exists
	if {1==[winfo exists $w]} {
# Window already exists--  update happens automatically
		
	} else {
# We have to create the entire window from scratch
		catch "destroy $w"
		toplevel $w
		wm title $w "BlowUp Window"
		wm iconname $w "BlowUp"
		wm group $w .
		bind $w <Destroy> {adornCanvases}
		keyBindings $w

		set c $w.canvas
# Create scroll bars
		scrollbar $w.hscroll -orient horiz -command "$c xview"
		scrollbar $w.vscroll -command "$c yview"
		pack $w.hscroll -side bottom -fill x
		pack $w.vscroll -side right -fill y

# Create the main canvas
		canvas $c \
			-width $link_blowup_size -height $link_blowup_size\
			-scrollregion  "0 0 $link_blowup_size $link_blowup_size" \
			-xscrollcommand "$w.hscroll set" \
			-yscrollcommand "$w.vscroll set"
	
#create the canvas' main image
		$c create image 0 0 -image img_blowup -anchor nw
	
#create the mouse-down event binding
		$c bind all <1> {doCanvasDown 1 [.pop_blow.canvas canvasx %x] [.pop_blow.canvas canvasy %y] }
		pack $c -fill both -expand yes
	} 
#Update the new and old canvases
	adornCanvases
}

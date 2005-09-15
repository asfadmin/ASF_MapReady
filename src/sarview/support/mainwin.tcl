# mainwin.tcl
#
# This is the main-window interface portion of SARview.
#  it's mostly responsible for maintaining the main canvas.
#
# Orion Sky Lawlor, 5/7/99
#

################### Create the main window scroll bars ####################
scrollbar .hscroll -orient horiz 
scrollbar .vscroll 
pack .hscroll -side bottom -fill x
pack .vscroll -side right -fill y


#### For Fun: load up the logo.
# We name it .canvas so when the user loads a real image, 
# it will overwrite the logo.
set fName [file join $link_path "images/logo.gif"]
image create photo img_logo -file $fName
label .canvas -image img_logo
pack .canvas

################### Create the main canvas, on command  #########
proc createMainCanvas {} {
	global link_zoom
#blow away old canvas
	catch {destroy .canvas}
#create new canvas
	set c .canvas
	set width [image width img_main]
	set height [image height img_main]
	canvas $c \
		-width $width -height $height \
		-scrollregion  "0 0 $width $height" \
		-xscrollcommand ".hscroll set" \
		-yscrollcommand ".vscroll set"
	.hscroll configure -command "$c xview"
	.vscroll configure -command "$c yview"

#create the canvas' main image
	$c create image 0 0 -image img_main -anchor nw
#create the mouse-down event binding
	$c bind all <1> {doCanvasDown 0 [.canvas canvasx %x] [.canvas canvasy %y]}

#Initialize the coordinate system of the canvas
	cproc_initCanvas 0 0.0 0.0 $link_zoom
#adorn the canvas with widgets
	adornCanvas $c 0

	pack $c -fill both -expand yes
}

################## Key bindings for moving info point in window ###########
proc keyBindings {w} {
	bind $w <h> {infoShift -1 0}
	bind $w <l> {infoShift  1 0}
	bind $w <j> {infoShift  0 -1} 
	bind $w <k> {infoShift  0  1} 	
	bind $w <Left>  {infoShift -1 0}
	bind $w <Right> {infoShift  1 0}
	bind $w <Up>    {infoShift  0 -1} 
	bind $w <Down>  {infoShift  0  1} 	
}

keyBindings .

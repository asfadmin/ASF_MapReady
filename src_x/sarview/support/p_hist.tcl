# p_hist.tcl
#
# This is the histogram pop-up window.
#
# Orion Sky Lawlor, 5/12/99
# Patrick Denny, July 2001:	Added statistics & histogram labels
#

set file1 [file join $link_path "images/xBar.gif"]
image create photo img_xBar -file $file1
set file2 [file join $link_path "images/yBar.gif"]
image create photo img_yBar -file $file2
################### Create the Histogram window ####################
proc doHistogram {} {
#Set the output window name
	set w .pop_hist
	
# We have to create the entire window from scratch
	catch "destroy $w"
	toplevel $w
	wm title $w "Histogram"
	wm iconname $w "Histo"
	wm group $w .

# Add buttons
	button $w.dismiss -text "OK" -command "destroy $w"
	pack $w.dismiss -side bottom

# Compute histogram
	cproc_renderhist 384 384 minHt maxHt minVal maxVal mean rms

# Display statistics
	set s $w.stat
	frame $s
	pack $s -fill x -side left -anchor n
	
	label $s.minHt -text "Min Occurance = $minHt" -font fixed
	pack $s.minHt -anchor w -padx 10 -pady 10

	label $s.maxHt -text "Max Occurance = $maxHt" -font fixed
	pack $s.maxHt -anchor w -padx 10 -pady 10

	label $s.minVal -text "Min Value = $minVal" -font fixed
	pack $s.minVal -anchor w -padx 10 -pady 10

	label $s.maxVal -text "Max Value = $maxVal" -font fixed
	pack $s.maxVal -anchor w -padx 10 -pady 10

	label $s.mean -text "Mean Value = $mean" -font fixed
	pack $s.mean -anchor w -padx 10 -pady 10

	label $s.rms -text "RMS Value  = $rms" -font fixed
	pack $s.rms -anchor w -padx 10 -pady 10

# Draw histogram with axis labels
	set h $w.hist
	frame $h
	pack $h -side left -fill both -padx 5 -pady 5
	
	label $h.img -image img_hist
	pack $h.img
	
	label $h.xBar -image img_xBar
	pack $h.xBar -side top

	label $h.yBar -image img_yBar
	pack $h.yBar -side left -anchor n -before $h.img

    # label x axis
	set midVal [expr ($minVal + $maxVal) / 2.0]
	set midminVal [expr ($minVal + $midVal) / 2.0]
	set midmaxVal [expr ($midVal + $maxVal) / 2.0]
	set minVal [format "%.2f" $minVal]
	set midminVal [format "%.2f" $midminVal]
	set midVal [format "%.2f" $midVal]
	set midmaxVal [format "%.2f" $midmaxVal]
	set maxVal [format "%.2f" $maxVal]

	set x $h.vals
	frame $x -height 20 -width 384
	pack $x -side top

	label $x.min -text "$minVal" -font "fixed"
	place $x.min -relx 0.03 -anchor n

	label $x.midmin -text "$midminVal" -font "fixed"
	place $x.midmin -relx 0.25 -anchor n
	
	label $x.mid -text "$midVal" -font "fixed"
	place $x.mid -relx 0.5 -anchor n
	
	label $x.midmax -text "$midmaxVal" -font "fixed"
	place $x.midmax -relx 0.75 -anchor n
	
	label $x.max -text "$maxVal" -font "fixed"
	place $x.max -relx 0.95 -anchor n

    # label y axis
	set midHt [expr ($minHt + $maxHt) / 2]
	set midminHt [expr ($minHt + $midHt) / 2]
	set midmaxHt [expr ($midHt + $maxHt) / 2]
	set minHt [format "%.0f" $minHt]
	set midminHt [format "%.0f" $midminHt]
	set midHt [format "%.0f" $midHt]
	set midmaxHt [format "%.0f" $midmaxHt]
	set maxHt [format "%.0f" $maxHt]

	set y $h.ht
	frame $y -height 386 -width 100 -borderwidth 5
	pack $y -side left -anchor n -before $h.yBar

	label $y.min -text "$minHt" -font "fixed"
	place $y.min -rely 1.0 -relx 1.0 -anchor e

	label $y.midmin -text "$midminHt" -font "fixed"
	place $y.midmin -rely 0.753 -relx 1.0 -anchor e
	
	label $y.mid -text "$midHt" -font "fixed"
	place $y.mid -rely 0.5 -relx 1.0 -anchor e
	
	label $y.midmax -text "$midmaxHt" -font "fixed"
	place $y.midmax -rely 0.247 -relx 1.0 -anchor e
	
	label $y.max -text "$maxHt" -font "fixed"
	place $y.max -rely 0.0 -relx 1.0 -anchor e
}

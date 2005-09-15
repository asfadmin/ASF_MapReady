# cursor.tcl
#
# This is causes the cursor to change to a watch while a process works
#
# Patrick Denny, July, 2001
#

proc busy {cmds} {
	global errorInfo

# Create list of windows needing cursor to be a watch
	set busy {. .menubar}
	if {1==[winfo exists .pop_about]}	{lappend busy .pop_about}
	if {1==[winfo exists .pop_blow]}	{lappend busy .pop_blow}
	if {1==[winfo exists .pop_image]}	{lappend busy .pop_image .pop_image.text}
	if {1==[winfo exists .pop_info]}	{lappend busy .pop_info .pop_info.text}
	if {1==[winfo exists .pop_bc]}		{lappend busy .pop_bc}
	if {1==[winfo exists .pop_hist]}	{lappend busy .pop_hist}
	if {1==[winfo exists .pop_flic]}	{lappend busy .pop_flic}

# Cursor is a watch in each window, 'w'
	foreach w $busy {
		catch {[lindex $w 0] config -cursor watch}
	}

# Let SARview work
	update idletasks

# Watch for errors
	set error [catch {uplevel eval [list $cmds]} result]
	set ei $errorInfo

# Change cursor back to normal for each window, 'w'
	foreach w $busy {
		catch {[lindex $w 0] config -cursor [lindex $w 1]}
	}

# Report any errors
	if $error {
		error $result $ei
	} else {
		return -code $error $result
	}
}

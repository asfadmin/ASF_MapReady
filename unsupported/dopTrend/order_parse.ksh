#!/usr/bin/ksh
#
# This is a script for taking a file that has the format
#     Format: ramp/orderNumber/fileName   Beam   DopplerOffset
# and creating a script that will place orders for the
# revs/frames/beams for RAMM left looking data

cut -d '/' -f 3 $1 >| 2t
cut -c 3,4,5,6,7 2t >| rev1
cut -c 8,9,10 2t >| frame1
cut -f 2 2t >| beam1
paste rev1 beam1 frame1 >| order_2.data
/bin/rm 2t rev1 beam1 frame1

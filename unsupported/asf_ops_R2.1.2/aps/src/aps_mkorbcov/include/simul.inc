     -/'@(#)simul.inc	5.1 98/01/08 APS/ASF\0'/

*.
*   Simul.inc - simultaneous coverage display variables
*
*   Needs INSTRUMENTS.INC for MAXTRACK
*
*	11/28/88 11:09
*..

*     StackSize - number of swath chunks we'll have to remember
*     NumSimBox - number of boxes we'll remember
	integer StackSize
	integer NumSimBox

	integer MAXSTACKSIZE
	parameter(MAXSTACKSIZE=35000)
	integer MAXSIMBOX
	parameter(MAXSIMBOX=MAXSTACKSIZE/2-1)
c	integer BoxStack(2,MAXTRACK,MAXSTACKSIZE)
c	logical whole(2,MAXTRACK/2,MAXSIMBOX)
	integer BoxStack(2,4,MAXSTACKSIZE)
	logical whole(2,2,MAXSIMBOX)
	integer NumSimTracks(2)
c Added to keep track of rev & time at overlapping regions .. aap .. 11/1/93
	double precision TimeStack(2,MAXSIMBOX)
	integer RevStack(2,MAXSIMBOX)
c
	common /eos$simul/ StackSize,NumSimBox,BoxStack,
     .				whole,NumSimTracks
c aap .. 11/1/93
	common /otherstacks/ TimeStack,RevStack

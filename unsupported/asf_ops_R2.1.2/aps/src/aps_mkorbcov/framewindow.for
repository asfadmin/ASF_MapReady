*.
*   FrameWindow - draw a frame around the graphics window
*
*	09/08/88 9:45
*..
	subroutine FrameWindow
	include 'eosinclude:display.inc'

	  character*100 SccsFileID
     -/'@(#)framewindow.for	5.1 98/01/08 APS/ASF\0'/
	call GrafComment('Frame')

	call DrawBox(screenx(1),screeny(1),screenx(2),screeny(2))

	end

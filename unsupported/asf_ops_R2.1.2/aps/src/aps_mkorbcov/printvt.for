
*.
*   PrintVT100 and PrintVT220 - dump graphics screen to attached printer
*
*	09/06/88 10:10
*..
	subroutine PrintVT100
	include 'eosinclude:display.inc'

	  character*100 SccsFileID
     -/'@(#)printvt.for	5.1 98/01/08 APS/ASF\0'/
	write(*,'(a,/)') ' Printing...'

	write(*,'(4a,$)') '+',esc1,eschcpy,esc2

	end


	subroutine PrintVT220
	include 'eosinclude:display.inc'

	write(*,'(a,/)') ' Printing...'

	write(*,'(6a,$)') '+',esc5i,esc1,escetb,esc2,esc4i

	end

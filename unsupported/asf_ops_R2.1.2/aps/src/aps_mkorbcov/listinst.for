*.
*   ListInst - Show instrument names, numbers
*
*   Commons
*	eos$instruments - InstName,NumInst
*
*   History
*	03/06/91 Use ShowNames routine
*	03/11/89 Original
*
*   Last modified
*	03/06/91 11:34
*..
	subroutine ListInst

	  character*100 SccsFileID
     -/'@(#)listinst.for	5.1 98/01/08 APS/ASF\0'/
** Includes
	include 'eosinclude:instruments.inc'

	write(*,*)
	call ShowNames(InstName,NumInst)

	end

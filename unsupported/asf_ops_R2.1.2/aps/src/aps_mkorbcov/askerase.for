*.
*   Askerase - prompt to erase screen
*
*	02/15/89 07:59
*..
	subroutine AskErase
      character*100 SccsFileID
     -/'@(#)askerase.for	5.1 98/01/08 APS/ASF\0'/

	include 'eosinclude:display.inc'

	character YesNo

	if(disp.ne.0 .and. disp.ne.NONE) then
	    write(*,*)
10	    write(*,'(a,$)') ' Erase graphics screen [no]? '
	    read(*,'(a)',err=10,end=999) YesNo
	    if(YesNo.eq.'Y' .or. YesNo.eq.'y') call clearg
	end if

	go to 1000
999	continue
	call exit(0)
1000	continue
	end

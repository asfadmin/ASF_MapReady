
*.
*   ReadPlatFile - read platform data file
*
*   Args
*	FileInput	C**	input	Platform file name (if blank,
*					  open default file, if NUL,
*					  ask for filename)
*
*	See the routine ReadPlatLine for a description of the platform keys,
*		and ReadInstLine for a description of instrument keys
*
*   History
*	06/11/90 If FileInput(1:1)=NUL, ask for filename
*	12/03/89 Original
*
*	12/03/89 16:03
*..
	subroutine ReadPlatFile(FileInput)
	character*80 FileInput

	  character*100 SccsFileID
     -/'@(#)readplatfile.for	5.1 98/01/08 APS/ASF\0'/
** Includes
	include 'eosinclude:files.inc'
	include 'eosinclude:instruments.inc'
	include 'eosinclude:pen.inc'

** Variables
* Instrument file
	integer uIn,ios
	logical NotOk
	character*80 fIn,InLine

* Keys
	logical NewPlat,NewInst,NoKey

* Other
	integer iplat,iinst,instindex,s1,s2
	character*80 prompt

** Functions
	integer StringLen

** Data
	data uIn /31/

* Open platform file

*	if(FileInput(1:1).eq.' ') then
*	    fIn = DataDir // 'platform.dat'
*	else if(FileInput(1:1).eq.char(0)) then
*	    write(*,*)
*10	    write(*,'(a,$)') ' Enter platform data filename [default]: '
*	    read(*,'(a)',err=10,end=999) fIn
*	    if(fIn(1:1).eq.' ') fIn = DataDir // 'platform.dat'
*	else
*	    fIn = FileInput
*	end if

	inquire(unit=uIn,opened=NotOk)
	if(NotOk) then
	    call ErrorMessage('ReadPlat - unit already open')
	    return
	end if
	fIn = FileInput(1:6)
	open(unit=uIn,file=fIn,status='old',
     .		form='formatted',iostat=ios,err=20)
c..aap     .		form='formatted',readonly,iostat=ios,err=20)
20	continue
	if(ios.ne.0) then
	    call ErrorMessage('ReadPlat - couldn''t open platform file '
     .				// fIn(1:StringLen(fIn)))
	    call WaitKey
	    return
	end if

c	iplat = NumPlat
	iplat = 0
	instindex = 0

	do while(.true.)
	    read(uIn,'(a)',err=200,end=200) InLine
	    call ReadPlatLine(InLine,iplat,NewPlat,NoKey)
	    if(NoKey) call ReadInstLine(InLine,instindex,NewInst,NoKey)
	    if(.not.NoKey) then
		if(iplat.gt.0) then
		    if(NewInst) then
			NumInstPlat(iplat) = NumInstPlat(iplat) + 1
			iinst = NumInstPlat(iplat)
			platform(iplat,iinst) = instindex
			s1 = StringLen(InstName(instindex))
			s2 = StringLen(PlatName(iplat))
			prompt = 'for ' // InstName(instindex)(1:s1)
     .				// ' on ' // PlatName(iplat)(1:s2)
			call GetColor(color(iplat,iinst),prompt(1:s1+s2+8))
		    else
			call PreSetUpInst(iplat,iinst)
		    end if
		else
c		    write(*,*) 'Oops - instrument before a platform!'
		end if
	    end if
	end do

200	continue
	close(uIn)
	
999	continue
	end


*.
*   ReadInstFile - read instrument data file
*
*   Args
*	FileInput	C**	input	Instrument file name (if blank,
*					  open default file)
*
*   Notes
*	See the routine ReadInstLine for a description of the instrument file
*
*   History
*	12/03/89 Original - modified from ReadInst
*
*	12/03/89 14:57
*..
	subroutine ReadInstFile(FileInput)
	character*(*) FileInput

	  character*100 SccsFileID
     -/'@(#)readinstfile.for	5.1 98/01/08 APS/ASF\0'/
** Includes
	include 'eosinclude:files.inc'

** Variables
* Instrument file
	integer uIn,ios
	logical NotOk
	character*80 fIn,InLine

* Keys
	logical NewInst,NoKey

* Other
	integer iinst

** Functions
	integer StringLen

** Data
	data uIn /31/

* Open instrument file

	if(FileInput(1:1).eq.' ') then
	    fIn = DataDir // 'instrus.dat'
	else
	    fIn = FileInput
	end if

	inquire(unit=uIn,opened=NotOk)
	if(NotOk) then
	    call ErrorMessage('ReadInst - unit already open')
	    return
	end if

	open(unit=uIn,file=fIn,status='old',
     .		form='formatted',iostat=ios,err=10)
c..aap     .		form='formatted',readonly,iostat=ios,err=10)
10	continue
	if(ios.ne.0) then
	    call ErrorMessage('ReadInst - couldn''t open instrument file '
     .				// fIn(1:StringLen(fIn)))
	    return
	end if

	iinst = 0

* Read instrument data

	do while(.true.)
	    read(uIn,'(a)',err=200,end=200) InLine
	    call ReadInstLine(InLine,iinst,NewInst,NoKey)
	end do

200	continue
	close(uIn)

	end

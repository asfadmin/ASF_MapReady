
*.
*   MenuInst - instrument menu for EosMenu
*
*   History
*	 3/20/91 Fixed help
*	 4/20/90 Consolidated MenuAddInst, MenuInst, MenuSetupInst
*	 7/21/89 Add "Read instrument data file" menu item
*	 3/18/89 Original
*
*   Last modified
*	3/20/91 17:17
*..
	subroutine MenuInst(iMenu,instrument,slen)
	integer iMenu
*	double precision yawPoint
	character*80 instrument

** Includes
	include 'eosinclude:instruments.inc'
	include 'eosinclude:eossimul.inc'
	include 'eosinclude:pen.inc'

	  character*100 SccsFileID
     -/'@(#)menuinst.for	5.1 98/01/08 APS/ASF\0'/
** Variables
* Menu
	integer MaxMenu
	parameter(MaxMenu=10)
	character*40 MenuI(MaxMenu)
	character MenuC(MaxMenu)
	integer itemNew,itemAdd,itemSetup,itemList
	integer itemRead,itemWrite,itemRemove,itemInfo
	integer itemPen,itemHelp
	parameter(itemNew=1,itemAdd=2,itemSetup=3,itemList=4)
	parameter(itemRead=5,itemWrite=6,itemRemove=7,itemInfo=8)
	parameter(itemPen=9,itemHelp=MaxMenu)
	logical done

	character*80 FileInst
	integer iplat,iinst,instnum,ip
	integer slen

	character prompt*80,YesNo

	logical NewInst,NoKey
	character*80 InLine

** Data
	data MenuI /	'New Instrument',
     .			'Add Instrument to Platform',
     .			'Setup Instrument',
     .			'List Names',
     .			'Read Instrument Data File',
     .			'Write Instrument Data File',
     .			'Remove Instrument from Platform',
     .			'Instrument Info',
     .			'Pen',
     .			'Help'/
	data MenuC /'N','A','S','L','R','W','X','I','P','?'/

** Functions
	integer StringLen

	done = .false.

	do while(.not.done)

*	    call MenuInput('Instruments',MenuI,MenuC,MaxMenu,iMenu)

	    if(iMenu.eq.itemNew) then

		iinst = NumInst
		write(*,'(/,a)')
     .		' Enter instrument description (RETURN when done):'
		do while(.true.)
10		    read(*,'(a)',err=10,end=15) InLine
		    if(InLine(1:1).eq.' ') go to 15
		    call ReadInstLine(InLine,iinst,NewInst,NoKey)
		    if(NoKey)
     .		     write(*,*) '  No valid keywords found in this line...'
		end do
15		continue

	    else if(iMenu.eq.itemAdd) then

		call GetPlatNum(iplat)
		if(iplat.ne.0) then
		    call GetInstNum(instnum,instrument,slen)
		    if(instnum.ne.0) then
			NumInstPlat(iplat) = NumInstPlat(iplat) + 1
			iinst = NumInstPlat(iplat)
			platform(iplat,iinst) = instnum
			call PreSetupInst(iplat,iinst)
			slen = StringLen(InstName(instnum))
			prompt = 'for ' // InstName(instnum)(1:slen)
			color(iplat,iinst) = (iplat-1)*MAXINSTPERPLAT + iinst
			call GetColor(color(iplat,iinst),prompt(1:slen+4))
		   end if
		end if

		done = .true.

	    else if(iMenu.eq.itemSetup) then

		call GetPlatNum(iplat)
		if(iplat.ne.0) then
		    call GetInstIndex(iplat,iinst)
		    if(iinst.ne.0) call SetupInst(iplat,iinst,slen)
		end if

		done = .true.

	    else if(iMenu.eq.itemList) then

		call ListInst
		call WaitKey

	    else if(iMenu.eq.itemRead) then

		if(NumInst.gt.0) then
		    write(*,*)
20		    write(*,'(a,$)')
     .		    ' Really forget the current set of instruments [no]? '
		    read(*,'(a)',err=20,end=900) YesNo
		end if
		if(NumInst.lt.1 .or.
     .			(NumInst.ge.1 .and.
     .				(YesNo.eq.'y' .or. YesNo.eq.'Y'))) then
		    write(*,*)
25		    write(*,'(a,$)')
     .			' Enter instrument data filename [default]: '
		    read(*,'(a)',err=25,end=900) FileInst
		    call ReadInstFile(FileInst)
		    call ListInst
		    call WaitKey
		end if

	    else if(iMenu.eq.itemWrite) then

		write(*,'(/,a)') ' Nothing here yet...'
		call WaitKey

	    else if(iMenu.eq.itemRemove) then

		call GetPlatNum(iplat)

		if(iplat.ne.0) then
		    iinst = NumInstPlat(iplat)
		    if(iinst.ne.0) then
			ip = platform(iplat,iinst)
*	write(*,*)
*	write(*,'(3a,i2,a,$)')
* .			    ' Really remove ',
* .				InstName(ip)(1:StringLen(InstName(ip))),
* .				' on platform ',iplat,' [no]? '
*		read(*,'(a)',err=900,end=900) YesNo
*		if(YesNo.eq.'y' .or. YesNo.eq.'Y') then
			    NumInstPlat(iplat) = NumInstPlat(iplat) - 1
			    if(ShowSimul) then
				if((iplat.eq.simplat(1)
     .					.and. iinst.eq.siminst(1)) .or.
     .				   (iplat.eq.simplat(2)
     .					.and. iinst.eq.siminst(2))) then
				    ShowSimul = .false.
				    SimulOk = .false.
				end if
			    end if
*		    write(*,'(/,3a,i2)')
* .' Removed ',InstName(ip)(1:StringLen(InstName(ip))),
* .' from platform ',NumPlat
*			    call WaitKey
*			end if
		    end if
		end if


		done = .true.



	    else if(iMenu.eq.itemInfo) then

		call GetInstNum(iinst,instrument,slen)
		if(iinst.ne.0) then
		    slen = StringLen(InstFullName(iinst))
		    write(*,30) InstName(iinst),iinst,
     .				  InstFullName(iinst)(1:slen),
     .				  InstTypeText(InstType(iinst))
30		    format(/,1x,'     Name: ',a,5x,'(#',i2,')',
     .				/,1x,'Full name: ',a,
     .				/,1x,'     Type: ',a)
		    call WaitKey
		end if

	    else if(iMenu.eq.itemPen) then

		call GetPlatNum(iplat)
		if(iplat.ne.0) then
		    call GetInstIndex(iplat,iinst)
		    if(iinst.ne.0) then
			ip = platform(iplat,iinst)
			slen = StringLen(InstName(ip))
			prompt = 'for ' // InstName(instnum)(1:slen)
			call MenuPen(color(iplat,iinst),prompt(1:slen+4))
		    end if
		end if

	    else if(iMenu.eq.itemHelp) then

		call NewPage
		write(*,'(99(/,a))')
     .' In this menu you may create new instruments, add or remove',
     .'   an instrument to/from a platform, set view parameters,',
     .'   read and write instrument data files, get additional',
     .'   information about an instrument and change the drawing pen',
     .' ',
     .' To create a new instrument use the following keywords:',
     .' ',
     .'   INSTNAME            (must be the first item) (required)',
     .'                         Example: instname = ''BigSAR''',
     .'   FULLNAME            (full instrument name)',
     .'   TYPE                (instrument type) (required)',
     .'                         Choose from BEAM, SCAT, LIMB, LIMB2',
     .'                         Example: type = ''scat''',
     .'   LOOKMIN, -MAX       (min, max look angle (deg))',
     .'                         Example: lookmin = 15.0'
		call WaitKey
		call NewPage
		write(*,'(99(/,a))')
     .'   YAWMIN, -MAX        (min, max yaw angle (deg))',
     .'                         Example: yawmin = 90.0',
     .'   BEAMWIDTH           (beam width (deg))',
     .'                         Example: beamwidth = 5.0',
     .'   SWATHWIDTH          (swath width (km))',
     .'                         Example: swathwidth = 2000.0',
     .'   MASK                (masking)',
     .'                         Choose from SUNPLATFORM, SUNTARGET,',
     .'                           STATION, ASCENDING, DESCENDING',
     .'                         Example: mask = ''station''',
     .'   SUNELEVMIN          (min sun elevation (deg))',
     .'                         Example: sunelevmin = 20.0',
     .'   LIMBALTMIN, -MAX    (min, max limb altd (km))',
     .'                         Example: limbaltmin = 50.0',
     .'   LIMBTHICKMIN, -MAX  (min, max limb thickness (km))',
     .'                         Example: limbthickmin = 100.0'
		call WaitKey

	    else if(iMenu.eq.0) then

		done = .true.

	    end if

900	    continue

	end do

999	continue
	end

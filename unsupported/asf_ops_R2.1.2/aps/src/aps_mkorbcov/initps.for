
*.
*   InitPS - initialize PostScript printing
*
*	 6/20/91 12:45 Use PS-Adobe-3.0 conventions
*	11/28/88 16:08
*..
	subroutine InitPS

	character cdate*20,ctime*20
	integer lendate,lentime

      character*100 SccsFileID
     -/'@(#)initps.for	5.1 98/01/08 APS/ASF\0'/
	integer ret
	character esc
	character*80 FilePS

* Functions
	integer StringLen

* Includes
	include 'eosinclude:display.inc'

	write(*,*)
10	write(*,'(a,$)') ' Enter Postscript filename [out.ps]: '
	read(*,'(a)',err=10,end=999) FilePS
	if(FilePS(1:6).eq.'      ') FilePS = 'out.ps'
	open(unit=UnitPS,file=FilePS,form='formatted',
     .			status='new',err=10)
c..aap     .			status='new',carriagecontrol='list',err=10)

	PSPageNum = 1

	call GetSysDateTime(cdate,ctime,lendate,lentime)
	write(UnitPS,21) FilePS(1:StringLen(FilePS)),
     .				cdate(1:lendate),ctime(1:lentime)
21	format( '%!PS-Adobe-3.0',/,'%%Creator: Eos',/,
     .	'%%Title: ',a,/,'%%CreationDate: ',a,1x,a)

	write(UnitPS,'(a)') '%%DocumentData: Clean7Bit'
	write(UnitPS,'(a)') '%%For:'
	write(UnitPS,'(a)') '%%Pages: (atend)'
	write(UnitPS,'(a)') '%%BoundingBox: 36 36 576 756'
	write(UnitPS,'(a)') '%%DocumentFonts: Times-Roman'
	write(UnitPS,'(a,/)') '%%EndComments'

	write(UnitPS,'(a)') '%%BeginDefaults'
	write(UnitPS,'(a)') '%%PageResources: font Times-Roman'
	write(UnitPS,'(a,/)') '%%EndDefaults'

	write(UnitPS,'(a)') '{md} stopped {(no md found) print flush} if'
c	write(UnitPS,'(a)') 'gsave                   % save current state'
	write(UnitPS,'(a)') '/thou {.072 mul} def    % coords in 1000ths'
	write(UnitPS,'(a)') '/mto {moveto} def       % save some bytes'
	write(UnitPS,'(a)') '/lto {lineto} def       % save some bytes'
	write(UnitPS,'(a,/)') '%%EndProlog'

	write(UnitPS,'(a)') '%%BeginSetup'
	write(UnitPS,'(a)') '1 thou 1 thou scale'
	write(UnitPS,'(a,/)') '%%EndSetup'

	write(UnitPS,'(a)') '6.666 setlinewidth      % 2 point'

	write(UnitPS,'(/,a,i5,1x,i5,/)') '%%Page: ',PSPageNum,PSPageNum

999	continue
	end

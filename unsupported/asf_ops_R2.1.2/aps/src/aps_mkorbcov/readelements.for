
*.
*   ReadElements - reads default orbital elements
*
*   Args
*	OrbitName()		C**	output	array of orbit names
*	DefaultElements(7,)	double	output	array of orbital element arrays
*	NumOrbits		integer	output	number of orbits read
*	MaxOrbits		integer	input	max orbits to read
*
*	10/26/88 13:00
*..
	subroutine ReadElements(OrbitName,DefaultElements,
     .						NumOrbits,MaxOrbits)

	include 'eosinclude:files.inc'
	include 'eosinclude:cbody.inc'
	include 'eosinclude:time.inc'
	include 'eosinclude:constants.inc'

	  character*100 SccsFileID
     -/'@(#)readelements.for	5.1 98/01/08 APS/ASF\0'/
	integer NumElem
	parameter(NumElem=7)
	character*(*) OrbitName(*)
	double precision DefaultElements(NumElem,*)
	integer NumOrbits,MaxOrbits

	double precision a,e,i,node0,omega0,m0,t0

	integer UnitOrbit
	character*80 FileOrbit

	integer iorbit
	character*80 FormatElements

	integer j

	integer MAXCOLS
	parameter (MAXCOLS=5)
	integer orbnum(MAXCOLS)
	character*10 Field(MAXCOLS)
	integer rows,cols,row,col,lastcol

c Init
	call CheckLogName
	NumOrbits = 0

c Open orbit file

c	call NewPage

	UnitOrbit = 13

c10	write(*,'(a,$)')
c     .		' Enter orbit data filename [default]: '
c	read(*,'(a)',err=10,end=999) FileOrbit
c	if(FileOrbit(1:5).eq.'     ') FileOrbit = DataDir//'orbit.dat'
c	open(unit=UnitOrbit,file=FileOrbit,status='old',
c     .		form='formatted',err=10)

	FileOrbit = DataDir//'orbit.dat'
	open(unit=UnitOrbit,file=FileOrbit,status='old',
     .		form='formatted',err=999)
c..aap     .		form='formatted',readonly,err=999)

	read(UnitOrbit,'(a)',err=999,end=999) FormatElements

	write(*,'(/a/)') '   Default orbital elements: '

c Read default orbits

	do 100 iorbit=1,MaxOrbits
	    read(UnitOrbit,FormatElements,err=200,end=200)
     .		OrbitName(iorbit),(DefaultElements(j,iorbit),j=1,NumElem)
c	    write(*,'(3x,i2,1x,a10)') iorbit,OrbitName(iorbit)
	    NumOrbits = iorbit
100	continue

200	continue

	cols = MAXCOLS  

	if(NumOrbits.lt.cols) then
	    cols = 1
	    rows = NumOrbits
	else
	    rows = NumOrbits / cols
	    if(mod(NumOrbits,cols).ne.0) rows = rows + 1
	end if

	do 220 row=1,rows

	    lastcol = cols

	    do 210 col=1,cols
		orbnum(col) = (col-1) * rows + row
		if(orbnum(col).le.NumOrbits) then
		    Field(col) = OrbitName(orbnum(col))
		else
		    lastcol = cols - 1
		    go to 211
		end if
210	    continue
211	    continue

	    write(*,'(99(i4,1x,a))') (orbnum(col),Field(col),col=1,lastcol)

220	continue

999	continue
	close(UnitOrbit)

	end


*.
*   ShowNames - present list of names with a number next to each
*
*   Args
*	Name		C**	input	array of names
*	NumName		I	input	number of names in list
*
*	05/07/90 - stolen from ReadElements
*
*	05/07/90 09:51
*..
	subroutine ShowNames(Name,NumName)
	character*(*) Name(*)
	integer NumName

	  character*100 SccsFileID
     -/'@(#)shownames.for	5.1 98/01/08 APS/ASF\0'/
	integer row,rows,col,cols,lastcol
	integer MAXCOLS
	parameter(MAXCOLS=10)
	character*80 Field(MAXCOLS)
	integer namenum(MAXCOLS)
	integer slen

	cols = 80 / (len(Name(1))+5)
	cols = min(cols,MAXCOLS)

	slen = min(len(Name(1)),len(Field(1)))

	if(NumName.lt.cols) then
	    cols = 1
	    rows = NumName
	else
	    rows = NumName / cols
	    if(mod(NumName,cols).ne.0) rows = rows + 1
	end if

	do 20 row=1,rows

	    lastcol = cols

	    do 10 col=1,cols
		namenum(col) = (col-1) * rows + row
		if(namenum(col).le.NumName) then
		    Field(col) = Name(namenum(col))
		else
		    lastcol = cols - 1
		    go to 11
		end if
10	    continue
11	    continue

	    write(*,'(99(i4,1x,a))')
     .			(namenum(col),Field(col)(1:slen),col=1,lastcol)

20	continue

	end

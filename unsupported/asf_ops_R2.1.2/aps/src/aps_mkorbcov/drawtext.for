*.
*   DrawText - draw a text string
*
*	call DrawText(x,y,text,chars,height,font)
*
*		x,y - location of lower left of first character (I)
*		text - text string to draw (C*(*))
*		chars - number of characters to draw (I)
*		height - height of characters (I)
*		font - font to use (I)
*
*   History
*	06/08/90 if negative height given for POSTSCRIPT right-justify
*	03/29/89
*
*   Last modified
*	06/08/90 13:06
*..
	subroutine DrawText(x,y,text,chars,height,font)
	include 'eosinclude:constants.inc'	! aap..11/4/93
	integer x,y
	character*(*) text
	integer chars,height,font

	 character*100 SccsFileID
     -/'@(#)drawtext.for	5.1 98/01/08 APS/ASF\0'/
	logical*1 yhi,ylo,xhi,xlo
	integer ijust,JUSTLEFT,JUSTCENTER,JUSTRIGHT
	parameter (JUSTLEFT=1,JUSTCENTER=2,JUSTRIGHT=3)

	include 'eosinclude:display.inc'

	integer len

** Functions
	integer StringLen

	if(disp.eq.RAM9460) then
c	    if(font.eq.0) font = 3
	    len = StringLen(text)
c	    call RamText(text,len,font,height,x,y,255)
c..aap	    call RamText(text,len,3,height,x,y,255)
	    call RamText(text,len,ithree,height,x,y,i255)
	else if(disp.eq.TEK4105MAC .or. disp.eq.TEK4208) then
	    call TekXY(x,y,yhi,ylo,xhi,xlo)
	    write(*,'(7a,$)') '+',char(27),'LF',yhi,ylo,xhi,xlo
	    write(*,'(3a,$)') '+',char(chars),text(1:chars)
	else if(disp.eq.POSTSCRIPT) then
	    ijust = JUSTLEFT
	    if(height.lt.0) then
		height = abs(height)
		ijust = JUSTRIGHT
	    end if
	    if(ijust.eq.JUSTLEFT) then
		write(UnitPS,10) height,x,y,text(1:chars)
10		format('/Times-Roman findfont ',i5,' scalefont setfont',/,
     .			i5,1x,i5,' mto (',a,') show')
	    else if(ijust.eq.JUSTRIGHT) then
		write(UnitPS,11) height,x,y,text(1:chars)
11		format('/Times-Roman findfont ',i5,' scalefont setfont',/,
     .			i5,1x,i5,' mto (',a,')',
     .			' dup stringwidth pop neg 0 rmoveto show')
	    end if
	end if

	end

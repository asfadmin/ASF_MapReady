
      subroutine ramtext(string,length,font,dumsize,ix,iy,dn)

C******************************************************************************
C
C     Fred Burnette  :  March 5, 1986
C
C     Do you know where your cat is?  Dr. Mikey does!
C
C     Routine to write text on the Ramtek using only Longlib routines.  The
C     calling program may specify the location of the text string, the dn value,
C     the height of the characters and one of ten different fonts.
C
C     Parameters
C       string - character string to be written
C       length - length of character string
C       font   - font number (0 - 9)
C       size   - height of characters in pixels
C       ix     - X location in pixels of lower-left corner of text string
C       iy     - Y location in pixels of lower-left corner of text string
C       dn     - plane number of Ramtek to write text into
C
C     Variables
C       height - height of characters in inches
C       ichan  - channel number returned from 'whererm' call.  if ichan <= 0
C                then the ramtek has not been initialized for longlib calls.
C       x      - X-location in inches of lower-left corner of text
C       xres   - inches per pixel returned from 'whererm' call
C       y      - Y-location in inches of lower-left corner of text
C       yres   - inches per pixel returned from 'whererm' call
C
C     Subroutines     Frame, Plot, Syms, Whererm
C
C******************************************************************************

      include 'eosinclude:constants.inc'
      integer length,size,font,ix,iy,dn
      integer ichan

      character*100 SccsFileID
     -/'@(#)ramtext.for	5.1 98/01/08 APS/ASF\0'/
      real x,y,height
      real xres,yres
      integer centerflag,dumsize

      character*(*) string
      character*2 fonttype

c
      real d1,d2,d3,d4
      integer fontlen,i1,i2,i3
c
	if(dumsize.lt.0) then
c	  x,y is center of string
	    centerflag = 0
	else
c	  x,y is lower left of string
	    centerflag = -1
	end if
	size = abs(dumsize)
C
C     DETERMINE IF RAMTEK HAS BEEN INITIALIZED FOR LONGLIB ROUTINES.  XRES AND
C     YRES ARE RETURNED VALUES OF INCHES PER PIXEL.
C
      call whererm(d1,d2,d3,d4,xres,yres,i1,i2,i3,ichan)

      if (ichan .le. 0) then
c..aap         call frame(-3,-2,0.,0.,1.)
         call frame(ineg3,ineg2,dzero,dzero,d_one)
         call whererm(d1,d2,d3,d4,xres,yres,i1,i2,i3,ichan)
      end if
C
C     CONVERT RAMTEK COORDINATES TO INCHES.  RAMTEK ORIGIN IS IN UPPER-LEFT
C     CORNER OF SCREEN.  NEW ORIGIN FOR LONGLIB ROUTINES IS IN LOWER-LEFT
C     CORNER OF SCREEN.
C
      x = float(ix) * xres
      y = float(1023 - iy) * yres
C
C     CONVERT CHARACTER HEIGHT TO INCHES
C
      height = float(size) * yres
C
C     SET WHICH DN WE WILL WRITE INTO
C
c..aap      call plot(float(dn),0.,0)
      call plot(float(dn),dzero,izero)
C
C     SET FONT TYPE
C
      fonttype(1:1) = '|'
      fonttype(2:2) = char(font+48)

      fontlen = 2

c..aap      call syms(x,y,height,%ref(fonttype),0.,fontlen,-1)
      call syms(x,y,height,%ref(fonttype),dzero,fontlen,ineg1)
C
C     WRITE TEXT TO RAMTEK
C
c..aap      call syms(x,y,height,%ref(string),0.,length,centerflag)
      call syms(x,y,height,%ref(string),dzero,length,centerflag)
C
C     A FINAL CALL TO PLOT TO MOVE THE CURSOR LOCATION WITH PEN UP.  THIS
C     MAKES SURE THE FINAL VECTOR HAS BEEN WRITTEN TO COMPLETE THE STRING.
C
c..aap      call plot(x,y,3)
      call plot(x,y,ithree)

      return
      end


*.
*   ParseTabs - find fields delimited by tabs in a text string
*
*   Args
*	InLine		C**	input	input string
*	MaxField	I	input	maximum number of fields to find
*	NumField	I	output	number of fields found
*	Field 		C**	output	array of fields
*	FieldLen	I	output	array of field lengths
*
*	04/30/90 Fixed problem if actual field length greater than size of Field
*	08/25/88
*
*	04/30/90 12:42
*..
	subroutine ParseTabs(InLine,MaxField,NumField,Field,FieldLen)
	character*(*) InLine
	integer MaxField,NumField
	character*(*) Field(*)
	integer FieldLen(*)

	integer ic,ip,maxip,ifield
	character tab,c

	  character*100 SccsFileID
     -/'@(#)parsetabs.for	5.1 98/01/08 APS/ASF\0'/
** Data
	data tab /9/

	integer slen

	slen = len(InLine)

	do ifield=1,MaxField
	    Field(ifield) = ' '
	    FieldLen(ifield) = 0
	end do

	NumField = 0
	ifield = 1
	ip = 0
	maxip = len(Field(1))

	do ic=1,slen

	    c = InLine(ic:ic)

	    if(c.ne.tab) then
		ip = ip + 1
		if(ip.le.maxip) then
		    Field(ifield)(ip:ip) = c
		    FieldLen(ifield) = ip
		else
		    ip = maxip
		end if
	    else
		ifield = ifield + 1
		if(ifield.gt.MaxField) then
		    NumField = MaxField
		    return
		end if
		ip = 0
	    end if

	end do

	NumField = ifield

	end

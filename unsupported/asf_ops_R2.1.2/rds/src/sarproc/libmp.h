*	RAPID Fortran library include file

*   Parameter definitions

	integer*4 SUCCESS, INFO, WARNING, ERROR, FATAL
	integer*4 DEFAULT, NULL, NEVWDS
	integer*4 SIZEOFGCA
	integer*4 NULLINTRAY
	real*4    UNDEF

	parameter (SUCCESS= 0, INFO= -1, WARNING= -2, ERROR= -3, FATAL= -4)
	parameter (DEFAULT=1, NULL=0, NEVWDS=2)
	parameter (SIZEOFGCA=1000)
	parameter (NULLINTRAY=-2)
	parameter (UNDEF=-.11e39)

      character*80 sccsid_libmp

      data sccsid_libmp /'@(#)libmp.h	1.4 96/04/09 22:44:15\0'/

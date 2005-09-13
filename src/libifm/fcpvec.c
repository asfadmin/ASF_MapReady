/****************************************************************
FUNCTION NAME: fcpvec()

SYNTAX:

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------

DESCRIPTION:

   Copy one floating point vector to another.

RETURN VALUE:
   1 - if success
   0 - if failure

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
   1.0 - Mike Shindle - Original Development
****************************************************************/

int fcpvec(float *from, float *to, int nitems)
{
   int i;

   if (nitems < 0) return 0;
   for (i = 0; i < nitems; i++) 
      to[i] = from[i];
   return (1);
}

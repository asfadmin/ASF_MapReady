static char sccsid_odl_c[] = "@(#)odl.c	1.7 96/10/25 10:47:29";

#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>

#include "Msg.h"
#include "Obj.h"
#include "Label.h"
#include "Group.h"
#include "Object.h"

#include "Ident.h"
#include "String.h"
#include "Symbol.h"
#include "Int.h"
#include "UnsignedInt.h"
#include "Double.h"
#include "Time.h"
#include "Unit.h"

#include "Array.h"
#include "Range.h"
#include "Set.h"
#include "Seq.h"
#include "Seq2.h"



#include "odl.h"

int makeUnsignedInt(ODL odl)
{
  if (odl != NULL) {
/****
printf("odl->send = %x, Int %x, Unsigned %x\n", 
    ((Obj_t*) odl)->send, Int, UnsignedInt);
fflush(stdout);
****/
    ((Obj_t*) odl)->send = (Method_t) UnsignedInt;
  }

  return(1);
}


#ifdef  NOSTDARG
int setObjectVal (va_alist)
va_dcl
{
    GMT_t gmt;
    struct timeval *tvp;
    Method_t type;
    ODL odl;
    char* name;
    va_list  args;
    va_start(args);
    odl  = va_arg(args, ODL);
    name = va_arg(args, char*);

#else
int setObjectVal (ODL odl, char* name, ...)
{
    GMT_t gmt;
    Method_t type;
    struct timeval *tvp;
    va_list  args;
    va_start(args, name);
#endif
/*printf("setObjectVal: %s\n", name); */
/*
    odl = Value(((ODL*)Val(odl))[0]);
*/
    odl = Value(odl);
/*printf("setObjectVal: modified odl %s\n", ODLToStr(odl, NULL) ); */
    odl = Lookup(odl, name);

    if (Type(Value(odl)) == (Method_t) Time) {
        tvp = va_arg(args, struct timeval*);
        localtime_r((time_t*) &tvp->tv_sec, &gmt.tm);
        gmt.tv_usec = tvp->tv_usec;
        return (SetVal(odl, &gmt) ? 1 : 0);
    }
    return (odl != NULL && (*Type(odl))(_Relay, _SetVal, odl, args) ? 1 : 0);
}



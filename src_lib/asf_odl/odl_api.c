/*=============================================================================
 |  @(#)odl_api.c	1.16 96/10/31 11:25:23
 |
 |  Object Description Language (ODL) Application Interface.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#include "asf.h"
#include <errno.h>
#include <fcntl.h>
#include <syslog.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>

#include "Parser.h"

#include "Msg.h"
#include "Obj.h"
#include "Label.h"
#include "Group.h"
#include "Object.h"

#include "Ident.h"
#include "odl_String.h"
#include "Symbol.h"
#include "Int.h"
#include "UnsignedInt.h"
#include "Double.h"
#include "odl_Time.h"
#include "Unit.h"

#include "Array.h"
#include "Range.h"
#include "Set.h"
#include "Seq.h"
#include "Seq2.h"

#include "odl.h"


#define mkTime mktime

#ifdef	_NO_PROTO
int ODLinit()

#else
int ODLinit(void)
#endif
{
	return 0;
}

#ifdef	_NO_PROTO
void ODLFree (odl)
void* odl;
#else

void ODLFree (void* odl)
#endif
{
#ifdef DEBUG
    static Method_t classes[] = {
	(Method_t) Array, 
	(Method_t) Label, 
	(Method_t) Object, 
	(Method_t) Group, 
	(Method_t) Ident, 
	(Method_t) String, 
	(Method_t) Int, 
	(Method_t) Double, 
	(Method_t) Time,
	(Method_t) Seq, 
	(Method_t) Set, 
	(Method_t) Seq2, 
	(Method_t) Range, 
	(Method_t) Unit, 
	(Method_t) Symbol, 
	(Method_t) Obj,
	(Method_t) UnsignedInt
    };
    int i;
    Method_t type;
    if (! odl) return;
    type = Type(odl);
    for (i = 0; i < sizeof(classes) / sizeof(Method_t); ++i)
	if (type == classes[i]) {
	    Destroy(odl);
	    break;
	}
#else
    if (! odl) return;
    if (Type(odl) == (Method_t) Array)
	Destroy(odl);
#endif
    free(odl);
}


#ifdef	_NO_PROTO
char* ODLToStr (odl, name)
ODL odl;
char* name;

#else
char* ODLToStr (ODL odl, char* name)
#endif
{
    String_t s;
    size_t n;
    char *p;
    
    if (name != NULL)
	odl = Lookup(odl, name);

    if (! Image(odl, String(_Init, &s,"",0), 0))
	return NULL;

    n = Len(&s);
    p = (char*) MALLOC(n+6);
    memcpy(p, Val(&s), n);
    memcpy(p+n, "END\n", 5);
    Destroy(&s);
    return p;
}


#ifdef	_NO_PROTO
ODL StrToODL (str, len)
char* str;
size_t len;

#else
ODL StrToODL (char* str, size_t len)
#endif
{
    char err[256];
    ODL  odl = (ODL) ODLparse(str, len, err);
    return odl;
}


#ifdef	_NO_PROTO
ODL ODLcopy(odl)
ODL odl;

#else
ODL ODLcopy(ODL odl)
#endif
{
    char err[256];
    ODL  new;
    String_t s;

    if (!odl || !Image(odl, String(_Init, &s,"",0), 0))
	return NULL;
    new = (ODL) ODLparse(Val(&s), Len(&s), err);
    Destroy(&s);
    return new;
}


#ifdef	_NO_PROTO
int ODLGetVal (odl, name, val)
ODL odl;
char* name;
void *val;

#else
int ODLGetVal (ODL odl, char* name, void* val)
#endif
{
    GMT_t gmt;
    Method_t type;
    odl = Value(Lookup(odl, name));
    type = Type(odl);

    if (type == (Method_t) Int) {
        *(int*) val = *(int*) Val(odl);
        return 1;
    }
    if (type == (Method_t) Double) {
        *(double*) val = *(double*) Val(odl);
        return 1;
    }
    if (type == (Method_t) String ||
        type == (Method_t) Ident ||
        type == (Method_t) Symbol) {
        *(char**) val = (char*) Val(odl);
        return 1;
    }
    if (type == (Method_t) Time) {
        gmt = *(GMT_t*) Val(odl);
        ((struct timeval*) val)->tv_sec = mkTime((struct tm*) &gmt);
        ((struct timeval*) val)->tv_usec = gmt.tv_usec;
        return 1;
    }
    if (type == (Method_t) Array ||
        type == (Method_t) Seq ||
        type == (Method_t) Set ||
        type == (Method_t) Seq2) {
        *(Obj_t***) val = (Obj_t**) Val(odl);
        return 1;
    }
    return 0;
}




#ifdef	_NO_PROTO
int ODLGetInt (odl, name, errPtr)
ODL odl;
char* name;
int *errPtr;

#else
int ODLGetInt (ODL odl, char* name, int* errPtr)
#endif
{
    *errPtr = 0;
    odl = Value(Lookup(odl, name));

    if (Type(odl) != (Method_t) Int) {
	*errPtr = -1;
	return 0;
    }
    return (*(int*) Val(odl));
}

#ifdef	_NO_PROTO
unsigned int ODLGetUnsignedInt (odl, name, errPtr)
ODL odl;
char* name;
int *errPtr;

#else
unsigned int ODLGetUnsignedInt (ODL odl, char* name, int* errPtr)
#endif
{
    *errPtr = 0;
    odl = Value(Lookup(odl, name));

    if (Type(odl) != (Method_t) UnsignedInt) {
	*errPtr = -1;
	return 0;
    }
    return (*(unsigned int*) Val(odl));
}

#ifdef	_NO_PROTO
double ODLGetDouble (odl, name, errPtr)
ODL odl;
char* name;
int *errPtr;

#else
double ODLGetDouble (ODL odl, char* name, int* errPtr)
#endif
{
    *errPtr = 0;
    odl = Value(Lookup(odl, name));

    if (Type(odl) != (Method_t) Double) {
	*errPtr = -1;
	return 0;
    }
    return (*(double*) Val(odl));
}


#ifdef	_NO_PROTO
char* ODLGetString (odl, name, errPtr)
ODL odl;
char* name;
int *errPtr;

#else
char* ODLGetString (ODL odl, char* name, int* errPtr)
#endif
{
    size_t n;
    char *p;
    Method_t type;
    *errPtr = 0;
    odl = Value(Lookup(odl, name));
    type = Type(odl);

    if (type == (Method_t) String ||
	type == (Method_t) Ident ||
	type == (Method_t) Symbol)
    {
    	p = (char*) MALLOC(n = (size_t) Len(odl)+1);
	return (char*) memcpy(p, Val(odl), n);
    }
    *errPtr = -1;
    return NULL;
}


#ifdef  _NO_PROTO
char* ODLGetStr(odl, name)
ODL   odl;
char* name;
#else

char* ODLGetStr(ODL odl, char* name)
#endif
{
    Method_t type;
    odl = Value(Lookup(odl, name));
    type = Type(odl);
    return (type == (Method_t) String || type == (Method_t) Ident ||
	    type == (Method_t) Symbol)
	? Val(odl)
	: NULL;
}


#ifdef	_NO_PROTO
char* ODLGetTime (odl, name, errPtr)
ODL odl;
char* name;
int *errPtr;

#else
char* ODLGetTime (ODL odl, char* name, int* errPtr)
#endif
{
    String_t s;
    char* tmbuf;

    *errPtr = 0;
    odl = Value(Lookup(odl, name));
    if (Type(odl) != (Method_t) Time || (tmbuf = (char*) MALLOC(48)) == NULL) {
	*errPtr = -1;
	return 0;
    }
    strcpy(tmbuf, Val(Image(odl, String(_Init, &s, "",0), 0)));
    Destroy(&s);
    return tmbuf;
}


#ifdef	_NO_PROTO
double* ODLGetArrayDouble(odl, name, retval, p_row, p_col)
ODL   odl;
char* name;
double* retval;
int* p_row;
int* p_col;
#else

double* ODLGetArrayDouble(ODL odl, char* name, double *retval,
			  int* p_row, int* p_col)
#endif
{
    int  i, k, n_row = 1, n_col;
    double *v = retval;
    ODL *vectr, *elems, value_odl = Value(Lookup(odl, name));
    Method_t odl_type = Type(value_odl);

    if (odl_type == (Method_t) Seq2) {
	vectr = Val(value_odl);
	n_row = Len(value_odl);
	n_col = Len(vectr[0]);

	if ((!n_col) || (v && (*p_row < n_row || *p_col < n_col)) ||
	    (!v && !(v = (double*) MALLOC(n_row*n_col*sizeof(double))) )) {
	    *p_row = n_row;
	    *p_col = n_col;
	    return NULL;
	}
	*p_row = n_row;
	*p_col = n_col;

	for (i = 0; i < n_row; ++i) {
	    elems = Val(vectr[i]);
	    for (k = 0; k < n_col && elems[k]; ++k) {
		odl_type = Type(elems[k]);

		if (odl_type == (Method_t) Double)
		    *v++ = *(double*) Val(elems[k]);
		else if (odl_type == (Method_t) Int)
		    *v++ = (double) (*(int*) Val(elems[k]));
		else {
		    if (!retval) free(v);
		    return NULL;
		}
	    }	
	    if (k != n_col) {
		if (!retval) free(v);
		return NULL;
	    }
	}
    }
    else if (odl_type == (Method_t) Seq) {
	elems = Val(value_odl);
	n_col = Len(value_odl);

	if ((!n_col) || (v && (*p_row < 1 || *p_col < n_col)) ||
	    (!v && !(v = (double*) MALLOC(n_col*sizeof(double))) )) {
	    *p_row = 1;
	    *p_col = n_col;
	    return NULL;
	}
	*p_row = 1;
	*p_col = n_col;

	for (k = 0; k < n_col; ++k) {
	    odl_type = Type(elems[k]);

	    if (odl_type == (Method_t) Double)
		*v++ = *(double*) Val(elems[k]);
	    else if (odl_type == (Method_t) Int)
		*v++ = (double) (*(int*) Val(elems[k]));
	    else {
		if (!retval) free(v);
		return NULL;
	    }	
	}
    }
    else {
	*p_row = 0;
	*p_col = 0;
	return NULL;
    }
    return (v-(n_row*n_col));
}


#ifdef	_NO_PROTO
int* ODLGetArrayInt(odl, name, retval, p_row, p_col)
ODL   odl;
char* name;
int* retval;
int* p_row;
int* p_col;
#else

int* ODLGetArrayInt(ODL odl, char* name, int *retval, int* p_row, int* p_col)
#endif
{
    int  i, k, n_row = 1, n_col;
    int *v = retval;
    ODL *vectr, *elems, value_odl = Value(Lookup(odl, name));
    Method_t odl_type = Type(value_odl);

    if (odl_type == (Method_t) Seq2) {
	vectr = Val(value_odl);
	n_row = Len(value_odl);
	n_col = Len(vectr[0]);

	if ((!n_col) || (v && (*p_row < n_row || *p_col < n_col)) ||
	    (!v && !(v = (int*) MALLOC(n_row*n_col*sizeof(int))) )) {
	    *p_row = n_row;
	    *p_col = n_col;
	    return NULL;
	}
	*p_row = n_row;
	*p_col = n_col;

	for (i = 0; i < n_row; ++i) {
	    elems = Val(vectr[i]);
	    for (k = 0; k < n_col && elems[k]; ++k) {
		if (Type(elems[k]) == (Method_t) Int)
		    *v++ = *(int*) Val(elems[k]);
		else {
		    if (!retval) free(v);
		    return NULL;
		}
	    }	
	    if (k != n_col) {
		if (!retval) free(v);
		return NULL;
	    }
	}
    }
    else if (odl_type == (Method_t) Seq) {
	elems = Val(value_odl);
	n_col = Len(value_odl);


	if ((!n_col) || (v && (*p_row < 1 || *p_col < n_col)) ||
	    (!v && !(v = (int*) MALLOC(n_col*sizeof(int))) )) {
	    *p_row = 1;
	    *p_col = n_col;
	    return NULL;
	}
	*p_row = 1;
	*p_col = n_col;

	for (k = 0; k < n_col; ++k) {
	    if (Type(elems[k]) == (Method_t) Int)
		*v++ = *(int*) Val(elems[k]);
	    else {
		if (!retval) free(v);
		return NULL;
	    }	
	}
    }
    else {
	*p_row = 0;
	*p_col = 0;
	return NULL;
    }
    return (v-(n_row*n_col));
}


#ifdef  _NO_PROTO
int ODLGetStringArray (odl, name, ap)
ODL odl;
char* name;
char*** ap;
#else

int ODLGetStringArray (ODL odl, char* name, char*** ap)
#endif
{
    Method_t type;
    ODL* array;
    register char** sp, *bp;
    register int i, nb = 0, ns = 0;;

    odl = Value(Lookup(odl, name));
    type = Type(odl);

    if (type != (Method_t) Seq &&
        type != (Method_t) Set) {
        return -1;
    }
    /*  Figure out how much space we need so we can free() in one shot. */

    array = Val(odl);
    for (i = 0; array[i] != NULL; ++i) {
        if (Type(array[i]) != (Method_t) String) {
            return -1;
        }

        ns++;
        nb += strlen(Val(array[i])) + 1;
    }
    if ((sp = (char**) MALLOC((ns + 1)*sizeof(char*) + nb)) == NULL) {
        return -1;
    }
    /*  Set up array of pointers to point to the corresponding strings */

    bp = (char*) (sp + ns + 1);
    for (i = 0; i < ns; ++i) {
        char *ap = Val(array[i]);
        memcpy(sp[i] = bp, ap, nb = strlen(ap) + 1);
        bp += nb;
    }
    sp[i] = NULL;
    *ap = sp;

    return ns;
}


#ifdef	NOSTDARG
int ODLSetVal (va_alist)
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
int ODLSetVal (ODL odl, char* name, ...)
{
    GMT_t gmt;
    Method_t type;
    struct timeval *tvp;
    va_list  args;
    va_start(args, name);
#endif
    odl = (ODL) Lookup(odl, name);
    type = Type(Value(odl));

    if (type == (Method_t) Int)
	return (SetVal(odl, va_arg(args, int)) ? 1 : 0);

    if (type == (Method_t) Double)
	return (SetVal(odl, va_arg(args, double)) ? 1 : 0);

    if (type == (Method_t) String ||
	type == (Method_t) Ident ||
	type == (Method_t) Symbol)
	return (SetVal(odl, va_arg(args, char*)) ? 1 : 0);

    if (type == (Method_t) Time) {
	tvp = va_arg(args, struct timeval*);

	gmt.tm=*localtime((time_t*) &tvp->tv_sec);

	gmt.tv_usec = tvp->tv_usec;
	return (SetVal(odl, &gmt) ? 1 : 0);
    }
    if (type == (Method_t) Array ||
	type == (Method_t) Seq   ||
	type == (Method_t) Set   ||
	type == (Method_t) Seq2)
	return (SetVal(odl, va_arg(args, ODL*)) ? 1 : 0);
    return 0;
}


#ifdef	_NO_PROTO
int ODLSetInt (odl, name, val, unit)
ODL odl;
char* name;
int val;
char* unit;

#else
int ODLSetInt (ODL odl, char* name, int val, char* unit)
#endif
{
    odl = (ODL) Lookup(odl, name);
    return (Type(Value(odl)) == (Method_t) Int ? (int) SetVal(odl, val) : 0);
}


#ifdef	_NO_PROTO
int ODLSetUnsignedInt (odl, name, val, unit)
ODL odl;
char* name;
unsigned int val;
char* unit;

#else
int ODLSetUnsignedInt (ODL odl, char* name, unsigned int val, char* unit)
#endif
{
    odl = (ODL) Lookup(odl, name);
    return (Type(Value(odl)) == (Method_t) UnsignedInt ? (int) SetVal(odl, val) : 0);
}


#ifdef	_NO_PROTO
int ODLSetDouble (odl, name, val, unit)
ODL odl;
char* name;
double val;
char* unit;

#else
int ODLSetDouble (ODL odl, char* name, double val, char* unit)
#endif
{
    odl = (ODL) Lookup(odl, name);
    return (Type(Value(odl)) == (Method_t) Double ? (int) SetVal(odl, val) : 0);
}


#ifdef	_NO_PROTO
int ODLSetString(odl, name, val)
ODL odl;
char* name;
char* val;

#else
int ODLSetString (ODL odl, char* name, char* val)
#endif
{
    Method_t type;
    odl = (ODL) Lookup(odl, name);
    type = Type(Value(odl));

    if (type == (Method_t) String ||
	type == (Method_t) Ident  ||
	type == (Method_t) Symbol)
	return (int) SetVal(odl, val);
    return 0;
}


#ifdef	_NO_PROTO
int m_init()

#else
int m_init(void)
#endif
{
	return 0;
}


#ifdef	_NO_PROTO
void m_free(ptr)
void* ptr;

#else
void m_free(void* ptr)
#endif
{
    free(ptr);
}


#ifdef	_NO_PROTO
void* m_alloc(size, ptr)
size_t size;
void* ptr;

#else
void* m_alloc(size_t size, void *ptr)
#endif
{
    return (void*) MALLOC(size);
}

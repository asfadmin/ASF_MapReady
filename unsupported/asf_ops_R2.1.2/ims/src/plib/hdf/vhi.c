/***************************************************************************
*
*
*                         NCSA HDF version 3.3r1
*                            September 20, 1993
*
* NCSA HDF Version 3.3 source code and documentation are in the public
* domain.  Specifically, we give to the public domain all rights for future
* licensing of the source code, all resale rights, and all publishing rights.
*
* We ask, but do not require, that the following message be included in all
* derived works:
*
* Portions developed at the National Center for Supercomputing Applications at
* the University of Illinois at Urbana-Champaign, in collaboration with the
* Information Technology Institute of Singapore.
*
* THE UNIVERSITY OF ILLINOIS GIVES NO WARRANTY, EXPRESSED OR IMPLIED, FOR THE
* SOFTWARE AND/OR DOCUMENTATION PROVIDED, INCLUDING, WITHOUT LIMITATION,
* WARRANTY OF MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE
*
*****************************************************************************/

#include "vg.h"

/* ------------------------ VHstoredata ------------------------------- */
/*
Stores 'n' elements of data from 'buf' as a field 'field' in a new vdata 
called 'vsname' into the already opened HDF file (with pointer 'f').

The datatype variable must be specified as a valid HDF type

NOTES:
n should not be zero or negative.
RETURN:
      -1 if error.
      ref of that new vdata (a +ve integer) if successful.
*/

#ifdef PROTOTYPE
PUBLIC int32 VHstoredata (HFILEID f, char *field, uint8 buf[], int32 n, int32 datatype, char *vsname, char *vsclass)
#else

PUBLIC int32 VHstoredata (f, field, buf, n, datatype, vsname, vsclass)
	HFILEID 	f;
	char * 	field;
    uint8        buf[];
	int32 	n;
	char * 	vsname, * vsclass;
	int32 	datatype;
#endif

{
  int32	ref;
  int32 order = 1;
  char * FUNC = "VHstoredata";
  
  ref = (int32) VHstoredatam (f, field, buf, n, datatype, vsname, vsclass, order);
  return (ref);
}

/* ----------------------- VHstoredatam ---------------------------- */
/*
Same as VHstoredata but allows aggregate-typed field.
Stores 'n' elements of data from 'buf' as a
field 'field' in a new vdata called 'vsname' into the
already opened HDF file (with pointer 'f').

The datatype variable must be specified as a valid HDF type

NOTES:
n should not be zero or negative.
RETURN:
      -1 if error.
      ref of that new vdata (a +ve integer) if successful.
*/

#ifdef PROTOTYPE
PUBLIC int32 VHstoredatam (HFILEID f, char *field, uint8 buf[], int32  n, int32 datatype, char *vsname, char *vsclass, int32 order)
#else
PUBLIC int32 VHstoredatam (f, field, buf, n, datatype, vsname, vsclass, order)
HFILEID     f;
char    *   field;
uint8        buf[];
int32   n;
int32   datatype;
int32   order;
char *  vsname, * vsclass;
#endif
{
    int32 s;
    int32 ref;
#ifdef OLD_WAY
    VDATA * vs;
#else
    int32 vs;
#endif
    char * FUNC = "VHstoredatam";
    
    vs = VSattach (f,-1,"w");
    if (vs == FAIL)
        return (FAIL);

    s = VSfdefine (vs, field, datatype, order);
    if (s == -1)
        return (FAIL);

    s = VSsetfields (vs,field);
    if (s == -1)
        return (FAIL);

    s = VSwrite (vs, buf, n, FULL_INTERLACE);
    if (n != s)
        return (FAIL);

    VSsetname (vs, vsname);
    VSsetclass(vs, vsclass);
    ref = VSQueryref(vs);
    VSdetach(vs);

    return ((int32) ref);

} /* VHstoredatam */

/* ------------------------ VHmakegroup ------------------------------- */
/* 
Takes an array of tags and and array of refs and create a vgroup to
store them. You tell it how many tag/ref pairs there are. You must 
also give the vgroup a name.
NOTE:
Does bot check if a tag/ref is valid or exist, but ALL tag/ref 
pairs MUST be unique.
Creating EMPTY vgroups is allowed.

RETURN: 
      -1 if error
      ref of the new vgroup (a +ve integre) if ok.
*/
#ifdef PROTOTYPE
PUBLIC int32 VHmakegroup (HFILEID f, int32 tagarray[], int32 refarray[] , int32 n, char *vgname, char *vgclass)
#else
PUBLIC int32 VHmakegroup (f, tagarray, refarray , n, vgname, vgclass)
HFILEID     f;
int32   tagarray[], refarray[];
int32   n;
char        * vgname, * vgclass;
#endif
{
    int32 ref, i, s;

#ifdef OLD_WAY
    VGROUP *	vg;
#else
    int32 vg;
#endif

    char * FUNC = "VHmakegroup";

    vg = Vattach (f, -1, "w");
    if(vg == FAIL)
        return (FAIL);

    Vsetname (vg, vgname);
    Vsetclass(vg, vgclass);
    
    for (i=0; i<n; i++) {
        s = Vaddtagref (vg, tagarray[i], refarray[i]);
        if (s == -1)
            return (FAIL);
    }
    
    ref = VQueryref(vg);
    Vdetach(vg);

    return (ref);

} /* VHmakegroup */

/* ------------------------------------------------------------------ */

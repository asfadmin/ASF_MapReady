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

/*-----------------------------------------------------------------------------
 * File:    dfsd.h
 * Purpose: header file for the Scientific Data set
 * Invokes: dfrig.h
 * Contents:
 *  Structure definitions: DFSsdg
 *  Constant definitions: DFS_MAXLEN
 * Remarks: This is included with user programs which use SDG
 *          Currently defined to be 2-D.  Will later be increased to
 *          multiple dimensions
 *---------------------------------------------------------------------------*/

#ifndef DFSDG                       /* avoid re-inclusion */
#define DFSDG

#include "hdf.h"

#define DFS_MAXLEN 255		/*  Max length of label/unit/format strings */
#define DFSD_MAXFILL_LEN 16 /* Current max length for fill_value space */

/* include numbertype and aid for 3.2   S. Xu   */
/* structure to hold SDG info */
typedef struct DFSsdg {
    DFdi    data;               /* tag/ref of data in file */
    intn    rank;               /* number of dimensions */
    int32   _HUGE *dimsizes;    /* dimensions of data */
    char    _HUGE *coordsys;
    char    _HUGE *dataluf[3];  /* label/unit/format of data */
    char    _HUGE **dimluf[3];  /* label/unit/format for each dim */
    uint8   _HUGE **dimscales;  /* scales for each dimension */
    uint8   max_min[16];        /* max, min values of data, */
                                /*  currently atmost 8 bytes each   */
    int32   numbertype;         /* default is float32      */
    uint8   filenumsubclass;    /* number format in the file, default is IEEE */
    int32   aid;                /* access id     */
    int32   compression;        /* 0 -- not compressed  */
    int32   isndg;              /* 0 -- pure sdg, written by 3.1 else ndg */
    float64 cal, cal_err;       /* calibration multiplier stuff          */
    float64 ioff, ioff_err;     /* calibration offset stuff              */
    int32   cal_type;           /* number type of data after calibration */
    uint8   fill_value[DFSD_MAXFILL_LEN];   /* fill value if any specified  */
    bool fill_fixed;            /* whether ther fill value is a fixed value, or it can change */
} DFSsdg;

/* DFnsdgle is the internal structure which stores SDG or NDS and   */
/* related SDG in an HDF file.                                      */
/* It is a linked list.                                             */

typedef struct DFnsdgle   {
    DFdi nsdg;			/* NDG from 3.2 or SDG from 3.1  */
    DFdi sdg;			/* Only special NDF has values in this field */
    struct DFnsdgle *next;
}  DFnsdgle;

typedef struct DFnsdg_t_hdr	{
    uint32 size;
    DFnsdgle *nsdg_t;
}  DFnsdg_t_hdr;

extern int32 DFSDIopen
    PROTO((char _HUGE *filename, int access));

extern int DFSDIsdginfo
    PROTO((int32 file_id));

extern int DFSDIclear
    PROTO((DFSsdg _HUGE *sdg));

extern int DFSDIclearNT
    PROTO((DFSsdg _HUGE *sdg));

extern int DFSDIgetdata
    PROTO((char _HUGE *filename, intn rank, int32 _HUGE maxsizes[], VOIDP data,
                int isfortran));

extern int DFSDIputdata
    PROTO((char _HUGE *filename, intn rank, int32 _HUGE *dimsizes, VOIDP data,
                int accmode, int isfortran));

extern int DFSDIgetslice
    PROTO((char _HUGE *filename, int32 _HUGE winst[], int32 _HUGE windims[], VOIDP data,
                int32 _HUGE dims[], int isfortran));

extern int DFSDIputslice
    PROTO((int32 _HUGE windims[], VOIDP data, int32 _HUGE dims[], int isfortran));

extern int DFSDIendslice
    PROTO((int isfortran));

extern intn DFSDIrefresh
    PROTO((char _HUGE *filename));

extern int DFSDIisndg
    PROTO((intn _HUGE *isndg));

extern int DFSDIgetrrank
    PROTO((intn _HUGE *rank));

extern int DFSDIgetwrank
    PROTO((intn _HUGE *rank));

extern int DFSDIsetdimstrs
    PROTO((int dim, char _HUGE *label, char _HUGE *unit, char _HUGE *format));

extern int DFSDIsetdatastrs
    PROTO((char _HUGE *label, char _HUGE *unit, char _HUGE *format,
            char _HUGE *coordsys));

#endif /*DFSDG*/

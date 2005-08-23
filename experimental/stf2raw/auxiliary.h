/*Interface to 
-auxiliary-data decoding routines:
	auxUpdate: write all parameters we can find in the
auxiliary data to the given satellite structure.
	auxPrint: write all parameters to given file
	auxAGC_window:call updateAGC_window with satellite
AGC and window position for this line.

-satellite-frame input routines (*_readNextFrame)

-data unpacking routines:
	unpackBytes: convert the (bit-packed) downlinked bytes
	into (byte-aligned) I/Q samples.  These return a pointer
	to just past the end of the data they wrote.

*/

/*Open the given binary file, and make it s' current file*/
void openBinary(bin_state *s,const char *fName);

/*Seek to the given (0-based) frame number in the given file*/
void seekFrame(bin_state *s,int frameNo);

#include "missing.h"

/***************************************
Typedef's stolen straight from RANDI:
*/

#ifndef UNSIGNED_TYPES
#define UNSIGNED_TYPES
typedef unsigned long   Ulong;
typedef unsigned short  Ushort;
typedef unsigned int    Uint;
typedef unsigned char   Uchar;
#endif /*UNSIGNED_TYPES */

/* Structures and macros to access big-endian integers in a system-independent fashion*/
struct SAT_USHORT {
        Uchar Msb;
        Uchar Lsb;
        };
#define GET_SAT_USHORT(x) ( ((Ushort)((x).Msb) << 8) | ((Ushort)((x).Lsb)) )


struct SAT_ULONG {
        struct SAT_USHORT Msw;
        struct SAT_USHORT Lsw;
        };
#define GET_SAT_ULONG(x) ( ((Ulong)GET_SAT_USHORT((x).Msw) << 16) \
                                | ((Ulong)GET_SAT_USHORT((x).Lsw)) )


#include "aux_RSAT.h"
#include "aux_JRS.h"
#include "aux_ERS.h"






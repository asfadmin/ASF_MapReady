#ifndef _SYSDEF_H_
#define _SYSDEF_H_
/*
  Defines the valid systems
*/
#define NMR_SYSTEMS      5             /* Number of systems defined         */

#define SYS_IEEE_STD     0              /* IEEE standard (ref 754)           */
#define SYS_IEEE_LIL     1              /* IEEE standard (ref 754) little-endian */
#define SYS_IBM_MVS	     2		/* IBM MVS			     */
#define SYS_CRAY_UNICOS  3		/* Cray Y-MP Unicos                  */
#define SYS_OTHER_MSC	 4		/* Misc. Other systems not covered   */

/*
  Strings for the same systems as above
*/
#define IEEE    "ieee-std"
#define IEEE_LIL "ieee-lil"
#define MVS     "ibm-mvs"
#define UNICOS  "cray-unicos"
#define MSC     "other-msc"

/*
  Underflow and Overflow flags
*/
#define SYS_ERR         0       /* Error on underflow and overflow           */
#define SYS_SET         1       /* Set to min/max for system                 */
#define SYS_USE         2       /* Use specified underflow & overflow        */

/*
  System code conversions
*/
#define SETN    0       /* Convert system string to system numerical code    */
#define SETS    1       /* Convert system numerical code to system string    */
#endif

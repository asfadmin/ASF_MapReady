/****************************************************************************/
/* mdcodes.h -- status codes used with db_server...                         */
/****************************************************************************/

/*** Oracle SQL error codes...   ***/
#define SQL_NORMAL     0
#define SQL_ENDTAB     1403
#define SQL_CANCELLED  1013

/*** db_server error codes...  ***/
#define MD_NORMAL      0   /* Normal status */
#define MD_ENDTAB      1   /* End of Table reached */
#define MD_CANCEL      2   /* Query Cancelled */
#define MD_OPENSECTION 3   /* GET command successful */
#define MD_QUALUNK     4   /* Resulting logic unknown (used with SmartStar) */
#define MD_QBFEMP      5   /* Buffer empty  (used with SmartStar)  */
#define MD_INVARG      6   /* Invalid Argument (used with SmartStar) */
#define MD_INVDESCR    7   /* Invalid descriptor (used with SmartStar) */
#define MD_SYNTAX      8   /* General Syntax Error */

   /* Internal error codes... */
#define MD_INVNAME    -1   /* Invalid Name... */
#define MD_INVCHN     -2   /* Invalid channel  */
#define MD_NOPOS      -3   /* No Position -- used with SmartStar */
#define MD_NOCHAN     -4   /* No Channel */
#define MD_SMALLBUF   -5   /* Return buffer too small */
#define MD_NODATABASE -6   /* No Database  */
#define MD_NORESOURCE -7   /* No Resources */
#define MD_INTERR     -8   /* General Internal error */
#define MD_NOSQLLABEL -9   /* CLIENT BASED:  No SQL Label...  */

#define MD_INVSECTION -15  /* Invalid Section (with GET command) */
#define MD_NOFILE     -16  /* File not found/not generated  */
#define MD_OOPS       -999

   /* Client-Server error codes... */
#define MD_CONNECTERR    -10
#define MD_READERR       -11
#define MD_WRITERR       -12
#define MD_TIMEDOUT      -13

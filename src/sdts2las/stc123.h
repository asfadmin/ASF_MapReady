/***************************************************************************
*  The following constants are defined to control compilation in           *
*  several operating environments.  Only the constant referring to your    *
*  operating system should be set to '1' (one) all others should be        *
*  set to '0' (zero).                                                      *
***************************************************************************/
#define MSDOS 0    /* Microsoft DOS */
#define DG 1       /* Data General DG/UX */
#define MVS 0      /* Amdahl MVS */

/***************************************************************************
**
**    INVOCATION NAME: STC123.H
**
**    PURPOSE: TO DECLARE DATA STRUCTURES USED IN SELECTED FUNCTIONS
**
**    INVOCATION METHOD: #INCLUDE "STC123.H"
**
**    ARGUMENT LIST: NONE
**     
**    EXTERNAL FUNCTION REFERENCES: NONE
**
**    INTERNAL VARIABLES: NONE
**  
**    GLOBAL REFERENCES:
**
**     DATA DESCRIPTIVE FILE CONTROL RECORD STRUCTURE (CR)
**      NAME            TYPE   USE   DESCRIPTION
**      F_TITLE[]       PTR    I/O   FILE TITLE
**      TAG_L           PTR    I/O   POINTER TO LIST OF TAG PAIR RECORDS
**      U_AFD[]         PTR    I/O   CHARACTER STRING POINTER TO USERS 
**                                    AUGMENTED FILE DESCRIPTION
**      
**     DATA DESCRIPTIVE RECORD STRUCTURE (DD)
**      NAME            TYPE   USE   DESCRIPTION
**      TAG[10]         CHAR   I/O   INTERNAL NAME OF AN ASSOCIATED FIELD
**      FD_LEN          INT    I/O   LENGTH OF DATA DESCRIPTIVE AREA RECORD
**      FD_POS          INT    I/O   POSITION OF DATA DESCRIPTIVE AREA 
**                                    FIELD RECORD
**      FD_CNTRL[10]    CHAR   I/O   FIELD CONTROLS
**      NAME[]          PTR    I/O   CHARACTER STRING POINTER TO NAME
**      NUM_DIM         INT    I/O   NUMBER OF DIMENSIONS IN ARRAY DESCRIPTOR
**      DIM_LPTR        PTR    I/O   HEAD POINTER TO DIMENSIONS LENGTHS
**                                    (NO LABELS)
**      PRIM_DMS        INT    I/O   NUMBER OF ELEMENTS IN PRIMARY DIMENSION
**      LABELS          PTR    I/O   HEAD POINTER TO A LINKED LIST CONTAINING
**                                    LINKED LIST OF DD LABEL SUBFIELD
**                                    STRUCTURES
**      FMT_RT          PTR    I/O   ROOT POINTER TO FORMAT CONTROLS BINARY
**                                    TREE STRUCTURE
**      NEXT            PTR    I/O   POINTER TO NEXT DATA DESCRIPTIVE 
**                                    STRUCTURE
**
**     DATA DESCRIPTIVE LEADER STRUCTURE (DL)
**      NAME            TYPE   USE   DESCRIPTION
**      REC_LEN         INT    I/O   DATA DESCRIPTIVE RECORD LENGTH      
**      ILEVEL          INT    I/O   INTERCHANGE LEVEL
**      LEAD_ID         CHAR   I/O   LEADER IDENTIFIER 
**      ICE_IND         CHAR   I/O   INLINE CODE EXTENSION INDICATOR
**      RESV_SP         CHAR   I/O   RESERVE SPACE CHARACTER
**      APPL_IND        CHAR   I/O   APPLICATION INDICATOR
**      FD_CNTRL_L      INT    I/O   FIELD CONTROL LENGTH
**      DA_BASE         INT    I/O   BASE ADDRESS OF DESCRIPTIVE AREA 
**      CCS_IND[4]      CHAR   I/O   CODE CHARACTER SET INDICATOR
**      S_FDLEN         INT    I/O   SIZE OF FIELD LENGTH 
**      S_FDPOS         INT    I/O   SIZE OF FIELD POSITION  
**      S_RESV          INT    I/O   RESERVED DIGIT
**      S_TAG           INT    I/O   SIZE OF FIELD TAG 
**
**     DIMENSION LENGTHS STRUCTURE (DM)
**      NAME            TYPE   USE   DESCRIPTION
**      LEN             INT    I/O   DIMENSION LENGTH
**      NXT             PTR    I/O   POINTER TO NEXT DIMENSION LENGTH    
**
**     DATA RECORD STRUCTURE (DR)
**      NAME            TYPE   USE   DESCRIPTION
**      TAG[10]         CHAR   I/O   INTERNAL NAME OF AN ASSOCIATED FIELD
**      FD_LEN          INT    I/O   LENGTH OF DISCRIPTIVE AREA DATA RECORD 
**      FD_POS          INT    I/O   POSITION OF DESCRIPTIVE AREA DATA
**                                    RECORD
**      NUM_DIM         INT    I/O   NUMBER OF DIMENSIONS (NO LABELS)
**      DIM_LPTR        PTR    I/O   HEAD POINTER TO DIMENSION LENGTHS
**                                    (NO LABELS)
**      VALUES          PTR    I/O   HEAD POINTER TO DATA VALUE SUBFIELD
**                                    RECORDS
**      NEXT            PTR    I/O   POINTER TO NEXT DATA RECORD 
**
**     DR DATA VALUE SUBFIELD STRUCTURE (DV)
**      NAME            TYPE   USE   DESCRIPTION
**      VALUE[]         PTR    I/O   DATA VALUE
**      NXT_VSET        PTR    I/O   POINTER TO NEXT SET OF DATA VALUES
**      NXT_VAL         PTR    I/O   POINTER TO NEXT DATA VALUE SUBFIELD 
**                                    RECORD
**
**     FORMAT CONTROLS STRUCTURE (FC)
**      NAME            TYPE   USE   DESCRIPTION
**      REP_NO          INT    I/O   NUMBER OF REPETITIONS
**      D_TYPE          CHAR   I/O   DATA TYPE - (A,I,R,S,C,B,X)
**      WIDTH           INT    I/O   FIELD WIDTH SPECIFICATION
**      U_DELIM         CHAR   I/O   USER DELIMITER
**      LEFT            PTR    I/O   LEFT POINTER TO FORMAT CONTROLS STRUCTURE
**                                    INDICATES REPETITION
**      RIGHT           PTR    I/O   RIGHT POINTER TO FORMAT CONTROLS STRUCTURE
**                                    INDICATES SAME LEVEL
**      BACK            PTR    I/O   BACK POINTER TO FORMAT CONTROLS STRUCTURE
**
**     FILE MANAGEMENT STRUCTURE (FM)
**      NAME            TYPE   USE   DESCRIPTION
**      FP              PTR    I/O   FILE POINTER
**      F_NAME[]        PTR    I/O   EXTERNAL FILE NAME
**      OPEN_MODE       CHAR   I/O   OPEN MODE OF FILE
**      CR_HD           PTR    I/O   HEAD POINTER TO DATA DESCRIPTIVE FILE
**                                    CONTROL RECORD STRUCTURE
**      DD_HD           PTR    I/O   HEAD POINTER TO DATA DESCRIPTIVE
**                                    RECORD STRUCTURE
**      DL_HD           PTR    I/O   HEAD POINTER TO DATA DESCRIPTIVE LEADER
**                                    STRUCTURE
**      DR_HD           PTR    I/O   HEAD POINTER TO DATA RECORD STRUCTURE
**      LP_HD           PTR    I/O   HEAD POINTER TO LABELS POINTER STRUCTURE
**      RL_HD           PTR    I/O   HEAD POINTER TO DATA RECORD LEADER
**                                    STRUCTURE
**      RS_HD           PTR    I/O   HEAD POINTER TO FORMAT REPETITION STACK
**                                    STRUCTURE
**      REC_LOC_HD      PTR    I/O   HEAD POINTER TO RECORD POINTER STRUCTURE
**      CUR_DD          PTR    I/O   CURRENT POINTER TO DATA DESCRIPTIVE
**                                    RECORD STRUCTURE ENTRY
**      CUR_DM          PTR    I/O   CURRENT POINTER TO DIMENSION LENGTHS
**                                    STRUCTURE ENTRY
**      CUR_DR          PTR    I/O   CURRENT POINTER TO DATA RECORD STRUCTURE
**                                    ENTRY
**      CUR_DV          PTR    I/O   CURRENT POINTER TO DR DATA VALUE SUBFIELD
**                                    STRUCTURE ENTRY
**      ROW_DVH         PTR    I/O   CURRENT POINTER TO NEXT SET OF VALUES
**                                    IN DR DATA VALUE SUBFIELD STRUCTURE ENTRY
**      CUR_FC          PTR    I/O   CURRENT POINTER TO FORMAT CONTROLS
**                                    STRUCTURE ENTRY
**      CUR_LP          PTR    I/O   CURRENT POINTER TO LABELS POINTER
**                                    STRUCTURE ENTRY
**      CUR_SL          PTR    I/O   CURRENT POINTER TO DD-LABEL SUBFIELD
**                                    STRUCTURE ENTRY
**      CUR_FCR         PTR    I/O   CURRENT POINTER TO ROOT OF FORMAT
**                                    CONTROLS STRUCTURE
**      CUR_RP          PTR    I/O   CURRENT POINTER TO RECORD POINTER 
**                                    STRUCTURE
**      NLD_RP          PTR    I/O   POINTER TO RECORD POINTER STRUCTURE 
**                                    WHERE CORRESPONDING DR HAS AN 'R' 
**                                    LEADER IDENTIFIER    
**      SF_FMT          CHAR   I/O   FORMAT CORRESPONDING TO THE CURRENT
**                                    DATA VALUE
**      NLD             INT    I/O   NO LEADER FLAG
**                                    TRUE  - NO LEADER
**                                    FALSE - LEADER EXISTS
**      BIT_CNT         INT    I/O   COUNT OF BITS STORED IN MEMORY FOR 
**                                    A SUBSEQUENT READ FUNCTION
**      BIT_BIN         CHAR   I/O   BITS STORED IN MEMORY FOR SUBSEQUENT
**                                    READ FUNCTION
**      COMPRESS        INT    I/O   FLAG TO SPECIFY COMPRESSED OR
**                                    UNCOMPRESSED ADJACENT FIXED-LENGTH
**                                    BIT FIELD I/O
**                                    0 - UNCOMPRESSED
**                                    1 - COMPRESSED
**      SF_STATE_DD     INT    I/O   SUBFIELD STATE (DD)
**                                    1 - FIELD CONTROL SUBFIELD
**                                    2 - NAME SUBFIELD
**                                    3 - LABELS SUBFIELD
**                                    4 - FORMATS SUBFIELD
**                                    5 - FINISHED
**      SF_STATE_DR     INT    I/O   SUBFIELD STATE (DR)
**                                    1 - NUMBER OF DIMENSIONS SUBFIELD
**                                    2 - LENGTH OF A DIMENSION SUBFIELD
**                                    3 - DATA VALUE STRING SUBFIELD
**                                    4 - FINISHED
**      NEXT            PTR    I/O   POINTER TO NEXT FILE MANAGEMENT
**                                    STRUCTURE
**      BACK            PTR    I/O   POINTER TO PREVIOUS FILE MANAGEMENT
**                                    STRUCTURE
**
**     LABELS POINTER STRUCTURE (LP)
**      NAME            TYPE   USE   DESCRIPTION
**      NEXT            PTR    I/O   POINTER TO NEXT LP ENTRY
**      FIRST           PTR    I/O   FIRST LABEL (SL) IN THE LABEL SET
**      CUR             PTR    I/O   CURRENT LABEL (SL) IN THE LABEL SET
**
**     DATA RECORD LEADER STRUCTURE (RL)
**      NAME            TYPE   USE   DESCRIPTION
**      REC_LEN         INT    I/O   DATA RECORD LENGTH     
**      RESV_SP         CHAR   I/O   RESERVED SPACE CHARACTER
**      LEAD_ID         CHAR   I/O   LEADER IDENTIFIER
**      S_RESV[6]       CHAR   I/O   RESERVED SPACE
**      DA_BASE         INT    I/O   BASE ADDRESS OF DATA AREA  
**      R_SPACE[4]      CHAR   I/O   RESERVED SPACE CHARACTERS
**      S_FDLEN         INT    I/O   SIZE OF FIELD LENGTH  
**      S_FDPOS         INT    I/O   SIZE OF FIELD POSITION 
**      SP_RSRV         INT    I/O   RESERVED DIGIT
**      S_TAG           INT    I/O   SIZE OF FIELD TAG 
**
**     RECORD POINTER STRUCTURE (RP)
**      NAME            TYPE   USE   DESCRIPTION
**      DR_START        INT    I/O   FILE LOCATION OF FIRST BYTE OF DATA RECORD
**      NEXT            PTR    I/O   POINTER TO NEXT DR_START
**      BACK            PTR    I/O   POINTER TO BACK DR_START
**
**     FORMAT REPETITION STACK STRUCTURE (RS)
**      NAME            TYPE   USE   DESCRIPTION
**      REP_NUM         INT    I/O   NUMBER OF REPEATS
**      NXT             PTR    I/O   POINTER TO NEXT STACK VALUE
**
**     DD LABEL SUBFIELD STRUCTURE (SL)
**      NAME            TYPE   USE   DESCRIPTION
**      LABEL[]         PTR    I/O   LABEL
**      NXT_LAB         PTR    I/O   POINTER TO NEXT LABEL IN SET
**      NXT_LSET        PTR    I/O   POINTER TO NEXT SET OF LABELS
**         
**     DATA DESCRIPTIVE TAG PAIR STRUCTURE (TL) 
**      NAME            TYPE   USE   DESCRIPTION
**      TAG_1[10]       CHAR   I/O   INTERNAL NAME OF AN ASSOCIATED FIELD
**      TAG_2[10]       CHAR   I/O   INTERNAL NAME OF AN ASSOCIATED FIELD
**      NEXT            PTR    I/O   POINTER TO DATA DESCRIPTIVE TAG PAIR
**                                    STRUCTURE
**
**    GLOBAL VARIABLES:
**     NAME              TYPE   USE   DESCRIPTION
**     ASCII             INT    I/O   ASCII DATA CONVERSION FLAG
**     CUR_FM            PTR    I/O   CURRENT POINTER TO FILE MANAGEMENT
**                                     STRUCTURE ENTRY
**     EBCDIC            INT    I/O   EBCDIC DATA CONVERSION FLAG
**     FM_HD             PTR    I/O   HEAD POINTER TO LIST OF OPEN FILES
**     GLB_STR[MAXSIZ]   CHAR   I/O   GLOBAL CHARACTER STRING USED FOR
**                                     PROCESSING
**     GLB_STR2[MAXSIZ]  CHAR   I/O   SECOND GLOBAL CHARACTER STRING USED FOR
**                                     PROCESSING
**
**    GLOBAL CONSTANTS:
**     NAME          TYPE      DESCRIPTION
**     ARYD_DLM      CHAR      ARRAY DESCRIPTOR DELIMITER CHARACTER
**     ARYD_STR[2]   CHAR      CHARACTER STRING CONTAINING ARRAY DESCRIPTOR
**                              DELIMITER
**     BLNK_SP       CHAR      BLANK SPACE CHARACTER
**     BLNK_STR[2]   CHAR      CHARACTER STRING CONTAINING A BLANK SPACE
**     BYTE_S        INT       NUMBER OF BITS IN A BYTE
**     CDL           CHAR      CARTESIAN VECTOR LABEL DELIMITER * 2/10
**     CDL_STR[2]    CHAR      CHARACTER STRING CONTAINING THE CARTESIAN
**                              VECTOR LABEL DELIMITER 
**     CONV          INT       COMPILATION DIRECTIVE FLAG USED TO COMPILE
**                              CODE FOR ASCII/EBCDIC CONVERSION OF DATA
**     CVDL_STR[2]   CHAR      CHARACTER STRING CONTAINING THE CARTESIAN
**                              VECTOR LABEL DELIMITER AND THE VECTOR DATA
**                              ELEMENT DELIMITER
**     DB_DIGIT      INT       NUMBER OF DIGITS IN BASE ADDRESS OR DATA AREA
**     DEL_STR[3]    CHAR      CHARACTER STRING CONTAINING THE FIELD AND UNIT
**                              TERMINATORS
**     DG            INT       COMPILATION DIRECTIVE FLAG USED TO COMPILE
**                              CODE FOR ENVIRONMENT
**                              0 - NON DG    
**                              1 - DG    
**     FCDSTYPE      INT       RELATIVE POSITION OF THE DATA STRUCTURE TYPE 
**                              WITHIN THE FIELD CONTROL 
**     FCDTYPE       INT       RELATIVE POSITION OF THE DATA TYPE WITHIN THE
**                              FIELD CONTROL 
**     FP_LEN        INT       LENGTH OF FILE NAME INCLUDING THE PATH
**     FT            CHAR      FIELD TERMINATOR (RS) 1/14
**     FT_STR[2]     CHAR      CHARACTER STRING CONTAINING THE FIELD
**                              TERMINATOR
**     LEAD_LEN      INT       DATA DESCRIPTIVE AND DATA RECORDS LEADER
**                              LENGTH
**     NFTAG123      INT       NUMBER OF FIELD TAGS DEFINED BY FIPS123
**                              INTERCHANGE STANDARD
**     LID_POS       INT       POSITION OF LEADER ID
**     LVL23FCL      INT       FIELD CONTROL LENGTH FOR FILE INTERCHANGE LEVELS
**                              2 AND 3
**     MAXINT        INT       MAXIMUM VALUE OF A TWO-BYTE INTEGER
**     MAXREC        INT       MAXIMUM RECORD LENGTH
**     MAXSIZ        INT       MAXIMUM AMOUNT OF CONTIGUOUS MEMORY SPACE
**     MSDOS         INT       COMPILATION DIRECTIVE FLAG USED TO COMPILE
**                              CODE FOR ENVIRONMENT
**                              0 - NON MS-DOS
**                              1 - MS-DOS
**     MVS           INT       COMPILATION DIRECTIVE FLAG USED TO COMPILE
**                              CODE FOR ENVIRONMENT
**                              0 - NON MVS   
**                              1 - MVS   
**     NC            CHAR      NULL CHARACTER
**     RADIX         INT       RADIX USED FOR NUMBER CONVERSION
**     RES_SP[6]     CHAR      CHARACTER STRING OF RESERVED SPACE FIVE
**                              POSITIONS IN LENGTH
**     RES_3SP[4]    CHAR      CHARACTER STRING OF RESERVED SPACE THREE
**                              POSITIONS IN LENGTH
**     RL_DIGIT      INT       NUMBER OF DIGITS OR RECORD LENGTH
**     SEP           CHAR      SEPARATOR CHARACTER--ASCII DECIMAL CODE 255
**     SEP_STR[2]    CHAR      CHARACTER STRING CONTAINING SEPARATOR FOR
**                              ADJACENT DELIMITERS
**     UT            CHAR      UNIT TERMINATOR (US) 1/15
**     UT_STR[2]     CHAR      CHARACTER STRING CONTAINING THE UNIT
**                              TERMINATOR
**     VDL           CHAR      VECTOR DATA ELEMENT DELIMITER ! 2/1
**     VDL_STR[2]    CHAR      CHARACTER STRING CONTAINING THE VECTOR DATA
**                              ELEMENT DELIMITER
**
**    CHANGE HISTORY:
**     AUTHOR        CHANGE_ID     DATE    CHANGE SUMMARY
**     L. MCMILLION              05/14/90  INITIAL PROLOG
**     A. DEWITT                 05/21/90  INITIAL CODE
**     A. DEWITT                 02/28/90  ADD RECORD POINTER STRUCTURE
**                                          WHICH CONTAINS STARTING POS
**                                          OF A DATA RECORD
**     L. MCMILLION  TASK #40    08/13/91  REMOVED IO.H INCLUSION
**     L. MCMILLION  TASK #40    08/14/91  CHANGED PROTOTYPE FOR BEG123FILE(),
**                                          G123DSTR(), GET123DVAL(),
**                                          WR123FLD(), AND WR123SFLD(); CHAR
**                                          ARGUMENTS CHANGED TO INT
**     A. DEWITT     TASK #40    08/24/91  ADDED CH123SIZE() PROTOTYPE
**     L. MCMILLION  TASK #40    03/17/92  ADDED CONDITIONAL COMPILATION
**                                          DIRECTIVE FOR ENVIRONMENT TYPE
**     L. MCMILLION  92DR009     04/14/92  ADDED SEPARATOR FOR ADJACENT
**                                          DELIMITERS
**     J. TAYLOR     92DR005     05/14/92  ADDED VARIABLES FOR STORING
**                                          ADJACENT FIXED-LENGTH BIT 
**                                          SUBFIELDS TO STRUCTURE FM;
**                                          ALSO ADDED PROTOTYPES FOR NEW
**                                          FUNCTIONS G123BSTR() AND
**                                          LOAD123FLD() AND MODIFIED
**                                          AFFECTED FUNCTIONS PROTOTYPES
**     J. TAYLOR     92DR005     05/20/92  ADDED FLAG TO SPECIFY 
**                                          COMPRESSED ADJACENT FIXED-LENGTH
**                                          READS TO STRUCTURE FM
**     J. TAYLOR     92DR003     11/11/92  ADDED FUNCTION PROTOTYPE FOR 
**                                          VER123DRTAG()
**     L. MCMILLION  TASK #40    11/20/92  CHANGED TYPE OF CUR_FM->SF_FMT
**                                          FROM STRUCT FC * TO CHAR--THE
**                                          CHANGE IS LOCAL AS IT IS 
**                                          CURRENTLY NOT USED
**     J. TAYLOR     92DR003     12/11/92  ADDED FUNCTION PROTOTYPE FOR 
**                                          VER123DDTAG()
**     J. TAYLOR     92DR018     12/11/92  ADDED FUNCTION PROTOTYPE FOR 
**                                          G123ORDER(), L123TOS(), AND
**                                          S123TOL()
**     L. MCMILLION  92DR002     01/15/93  DEFINED NUMBER OF FIELD TAGS
**                   92DR006                USED BY FIPS123 INTERCHANGE
**                                          STANDARD
**     J. TAYLOR     TASK 55     01/19/93  DEFINED "CONV" CONDITIONAL
**                                          COMPILATION VARIABLE
**     J. TAYLOR     TASK 55     01/19/93  DEFINED ASCII/EBCDIC GLOBAL
**                                          CONVERSION FLAGS 
**     J. TAYLOR     TASK 55     01/19/93  ADDED FUNCTION PROTOTYPE FOR 
**                                          A123TOE(), E123TOA(), CONV123CAT(),
**                                          RET123DDLEAD(), RET123DDDIR(),
**                                          RET123DRLEAD(), AND RET123DRDIR()
**     J. TAYLOR     TASK 55     01/19/93  CHANGED VALUE DEFINED FOR FP_LEN
**                                          SO THAT TEMPORARY FILE NAMES LARGE
**                                          ENOUGH FOR MVS CAN BE CREATED
**     J. TAYLOR     TASK #55    01/20/93  ADDED CONDITIONAL COMPILATION
**                                          DIRECTIVE FOR "MVS" ENVIRONMENT TYPE
**     J. TAYLOR     TASK #55    02/02/93  CHANGED DEFINITION OF NC FOR 
**                                          PORTABILTY
**     J. TAYLOR     TASK #40    01/28/93  MOVED DEFINE OS CONSTANTS TO TOP OF
**                                          FILE AND ADDED COMMENTS FOR USERS
**     J. TAYLOR     TASK #40    01/28/93  ADDED "DG" OS CONSTANT FOR 
**                                          CONSISTANCY WITH EXISTING PLATFORM
**     J. TAYLOR     93DR023     04/22/93  CHANGED FUNCTION PROTOTYPES:
**                                          G123BSTR()
**                                          G123DSTR()
**                                          GET123DIM()
**                                          GET123DVAL()
**                                          LOAD123FLD()
**                                          RET123FV()
**     J. TAYLOR     93DR023     04/22/93  ADDED FUNCTION PROTOTYPE:
**                                          G123SSTR()
**     J. TAYLOR     93DR034     06/07/93  MODIFIED FUNCTION PROTOTYPE FOR
**                                          RET123FV() TO RECEIVE FIELD LENGTH
**     L. MCMILLION  93DR033     06/15/93  ADDED MEMBERS TO DD STRUCTURE AND
**                                          FUNCTION PROTOTYPES TO IMPLEMENT
**                                          ARRAY DESCRIPTOR IN DDR LABEL
**                                          SUBFIELD.
**
****************************************************************************
**    CODE SECTION
**
****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define CONV 0

#define BYTE_S 8L
#define DB_DIGIT 5L
#define FCDSTYPE 0
#define FCDTYPE 1
#define FP_LEN 100L
#define LEAD_LEN 24L
#define LID_POS 6L
#define LVL23FCL 6L
#define MAXINT 32767L
#define MAXREC 99999L
#define NFTAG123 10

#if MSDOS
#define MAXSIZ 64000L
#else
#define MAXSIZ 128000L
#endif

#define RADIX 10
#define RL_DIGIT 5L

#define ARYD_DLM ','
#define ARYD_STR ","
#define BLNK_SP ' '
#define BLNK_STR " " 
#define CDL '*'
#define CDL_STR "*"
#define RES_SP "     "
#define RES_3SP "   "
#define VDL '!'
#define VDL_STR "!"
#define CVDL_STR "*!"

#define NC (char)0

extern int  ASCII           ;
extern int  EBCDIC          ;
extern char FT              ;
extern char FT_STR[2]       ;
extern char glb_str[MAXSIZ] ;
extern char glb_str2[MAXSIZ];
extern char SEP             ;
extern char SEP_STR[2]      ;
extern char UT              ;
extern char UT_STR[2]       ;
extern char DEL_STR[3]      ;

extern struct fm *cur_fm;
extern struct fm *fm_hd ;

struct dl  {
            long  rec_len   ;
            long  ilevel    ;
            char  lead_id   ;
            char  ice_ind   ;
            char  resv_sp   ;
            char  appl_ind  ;
            long  fd_cntrl_l;
            long  da_base   ;
            char  ccs_ind[4];
            long  s_fdlen   ;
            long  s_fdpos   ;
            long  s_resv    ;
            long  s_tag     ;
           };

struct sl  {
            char       *label    ;
            struct sl  *nxt_lab  ;
            struct sl  *nxt_lset ;
           };
           
struct fc  {
            long       rep_no  ;
            char       d_type  ;
            long       width   ;
            char       u_delim ;
            struct fc  *left   ;
            struct fc  *right  ;
            struct fc  *back   ;
           };
           
struct dd  {
            char       tag[10]     ;
            long       fd_len      ;
            long       fd_pos      ;
            char       fd_cntrl[10];
            char       *name       ;
            long       num_dim     ;
            struct dm  *dim_lptr   ;
            long       prim_dms    ;
            struct sl  *labels     ;
            struct fc  *fmt_rt     ;
            struct dd  *next       ;
           };
           
struct tl  {
            char       tag_1[10];
            char       tag_2[10];
            struct tl  *next    ;
           };
           
struct cr  {
            char       *f_title ;
            struct tl  *tag_l   ;
            char       *u_afd   ;
           };
           
struct rs  {
            long       rep_num  ;
            struct rs  *nxt     ;
           };
           
struct rl  { 
            long rec_len   ;
            char resv_sp   ;
            char lead_id   ;
            char s_resv[6] ;
            long da_base   ;
            char r_space[4];
            long s_fdlen   ;
            long s_fdpos   ;
            long sp_rsrv   ;
            long s_tag     ;
           };
                  
struct dv  {
            char       *value     ;
            struct dv  *nxt_vset  ;           
            struct dv  *nxt_val   ;
           };
           
struct dm  {
            long       len  ;
            struct dm  *nxt ;
           };
           
struct dr  {
            char       tag[10]   ;
            long       fd_len    ;
            long       fd_pos    ;
            long       num_dim   ;
            struct dm  *dim_lptr ;
            struct dv  *values   ;
            struct dr  *next     ;
           };
           
struct lp  {
            struct lp  *next  ;
            struct sl  *first ;
            struct sl  *cur   ;
           };
           
struct rp  {
            long   dr_start   ;
            struct rp *next   ;
            struct rp *back   ;
           };
                
struct fm  {
            FILE       *fp          ;
            char       *f_name      ;                                         
            char       open_mode    ; 
            struct cr  *cr_hd       ;
            struct dd  *dd_hd       ;
            struct dl  *dl_hd       ;
            struct dr  *dr_hd       ;
            struct lp  *lp_hd       ;
            struct rl  *rl_hd       ;
            struct rs  *rs_hd       ;
            struct rp  *rec_loc_hd  ;
            struct dd  *cur_dd      ;
            struct dm  *cur_dm      ;
            struct dr  *cur_dr      ;
            struct dv  *cur_dv      ;
            struct dv  *row_dvh     ;
            struct fc  *cur_fc      ;
            struct lp  *cur_lp      ;
            struct sl  *cur_sl      ;
            struct fc  *cur_fcr     ;
            struct rp  *cur_rp      ;
            struct rp  *nld_rp      ;
            char       sf_fmt       ;
            long       nld          ; 
            int        bit_cnt      ;
            char       bit_bin      ;
            int        compress     ;
            int        sf_state_dd  ; 
            int        sf_state_dr  ;
            struct fm  *next        ;
            struct fm  *back        ;
           }; 

extern int a123toe(char *)                                                    ;
extern int bak123fld(FILE *, int *)                                           ;
extern int bak123rec(FILE *, int *)                                           ;
extern int bak123sfld(FILE *, int *)                                          ;
extern int beg123ddrec(FILE *)                                                ;
extern int beg123ddsfld(void)                                                 ;
extern int beg123file(char *, int, long *, char *, char *, FILE **)           ;
extern int beg123rec(FILE *)                                                  ;
extern int bld123adscr(char *)                                                ;
extern int bld123fmt(char *)                                                  ;
extern int bld123lab(char *)                                                  ;
extern int ch123size(FILE **, long)                                           ;
extern int chk123fld(FILE *, char *, char *, char **, char *, char *, char *) ;
extern int chk123nfld(FILE *, char *, char *, char **, char *, char *, char *);
extern int chk123nrec(FILE *, long *, char *, char *)                         ;
extern int chk123nsfld(FILE *, char *, char *, char *)                        ;
extern int chk123rec(FILE *, long *, char *, char *)                          ;
extern int chk123sfld(FILE *, char *, char *, char *)                         ;
extern int cki123nfld(char **)                                                ;
extern int cmp123dddir(void)                                                  ;
extern int cmp123ddlead(void)                                                 ;
extern int cmp123drdir(void)                                                  ;
extern int cmp123drlead(void)                                                 ;
extern int conv123cat(char *, char *)                                         ;
extern struct dm * del123dim(struct dm *)                                     ;
extern int del123drsfld(void)                                                 ;
extern struct dv * del123drvals(struct dv *)                                  ;
extern struct fc * del123fmt(struct fc *)                                     ;
extern struct sl * del123labs(struct sl *)                                    ;
extern int end123ddrec(FILE *)                                                ;
extern int end123file(FILE **)                                                ;
extern int end123rec(FILE *)                                                  ;
extern int er123ddfld(FILE *, int *)                                          ;
extern int er123ddrec(FILE *, int *)                                          ;
extern int er123ddsfld(FILE *, int *)                                         ;
extern int e123toa(char *)                                                    ;
extern int free123lab(void)                                                   ;
extern int g123bstr(char **, int, char *, long *)                             ; 
extern int g123dstr(char **, char *, int)                                     ;
extern int g123fstr(FILE *, char *, long)                                     ;
extern int g123int(FILE *, long, long *)                                      ;
extern int g123order(int *)                                                   ;
extern int g123sstr(char **, char *, long)                                    ;
extern int g123str(FILE *, char *, long)                                      ;
extern int get123adscr(char *)                                                ;
extern int get123dim(char **, long *, long *)                                 ;
extern int get123dval(char **, char *, int, long *, int, int, char *)         ;
extern int get123fmt(char *, long *, char *)                                  ;
extern int get123level(FILE *)                                                ;
extern int i123toa(long, char *)                                              ;
extern int incre123lab(struct lp *, struct sl *, struct sl **)                ;
extern int is123adscr(char *)                                                 ;
extern int is123intgr(char *)                                                 ;
extern int l123tos(long, char *, int)                                         ;
extern int ld123ddrec(void)                                                   ;
extern int ld123rec(void)                                                     ;
extern int load123fld(char **, char *, int)                                   ; 
extern int load123fmt(char *)                                                 ;
extern int load123lab(char *)                                                 ;
extern int load123tagp(char *)                                                ;
extern int out123fmt(FILE *, struct fc *)                                     ;
extern int pop123rs(long *)                                                   ;
extern int push123rs(long)                                                    ;
extern int rd123ddfld(FILE *, char *, char *, int *)                          ;
extern int rd123ddrec(FILE *, char *, int *)                                  ;
extern int rd123ddsfld(FILE *, char *, char *, int *)                         ;
extern int rd123fld(FILE *, char *, char *, char *, long *, int *)            ;
extern int rd123rec(FILE *, char *, long *, int *)                            ;
extern int rd123sfld(FILE *, char *, char *, char *, long *, int *)           ;
extern int ret123dddir(char *)                                                ;
extern int ret123ddlead(char *)                                               ;
extern int ret123drdir(char *)                                                ;
extern int ret123drlead(char *)                                               ;
extern int ret123dv(char *, long)                                             ;
extern int ret123fv(char **, char *, long, long, int)                         ;
extern int ret123match(char *)                                                ;
extern int ret123pdm(long *)                                                  ;
extern int rt123pvfld(void)                                                   ;
extern int rt123pvsfld(void)                                                  ;
extern int s123tol(char *, long *, int)                                       ;
extern int setup123lb(void)                                                   ;
extern int set123stat(FILE *, int *)                                          ;
extern int stc123empty(void)                                                  ;
extern int stor123dv(char *, long)                                            ;
extern char * str123tok(char **, char *, long *)                              ;
extern int uld123ddrec(void)                                                  ;
extern int uld123rec(void)                                                    ;
extern int ver123ddtag(void)                                                  ;
extern int ver123drtag(void)                                                  ;
extern int wr123ddfld(FILE *, char *, char *, int)                            ;
extern int wr123ddrec(FILE *, char *, int *)                                  ;
extern int wr123ddsfld(FILE *, char *, char *, int)                           ;
extern int wr123fld(FILE *, char *, int, char *, long, int)                   ;
extern int w123int(FILE *, long, long)                                        ;
extern int wr123rec(FILE *, char *, long, int *)                              ;
extern int wr123sfld(FILE *, char *, int , char *, long, int)                 ;

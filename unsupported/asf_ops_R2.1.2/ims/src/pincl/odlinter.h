/*****************************************************************************
 
  Module:  odlinter.h

  Description:  This C-language include file contains prototypes for the
		functions used internally by the PDS Object Description
		Language (ODL) processing software.

  Author:  Marti DeMore, Jet Propulsion Laboratory

  Creation Date: 18 May 1991

  History:

  Creation - This include file was included in the Version 2.2 modification
  of the ODLC library.

****************************************************************************/

#ifndef ODLINTER
#define ODLINTER

#ifdef __STDC__			/* 1992/08/05 JWH changed from !SUN_UNIX */

#ifdef EOSDIS
   extern int yylineno; 
   extern int  ODLerror_count;  
   extern int  ODLwarning_count;  
   extern int  ODLlinenumber_flag; 
#endif

VALUE_DATA ODLConvertInteger  (char vtext[],
			       int vlength);
VALUE_DATA ODLConvertReal     (char vtext[],
			       int vlength);
VALUE_DATA ODLConvertSymbol   (char vtext[],
			       int vlength,
			       int vflag);
VALUE_DATA ODLConvertString   (char vtext[],
			       int vlength);
VALUE_DATA ODLConvertDate     (char vtext[],
			       int vlength);
VALUE_DATA ODLConvertTime     (char vtext[],
			       int vlength);
VALUE_DATA ODLConvertDateTime (char vtext[],
			       int vlength);
VALUE_DATA ODLConvertDate     (char vtext[],
			       int vlength);


void ODLExtractDate  (char *text,
		     VALUE_DATA *item);
void ODLExtractTime (char *text,
		     VALUE_DATA *item);


int ODLFormatInteger  (char stmt[],
		       VALUE_DATA *item);
int ODLFormatReal     (char stmt[],
		       VALUE_DATA *item);
int ODLFormatUnits    (char stmt[],
		       struct ODLUnits *units);
int ODLFormatSymbol   (char stmt[],
		       VALUE_DATA *item);
int ODLFormatDate     (char stmt[],
		       VALUE_DATA *item);
int ODLFormatTime     (char stmt[],
		       VALUE_DATA *item);
int ODLFormatDateTime (char stmt[],
		       VALUE_DATA *item);
int ODLFormatString   (char stmt[],
		       VALUE_DATA *item,
		       int *column,
		       int left_margin,
		       int right_margin,
		       int format_flag,
                       int is_pointer);
int ODLFormatComment  (char stmt[],
		       char comment[],
		       int left_margin,
		       int right_margin);


#ifdef EOSDIS
/* GMS: redirect errors to IK_Syslog instead */
#  include "IK_Syslog.h"
#  define ODLPrintError(warning)      \
{ \
      if (ODLlinenumber_flag) \
                IK_vSyslog(LOG_ERR, "ODL: Error at line %d: %s", yylineno,warning); \
      else \
                IK_vSyslog(LOG_ERR, "ODL: %s", warning); \
       ODLerror_count++; \
}

#  define ODLPrintWarning(warning) \
{ \
      if (ODLlinenumber_flag) \
                IK_vSyslog(LOG_NOTICE, "ODL: Warning at line %d: %s", yylineno,warning); \
      else \
                IK_vSyslog(LOG_NOTICE, "ODL: %s", warning); \
      ODLwarning_count++; \
		 }

#  define ODLPrintInfo(warning)       IK_vSyslog(LOG_INFO, "ODL: %s", warning)
#else
   void ODLPrintError   (char error_msg[]);
   void ODLPrintWarning (char warning[]);
   void ODLPrintInfo    (char info_message[]);
#endif
void ODLPrintStmt    (char statement[]);
void ODLWriteStmt    (FILE *output_file,
		      char statement[]);


void ODLBeginAggregate (AGGREGATE_KIND kind,
			VALUE_DATA *item);
void ODLEndAggregate   (AGGREGATE_KIND kind,
			VALUE_DATA *item);
void ODLBeginParameter (PARAMETER_KIND kind,
			VALUE_DATA *item);
void ODLMarkParameter  (VALUE_KIND kind);
void ODLStoreValue     (VALUE_DATA *item);
void ODLStoreUnits1    (VALUE_DATA *name);
void ODLStoreUnits2    (VALUE_DATA *name,
			VALUE_DATA *exponent);
void ODLMarkUnits      (int exponent_sign);
void ODLCheckSequence  (void);
void ODLCheckRange     (VALUE_DATA *low,
			VALUE_DATA *high);
int ODLEndLabel        (void);

#else

VALUE_DATA ODLConvertInteger  ();
VALUE_DATA ODLConvertReal     ();
VALUE_DATA ODLConvertSymbol   ();
VALUE_DATA ODLConvertString   ();
VALUE_DATA ODLConvertTime     ();
VALUE_DATA ODLConvertDateTime ();
VALUE_DATA ODLConvertDate     ();

void ODLExtractDate ();
void ODLExtractTime ();

int ODLFormatInteger  ();
int ODLFormatReal     ();
int ODLFormatUnits    ();
int ODLFormatSymbol   ();
int ODLFormatDate     ();
int ODLFormatTime     ();
int ODLFormatDateTime ();
int ODLFormatString   ();
int ODLFormatComment  ();

#ifdef EOSDIS
/* GMS: redirect errors to IK_Syslog instead */
#  include "IK_Syslog.h"
#  define ODLPrintError(warning)      \
{ \
      if (ODLlinenumber_flag) \
                IK_vSyslog(LOG_ERR, "ODL: Error at line %d: %s", yylineno,warning); \
      else \
                IK_vSyslog(LOG_ERR, "ODL: %s", warning); \
      ODLerror_count++; \
		 }

#  define ODLPrintWarning(warning) \
{ \
      if (ODLlinenumber_flag) \
                IK_vSyslog(LOG_NOTICE, "ODL: Warning at line %d: %s", yylineno,warning); \
      else \
                IK_vSyslog(LOG_NOTICE, "ODL: %s", warning); \
      ODLwarning_count++; \
		 }

#  define ODLPrintInfo(warning)               IK_vSyslog(LOG_INFO, "ODL: %s", warning) 

#else
   void ODLPrintError   ();
   void ODLPrintWarning ();
   void ODLPrintInfo    ();
#endif
void ODLPrintStmt    ();
void ODLWriteStmt    ();

void ODLBeginAggregate ();
void ODLEndAggregate   ();
void ODLBeginParameter ();
void ODLMarkParameter  ();
void ODLStoreValue     ();
void ODLStoreUnits1    ();
void ODLStoreUnits2    ();
void ODLMarkUnits      ();
void ODLCheckSequence  ();
void ODLCheckRange     ();
int ODLEndLabel        ();

#endif
#endif

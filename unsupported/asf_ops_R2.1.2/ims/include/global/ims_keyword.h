/******************************************************************************
**
** File:        ims_keyword.h
**
** Function:    This header file defines the enumerated types
**              required for IMS keyword manipulation.
**
** Author:      S. Hardman
**
** Date:        5/25/95
**
******************************************************************************/

#ifndef _IMS_KEYWORD_H
#define _IMS_KEYWORD_H

static char *sccsKeyword = "@(#)ims_keyword.h	5.1  16 Mar 1996";

/*
** Enumerated types for keyword significance values.
*/
typedef enum ims_keywordSignificance
{
	IMS_NO_SIGNIFICANCE,
	IMS_MANDATORY_NOT_INDEXED,
	IMS_MANDATORY_INDEXED,
	IMS_OPTIONAL_INDEXED,
	IMS_OPTIONAL_NOT_INDEXED
}IMS_KEYWORD_SIGNIFICANCE;

/*
** Enumerated types for keyword data type values.
*/
typedef enum ims_dataType
{
	IMS_NO_DATA_TYPE,
	IMS_INT1_TYPE,         /* CS_TINYINT  */
	IMS_INT2_TYPE,         /* CS_SMALLINT */
	IMS_INT4_TYPE,         /* CS_INT      */
	IMS_FLOAT4_TYPE,       /* CS_REAL     */
	IMS_FLOAT8_TYPE,       /* CS_FLOAT    */
	IMS_CHAR_TYPE,         /* CS_CHAR     */
	IMS_SYMBOL_TYPE,       /* CS_CHAR     */
	IMS_STRING_TYPE,       /* CS_CHAR     */
	IMS_DATETIME_TYPE,     /* CS_DATETIME */
	IMS_DOYTIME_TYPE       /* CS_CHAR - millisecond precision */
}IMS_DATA_TYPE;

#endif	/* !_IMS_KEYWORD_H */

#ifndef PPS_SMARTLIST_H
#define PPS_SMARTLIST_H

typedef enum
{
	PPS_SMART_TEXT,
	PPS_SMART_INT,
	PPS_SMART_FLOAT,
	PPS_SMART_OPTION_MENU

} PPSWidgetTypeE;

typedef struct
{
	int					unitLen;
	int					numOptions;
	char*				options;

} PPSSmartOptionAttr;

typedef struct
{
	char				widgetName[PPS_STRING_LEN];
	char				label[PPS_STRING_LEN];
	PPSWidgetTypeE		widgetType;
	void*				attributes;		/* holds pointer to attributes */

} PPSSmartWidgetSpec;

Widget ppsCreateSmartList(
						Widget					parent,
						String					name,
						PPSSmartWidgetSpec*		smartSpecs,
						int						numSpecs);

Boolean
ppsGetSmartListStrings(
					Widget					w,			/* IN */
					String**				strings,	/* OUT */
					int*					numStrings);/* OUT */

void
ppsFreeSmartListStrings(
					String*					strings,	/* IN */
					int						numStrings);/* IN */

Boolean
ppsSetSmartListStrings(
					Widget					w,			/* IN */
					String*					strings,	/* IN */
					int						numStrings);/* IN */

#endif

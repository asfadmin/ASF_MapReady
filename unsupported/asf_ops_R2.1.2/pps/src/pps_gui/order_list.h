#ifndef PPS_ORDERLIST_H
#define PPS_ORDERLIST_H

Widget
ppsCreateOrderList(
					Widget					parent,		/* IN */
					String					name,		/* IN */
					String*					strings,	/* IN */
					int						numStrings);/* IN */

Boolean
ppsGetOrderListStrings(
					Widget					w,			/* IN */
					String**				strings,	/* OUT */
					int*					numStrings);/* OUT */

void
ppsFreeOrderListStrings(
					String*					strings,	/* IN */
					int						numStrings);/* IN */

Boolean
ppsSetOrderListStrings(
					Widget					w,			/* IN */
					String*					strings,	/* IN */
					int						numStrings);/* IN */

#endif

/*=============================================================================
 |  @(#)Time.c	1.5 96/02/23 18:38:44
 |
 |  Time Object Class Realization.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#include <string.h>
#include "Time.h"
#include "Relay.h"

static char sccsid_Time_c[] =
	"@(#)Time.c	1.5 96/02/23 18:38:44";


#ifdef  NOSTDARG
Time_t* Time (va_alist)
va_dcl
#else

Time_t* Time (Msg_t msg, ...)
#endif
{
    char   	blank[64], buf[32], *str;
    String_t*	image;
    size_t	indent;
    struct tm 	t;
    double 	frac;
    long   	tv_usec;
    Time_t*	obj;
    va_init(ap, msg, obj);
    if (!obj) return NULL;

    switch (msg) {
    case _Init:
#ifdef	DEBUG
printf(" [Time] ");
#endif
	Obj(_Init, obj, Time);
	obj->vlast = -1;
	obj->unit = 0;
	memset(&obj->value, 0, sizeof(GMT_t));
        return String(_Relay, _Init, &obj->image, ap) ? obj : NULL;

    case _Destroy:
        Destroy(&obj->image);
	if (obj->unit) {
	    Destroy(obj->unit);
	    free(obj->unit);
	    obj->unit = 0;
	}
	return NULL;

    case _SetVal:
	obj->value = *va_arg(ap, GMT_t*);
	obj->vlast = 1;
	return obj;

    case _Val:
	if (obj->vlast >= 0)
	    return (Time_t*) &obj->value;

	memset(&t, 0, sizeof(struct tm));
	str = Val(&obj->image);

	if (strchr(str, '.') == NULL) {
	    if (sscanf(str, "%d-%dT%d:%d:%d",
		&t.tm_year, &t.tm_yday, &t.tm_hour, &t.tm_min, &t.tm_sec) != 5)
		return NULL;
	    tv_usec = 0;		
	}
	else if (sscanf(str, "%d-%dT%d:%d:%lf",
		&t.tm_year, &t.tm_yday, &t.tm_hour, &t.tm_min, &frac) != 5)
	    return NULL;
	else {
	    t.tm_sec = frac;
	    tv_usec  = ((frac - t.tm_sec) * 1000000);
	}
	if (t.tm_hour > 23 || t.tm_min  > 59  || t.tm_sec > 61 ||
	    t.tm_yday < 1  || t.tm_yday > 366 || t.tm_year < 1900)
	    return NULL;

	t.tm_mday = t.tm_yday;
	t.tm_yday -= 1;
	t.tm_year -= 1900;
	t.tm_isdst = -1;
	obj->value.tm = t;
	obj->value.tv_usec = tv_usec;
	obj->vlast = 0;
	return (Time_t*) &obj->value;

    case _Unit_of:
        return (Time_t*) Val(obj->unit);

    case _SetUnit:
        return (obj->unit = va_arg(ap, Unit_t*), obj);

    case _Image:
        image = va_arg(ap, String_t*);
        if ((indent = va_arg(ap, size_t)) >= sizeof(blank))
            return NULL;

        if (indent > 0) memset(blank, ' ', indent);
        blank[indent] = 0;

        if (obj->vlast > 0) {
	    int n;
	    t = obj->value.tm;
	    n = sprintf(buf, "%.4d-%.3dT%.2d:%.2d:%.2d",
		t.tm_year+1900, t.tm_yday+1, t.tm_hour,t.tm_min,t.tm_sec);

	    if (obj->value.tv_usec)
		n += sprintf(buf+n, ".%.3d", obj->value.tv_usec/1000);

            if (! SetVal(&obj->image, buf))
                return NULL;

            obj->vlast = 0;
        }
        return (Time_t*) String(_Cat, image, blank, Val(&obj->image), 0);

    default:
	return NULL;
    }
}

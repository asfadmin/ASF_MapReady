#ifndef GUI_COMMON_H
#define GUI_COMMON_H

static const char gui_common_h_id[] = "@(#)gui_common.h	1.3  02/21/97";

#ifndef NUM_MAX_ARGS
#define NUM_MAX_ARGS      30
#endif
 
#ifndef PPS_STRING_LEN
#define PPS_STRING_LEN      80
#endif
 
#ifndef PPS_BIG_SIZE
#define PPS_BIG_SIZE        1024
#endif
 
#ifndef     MIN_DOUBLE_CLICK_INTERVAL
#define     MIN_DOUBLE_CLICK_INTERVAL       400
#endif
 
#define CREATE_XMSTRING(x) (XmStringCreateLtoR((char*)x,XmFONTLIST_DEFAULT_TAG))


#endif /* GUI_COMMON_H */

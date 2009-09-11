#ifndef _IOCINC_H_
#define _IOCINC_H_

#include "buf.h"
#include "fdesc.h"
#include "datasize.h"

lasErr FUNCTION allocate_buffer(struct FDESC *fd,struct GDESC *gd);
lasErr FUNCTION check_window(struct FDESC *fd,struct DDR *ddr);
lasErr FUNCTION process_it(struct FDESC *fd, int cline, int cband,struct GDESC * gd);
#endif

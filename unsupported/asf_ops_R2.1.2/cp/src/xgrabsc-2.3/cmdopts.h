/*========================================================================
 *
 * Name - cmdopts.h
 *
 * ccs version:	1.6
 *
 * ccsid:	@(#)cmdopts.h	1.6 - 06/28/93 09:13:42
 * from: 	ccs/s.cmdopts.h
 * date: 	06/28/93 09:14:48
 *
 * Copyright (c) 1990-93 Bruce Schuchardt.
 * Read the file cpyright.h for full copyright information.
 *
 *
 * Description:
 *
 * command line options for xgrabsc
 *
 *========================================================================
 */

typedef enum {
  CMD_BELL,
  CMD_GRABSERVER,
  CMD_DISPLAY,
  CMD_OUTPUT,
  CMD_PRESLEEP,
  CMD_POSTSLEEP,
  CMD_VERBOSE,
  CMD_BORDERS,
  CMD_NOBORDERS,

  CMD_SOURCE_KEY,
  CMD_SOURCE_ID,
  CMD_SOURCE_RECT,
  CMD_SOURCE_ROOT,
  CMD_SOURCE_WD,

  CMD_AND,
  CMD_OR,
  CMD_NOT,
  CMD_BRIGHTEN,

  CMD_DITHER_MAP,
  CMD_DITHER_MATRIX,
  CMD_DITHER_FS,
  CMD_DITHER_HALFTONE,
  CMD_DITHER_NONE,

  CMD_OUTPUT_PS,
  CMD_OUTPUT_CPS,
  CMD_OUTPUT_SIMPLE,
  CMD_OUTPUT_XWD,
  CMD_OUTPUT_XWDXY,
  CMD_OUTPUT_XPM,
  CMD_OUTPUT_XPM2,
  CMD_OUTPUT_XPM3,
  CMD_OUTPUT_PIXMAP,
  CMD_OUTPUT_PUZZLE,

  CMD_BIN,
  CMD_COMPRESS,
  CMD_EPS,
  CMD_PREVIEWONLY,
  CMD_LANDSCAPE,
  CMD_LIMIT,
  CMD_PAGE,
  CMD_PREVIEW,
  CMD_COLORPROC,

  CMD_SOURCE_COORDS
  
  } commandEnum;


typedef struct {
  commandEnum	command;
  int		numargs;
  char		*userstr;
  } commandType;

commandType commands[] = {
 {  CMD_BELL,		0, "bell"	},
 {  CMD_GRABSERVER,	0, "grab"	},
 {  CMD_DISPLAY,	1, "d"		},
 {  CMD_DISPLAY,	1, "display"	},
 {  CMD_OUTPUT,		1, "o"		},
 {  CMD_OUTPUT,		1, "out"	},
 {  CMD_OUTPUT,		1, "output"	},
 {  CMD_PRESLEEP,	1, "s"		},
 {  CMD_PRESLEEP,	1, "sleep"	},
 {  CMD_POSTSLEEP,	1, "post"	},
 {  CMD_VERBOSE,	0, "verbose"	},
 {  CMD_BORDERS,	0, "borders"	},
 {  CMD_BORDERS,	0, "bdrs"	},

 {  CMD_SOURCE_KEY,	0, "key"	},
 {  CMD_SOURCE_ID,	1, "id"		},
 {  CMD_SOURCE_RECT,	0, "stretch"	},
 {  CMD_SOURCE_ROOT,	0, "root"	},
 {  CMD_SOURCE_WD,	0, "click"	},

 {  CMD_AND,		1, "and"	},
 {  CMD_OR,		1, "or"		},
 {  CMD_NOT,		0, "reverse"	},
 {  CMD_BRIGHTEN,	1, "b"		},
 {  CMD_BRIGHTEN,	1, "brighten"	},

 {  CMD_DITHER_MAP,	0, "bw"		},
 {  CMD_DITHER_MATRIX,	0, "mdither"	},
 {  CMD_DITHER_FS,	0, "dither"	},
 {  CMD_DITHER_HALFTONE,0, "halftone"	},
/*  {  CMD_DITHER_NONE,    0, "nodither"   }, */

 {  CMD_OUTPUT_PS,	0, "ps"		},
 {  CMD_OUTPUT_CPS,	0, "cps"	},
 {  CMD_OUTPUT_SIMPLE,	0, "simple"	},
 {  CMD_OUTPUT_XWD,	0, "xwd"	},
 {  CMD_OUTPUT_XWDXY,   0, "xwdxy"      },
 {  CMD_OUTPUT_PIXMAP,	0, "bm"		},
 {  CMD_OUTPUT_XPM2,	0, "bm2"	},
 {  CMD_OUTPUT_XPM3,	0, "bm3"	},
 {  CMD_OUTPUT_PUZZLE,	0, "puzzle"	},

 {  CMD_BIN,    	0, "bin"	},
 {  CMD_COMPRESS, 	0, "comp"	},
 {  CMD_COMPRESS, 	0, "compress"	},
 {  CMD_EPS,    	0, "eps"	},
 {  CMD_PREVIEWONLY,	0, "previewonly"},
 {  CMD_LANDSCAPE,	0, "l"		},
 {  CMD_LANDSCAPE,	0, "landscape"	},
 {  CMD_LIMIT,		0, "limit"	},
 {  CMD_PAGE,		1, "page"	},
 {  CMD_PREVIEW,	0, "preview"	},
 {  CMD_PREVIEW,	0, "prev"	},
 {  CMD_COLORPROC,	0, "colproc"    },
 
 {  CMD_SOURCE_COORDS,  1, "coords"     }
 
};

int numCmds = sizeof(commands) / sizeof(commandType);

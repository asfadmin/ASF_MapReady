/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* mbahelp.c -- help routines for bob */

#include <stdio.h>

/* bob_help (opcode,hlines) --------------------------------------------
	This routine prints help messages for bobtalk.
	"opcode" is the command number of the command for which help
		has been requested, or -1 if general help is desired.
	"hlines" is the number of history lines available.
*/
bob_help (opcode,hlines)
int opcode, hlines;
{
	int l, c;
	char s[100];

	switch (opcode) {
	  case -1:
	  case 254:
	    printf ("set word:        s  addr  value\n");
	    printf ("display words:   d  addr  [count]\n");
	    printf ("display real:    df  addr  [count]\n");
	    printf ("disp. intensity: dfi  addr  [count]\n");
	    printf ("move words:      m  src_start  src_end  ");
	      printf ("dest_start  [dest_end] \n");
	    printf ("compare:         c  blk1_start  blk1_end");
	      printf ("  blk2_start  [blk2_end]\n");
	    printf ("fill words:      f  strt_addr  end_addr  ");
	      printf ("value\n");
	    printf ("compare syncs:   csr  strt_addr  end_addr  ");
	      printf ("sync_addr  [spacing]\n");
	    printf ("fill with count: fc  strt_addr  end_addr  ");
	      printf ("beg_cnt  cnt_inc  end_cnt  [addr_inc]\n");
	    printf ("fill w/random:   fr  strt_addr  end_addr  ");
	      printf("[addr_inc  [random_set]]\n");
	    printf ("write to disk:   wd  strt_addr  end_addr  ");
	      printf("filename\n");
	    printf ("loop on rd/wrt:  lo  r/w[b]  addr  [value]\n");
	    printf ("init all regs:   i -or- pup\n");
	    printf ("trigger:         t  [count]\n");
	    printf ("clear graphics:  cg  [option]\n");
	    printf ("line(bar) plot:  p(1)  start_addr\n");
	    printf ("image source:    is  filename (or EXEC)\n");
	    printf ("image data type: id  type  format  ");
	      printf("[line_len  [sub-line_len]]\n");
	    printf ("image format:    if  scale  range  [mask]\n");
	    printf ("load greyscale:  ig  min  max  [scale]\n");
	    printf ("image location:  il  xtop  yleft\n");
	    printf ("disp.img.params: dip\n");
	    printf ("display image:   im  strt_adr  x  y  npix  ");
	      printf("nlin  [cmp_x  cmp_y  cmp_type]\n");
	    termsize (&l,&c);
	    if (l < 26) {
		printf("-- more --");
		gets(s);
	    }
	    printf ("run bob script:  b  filename  [repeatcount]\n");
	    printf ("graphical bob:   ibob\n");
	    printf ("display register:dreg  boardname  regnum\n");
	    printf ("set register:    sreg  boardname  regnum  [fieldnum]"); 
	      printf("  value\n");
	    printf ("display cage:    dc  [cagename]\n");
	    printf ("run c-shell:     sh\n");
	    printf ("change directory:cd  directory\n");
	    printf ("edit a file:     vi  filename\n");
	    printf ("set visual mode: v\n");
	    printf ("stop on error:   a\n");
	    printf ("reset v & a :    n\n");
	    printf ("command history: hi   (up to %d lines of history)\n",
			    hlines);
	    printf ("repeat command:  !! -or- !n -or- !cmd\n");
	    printf ("write history:   w  start_line  end_line  ");
	      printf ("filename\n");
	    printf("svp par val: set VME parameter to value (CAREFUL!)\n");
	    printf ("on-line help:    h  [command]\n");
	    printf ("quit:            q\n");
	    break;
	  case 0:
	  case 1:
	    printf ("set word:        s  addr  value\n");
	    printf ("set stim word:   ss  stim_addr  value\n\n");
	    printf ("    Sets one 16-bit word to the given value\n\n");
	    printf ("  addr:      byte address of word to set\n");
	    printf ("  value:     16-bit value to which word is set\n");
	    break;
	  case 2:
	  case 3:
	    printf ("display words:    d  addr  [count]\n");
	    printf ("display response: dr  resp_addr  [count]\n\n");
	    printf ("    Displays data in hex format\n\n");
	    break;
	  case 4:
	  case 5:
	    printf ("display fixed pt:      df  addr  [count]\n");
	    printf ("display fixed pt resp: dfr  resp_addr  [count]\n\n");
	    printf ("    Displays data in fixed point format\n\n");
	    break;
	  case 6:
	  case 7:
	  case 39:
	    printf ("move words:           m  src_start  src_end  ");
	      printf ("dest_start  [dest_end] \n");
	    printf ("move words from resp: mr  resp_start  resp_end  ");
	      printf ("dest_start  [dest_end] \n");
	    printf ("move words to stim:   ms  src_start  src_end  ");
	      printf ("stim_start  [stim_end] \n\n");
	    printf ("    Moves 16-bit words from one place to another\n\n");
	    break;
	  case 8:
	  case 9:
	  case 59:
	    printf ("compare:          c  blk1_start  blk1_end");
	      printf ("  blk2_start  [blk2_end]\n");
	    printf ("compare response: cr  resp_start  resp_end");
	      printf ("  blk2_start  [blk2_end]\n");
	    printf ("compare response real: crr  resp_start  resp_end");
	      printf ("  blk2_start  [blk2_end]\n\n");
	    printf ("    Compares two sets of 16-bit words, word-for-word\n\n");
	    break;
	  case 12:
	  case 13:
	    printf ("fill words:      f  strt_addr  end_addr  ");
	      printf ("value\n");
	    printf ("fill stim words: fs  stim_start  stim_end  ");
	      printf ("value\n\n");
	    printf ("    Fills a set of 16-bit words with the given value\n\n");
	    break;
	  case 14:
	  case 15:
	    printf ("fill with count:   fc  strt_adr  end_adr  ");
	      printf ("beg_cnt  cnt_inc  end_cnt  [adr_inc]\n");
	    printf ("fill stim w/count: fcs  strt_adr  end_adr  ");
	      printf ("beg_cnt  cnt_inc  end_cnt  [adr_inc]\n\n");
	    printf ("    Fills a set of 16-bit words with a counter\n");
	    printf ("  adr_inc:  Every n'th word is filled (default=2)\n\n");
	    break;
	  case 16:
	  case 17:
	    printf ("line plot:           p  start_addr\n");
	    printf ("line plot response:  pr  resp_addr\n\n");
	    printf ("    Draws a line plot of data.  The routine prompts\n");
	    printf ("for 7 more parameters, which may be entered on the\n");
	    printf ("command line if desired.\n\n");
	    break;
	  case 18:
	  case 19:
	    printf ("bar plot:           p1  start_addr\n");
	    printf ("bar plot response:  p1r  resp_addr\n\n");
	    printf ("    Draws a bar plot of data.  The routine prompts\n");
	    printf ("for 7 more parameters, which may be entered on the\n");
	    printf ("command line if desired.\n\n");
	    break;
	  case 20:
	    printf ("load greyscale:  ig  min  max  [scale]\n");
	    printf ("load rainbow:    ig\n\n");
	    printf ("    Loads the first 256 entries of the graphics color\n");
	    printf ("table with a greyscale ramp.  Entries below min are set\n");
	    printf ("to black; entries above max to white.\n");
	    printf ("If no arguments are given, a color rainbow is loaded.\n\n");
	    printf ("  scale:  0 = linear (default)\n");
	    printf ("          1 = log\n");
	    printf ("Scale determines method for incrementing intensity\n");
	    printf ("between min and max.\n\n");
	    break;
	  case 21:
	    printf ("image location:  il  xtop  yleft\n\n");
	    printf ("    Sets the screen location of the upper left corner\n");
	    printf ("of the image.\n");
	    printf ("The default is 0, 0 (upper left corner of screen).\n\n");
	    break;
	  case 22:
	    printf ("image data type: id  type  format  ");
	      printf("[line_len  [sub-line_len]]\n\n");
	    printf ("    Defines the image data type and format.\n\n");
	    printf ("  type:   0-3 = that byte in the 4-byte resp. buffer format\n");
	    printf ("           8 = consecutive bytes\n");
	    printf ("           A = real value (two's compl.) (bytes 0 & 1)\n");
	    printf ("           B = imaginary value (bytes 2 & 3)\n");
	    printf ("           C = intensity (r*r + i*i) (default)\n");
	    printf ("           D = sync codes\n");
	    printf ("           E = square root of real value (bytes 0 & 1)\n");
	    printf ("           F = real value in unsigned notation (bytes 0 & 1)\n\n");
	    printf ("  format: 0 = sequential (default)\n");
	    printf ("          1 = bit-reversed\n");
	    printf ("          2 = hi-lo\n");
	    printf ("          4 = sequential mux\n");
	    printf ("          5 = bit-reversed mux\n");
	    printf ("          6 = hi-lo mux\n\n");
	    printf ("  line_len: length of 1 line of data\n");
	    printf ("  sub-line_len: length of 1 sub-line (e.g., fft sub-line size)\n\n");
	    break;
	  case 23:
	    printf ("image format:    if  scale  range\n");
	    printf ("          or:    if  scale  range(3)  min  max\n");
	    printf ("          or:    if  scale  range(4)  mask\n\n");
	    printf ("    Defines the output image format\n\n");
	    printf ("  scale:   0 = linear (default)\n");
	    printf ("           1 = log\n\n");
	    printf ("  range:   0 = min & max = entire dynamic range (default)\n");
	    printf ("           1 = derive min & max from data in entire image\n");
	    printf ("           2 = min & max derived from displayed data only\n");
	    printf ("           3 = min and max supplied by user\n");
	    printf ("           4 = <and> data with bit mask\n\n");
	    break;
	  case 24:
	  case 25:
	    printf ("display image:   im  strt_adr  x  y  npix  ");
	      printf("nlin  [cmp_x  cmp_y  cmp_type]\n");
	    printf ("disp resp image: imr  strt_adr  x  y  npix  ");
	      printf("nlin  [cmp_x  cmp_y  cmp_type]\n\n");
	    printf ("    Displays an image.\n\n");
	    printf ("  strt_addr:  The memory location of the start of the image data\n");
	    printf ("  x, y:       The x,y location of the first data sample to\n");
	    printf ("                be displayed\n");
	    printf ("  npix,nlin:  The x and y number of data values to display\n");
	    printf ("  cmp_x, cmp_y: X and y compression factors.  Negative = expansion\n");
	    printf ("                When compressing, the size of the displayed image is\n");
	    printf ("                (npix / cmp_x) pixels by (nlin / cmp_y) lines.\n");
	    printf ("                When expanding, the size is:\n");
	    printf ("                (npix / -cmp_x) pixels by (nlin / -cmp_y) lines.\n");
	    printf ("  cmp_type:  Determines what value to use when m by n values\n");
	    printf ("               are compressed into 1 pixel for display:\n");
	    printf ("             0 = average (default)\n");
	    printf ("             1 = minimum value\n");
	    printf ("             2 = maximum value\n\n");
	    break;
	  case 26:
	    printf ("decode address:  ad  address\n\n");
	    printf ("    Prints the page and offset in the response buffer\n");
	    printf ("corresponding to the given relative response address\n\n");
	    break;
	  case 27:
	    printf ("set trigger count: tm  count\n\n");
	    printf ("    Sets the number of triggers per t command\n\n");
	    break;
	  case 28:
	    printf ("trigger:         t  [count]\n\n");
	    printf ("    Starts the EXEC running\n\n");
	    break;
	  case 29:
	  case 30:
	    printf ("init all regs:   i -or- pup\n\n");
	    printf ("    Sets all hardware registers to an initial state,\n");
	    printf ("with all boards in bypass.\n\n");
	    break;
	  case 31:
	    printf ("set stop-on-error:  a\n\n");
	    printf ("    Sets BOB in stop-on-error mode (which currently\n");
	    printf ("has no effect).\n\n");
	    break;
	  case 32:
	    printf ("set visual mode:  v\n\n");
	    printf ("    Sets BOB to display each command in a batch file\n\n");
	    break;
	  case 33:
	    printf ("set normal mode:  n\n\n");
	    printf ("    Cancels both stop-on-error and visual mode\n\n");
	    break;
	  case 34:
	    printf ("execute batch file: b  filename  [repetitions]\n\n");
	    printf ("    Executes a text file of bob commands.  This command\n");
	    printf ("may be nested up to 10 deep.\n");
	    printf ("'repetitions' gives the number of times to repeat the command\n\n");
	    break;
	  case 35:
	  case 44:
	  case 45:
	    printf ("    BOB is a command-driven debugging tool which interfaces with\n");
	    printf ("a hardware device through a memory-mapped array.\n");
	    printf ("The array is 1 Mbyte, with the following memory-mapped areas:\n");
	    printf ("      00000 - 00001 : PID (page ID) register\n");
	    printf ("      00004 - 00fff : device registers\n");
	    printf ("      01000 - 01007 : special data words for addressing\n");
	    printf ("                        the response buffer\n");
	    printf ("      01008 - 7ffff : local computer memory\n");
	    printf ("      80000 - fffff : device memories.  What shows here\n");
	    printf ("                        depends on the value in the PID register\n");
	    printf ("All addresses are given in bytes.  Addresses in the stimulus and\n");
	    printf ("response buffers may be given relative to the start of the buffer.\n");
	    printf ("The stimulus buffer always starts at 80000.  The response starts\n");
	    printf ("at some location in the response buffer, which is calculated auto-\n");
	    printf ("matically by the routines from the EXEC board FREEZEADDR and DELAY\n");
	    printf ("registers in conjunction with the values in locations 1000 and 1004.\n");
	    printf ("1000 = DATA START (32 bits); 1004 = RESPONSE LENGTH (32 bits).\n\n");
	    break;
	  case 36:
	    printf ("break and continue:  br\n\n");
	    printf ("    Stops execution of BOB, and returns a value of 0.\n\n");
	    break;
	  case 37:
	    printf ("quit:     q\n\n");
	    printf ("    Stops execution of BOB, and returns a value of 1.\n\n");
	    break;
	  case 38:
	    printf ("clear graphics screen:  cg [option]\n\n");
	    printf ("    Clears the screen.\n\n");
	    printf ("  option:   0 = clear graphics but not text\n");
	    printf ("            1 = clear graphics and text\n");
	    printf ("            2 = clear text only\n\n");
	    break;
	  case 40:
	  case 41:
	    printf ("fill w/random data:   fr  strt_addr  end_addr  ");
	      printf("[addr_inc  [random_set]]\n");
	    printf ("fill stim w/random:   frs  strt_addr  end_addr  ");
	      printf("[addr_inc  [random_set]]\n\n");
	    printf ("    Fills a set of locations with a random data set.\n\n");
	    printf ("  addr_inc:  fills every n'th word (default=2)\n\n");
	    printf ("  random_set:  if used, initializes random number generator,\n");
	    printf ("                 then skips n-1 sets of random values before\n");
	    printf ("                 starting fill\n\n");
	    break;
	  case 42:
	    printf ("display history:  hi  [count]\n\n");
	    printf ("    Displays the last n commands entered.  If 'count' is not used\n");
	    printf ("displays all previous commands up to a max of %d.\n\n",hlines);
	    break;
	  case 43:
	    printf ("write history to disk:  w  start  end  filename\n\n");
	    printf ("    Writes the lines between 'start' and 'end' to a disk file\n");
	    printf ("whose name is given by 'filename'.\n\n");
	    break;
	  case 46:
	    printf ("loop on read/write:  lo  r/w  addr  [value]\n\n");
	    printf ("    Performs a tight loop read or write of the given address.\n");
	    printf ("If writing, 'value' is the value to write.\n");
	    printf ("If reading, 'value' > 0 means do not display.\n\n");
	    break;
	  case 47:
	    printf ("compare syncs:    csr  respstart  respend  syncaddr  [syncend  [spacing]]\n\n");
	    printf ("    Compares an array of sync codes at syncaddr with the\n");
	    printf ("sync codes found in the response buffer in the given\n");
	    printf ("address range.\n\n");
	    printf ("If syncend is not entered, the number of syncs is derived from the\n");
	    printf ("CODEPER register of the EXEC STIM board.\n");
	    printf ("If spacing is not entered, the spacing between syncs is derived\n");
	    printf ("from the LSYNCPOW field of the EXEC STIM board register 0.\n");
	    break;
	  case 48:
	  case 49:
	    printf ("display fixed pt intensity:      dfi  addr  [count]\n");
	    printf ("display fixed pt intensity resp: dfir  resp_addr  [count]\n\n");
	    printf ("    Displays intensity data in fixed point format\n\n");
	    printf ("Intensity is defined as:\n");
	    printf ("   (real * real)  + (imaginary * imaginary)\n");
	    printf ("where real and imaginary are 2 successive 16-bit words\n");
	    printf ("from the data to be displayed.\n\n");
	    break;
	  case 50:
	    printf ("run graphical bob:  ibob\n\n");
	    printf ("    Brings up a graphical interface which displays\n");
	    printf ("and modifies the machine state by means of a hardware\n");
	    printf ("description file, hardware.dat.  The mouse is used\n");
	    printf ("to pick data values and memory areas to display and/or\n");
	    printf ("modify.\n\n");
	    break;
	  case 51:
	    printf ("run c-shell:       sh\n\n");
	    printf ("    Drops into the unix c shell so the user may execute\n");
	    printf ("unix commands.  To return to bob, type 'exit'\n\n");
	    break;
	  case 52:
	    printf ("change directory: cd  directory\n\n");
	    printf ("    Changes the working directory to the one given\n\n");
	    break;
	  case 53:
	    printf ("display image parameters:  dip\n\n");
	    printf ("    Displays the current setting of all image parameters\n\n");
	    break;
	  case 54:
	    printf ("edit a file:      vi  filename\n\n");
	    printf ("    Invokes the Unix vi editor to edit the named file\n\n");
	    break;
	  case 55:
	    printf ("set image source: is  filename (or EXEC)\n\n");
	    printf ("    Sets the source of the image to be displayed\n\n");
	    printf ("     filename is UNIX file, or EXEC (default)\n\n");
	    break;
	  case 56:
	    printf ("display register:  dreg  boardname  regnum\n");
	    printf ("                   dreg  regaddr\n\n");
	    printf ("    Displays the bitfields of a register defined in the\n");
	    printf ("/usr/local/hardware.dat file.  boardname is the name of\n");
	    printf ("the board on which the register resides, as given by\n");
	    printf ("hardware.dat.  regnum is the relative register number\n");
	    printf ("on the board.\n\n");
	    printf ("    In the altername form of the command, regaddr is\n");
	    printf ("the hardware address of the register.\n\n");
	    break;
	  case 57:
	    printf ("set register:   sreg  boardname  regnum  value\n");
	    printf ("                sreg  boardname  regnum  fieldnum  value\n\n");
	    printf ("    Sets the entire value of the given register.  The\n");
	    printf ("boardname is the name of the board on which the register\n");
	    printf ("resides, as given by /usr/local/hardware.dat.  regnum\n");
	    printf ("is the relative register number on the board.  value is\n");
	    printf ("the desired new value of the register.\n\n");
	    printf ("    In the alternate form of the command, fieldnum is the\n");
	    printf ("number of the particular bitfield to be changed, as\n");
	    printf ("printed by the dreg command.\n\n");
	    break;
	  case 58:
	    printf ("display cage:   dc  [cagename]\n\n");
	    printf ("    Displays the boards and register values for one\n");
	    printf ("entire cage, as defined by the /usr/local/hardware.dat\n");
	    printf ("file.  If cagename is omitted, a list of the defined\n");
	    printf ("cage names is displayed.\n\n");
	    break;
	  case 60:
	  case 61:
	    printf ("write data:          wd  start_addr  end_addr  filename\n");
	    printf ("write response data: wdr  resp_start  resp_end  filename\n\n");
	    printf ("    Write data to a disk file.\n\n");
	    break;
	  case 62:
	    printf("svp par val: set VME parameter to value (CAREFUL!)\n");
	    printf("Consult the BIT-3 Model 954 Software Support Manual\n");
	    break;
	  case 255:
	    printf ("unknown command\n\n");
	    break;
	  default:
	    printf ("The programmer was too lazy to document this one\n\n");
	    break;
	}  /* switch (opcode) */
}

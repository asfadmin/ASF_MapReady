
/* ******  5/30/95:  D. Pass   error in this code:  the
    integers in this file are too long:  a short is 2 characters
    long and a long is 4 characters long, but here the length
    has been doubled.   note the i2 chars:  what is this?????  */
struct   recfpr
                {
                int recnum;
                char fstsub;
                char rectyp;
                char secsub;
                char thrsub;
                int  reclen;
                char asciicd[2];
                char blank2[2];
                char fnumb[4];  /*  should be 2  */
                char fid[16];
                char fclass[28];
                char fclasscd[4];
                char fdtype[28];
                char fdtcode[4];
                char fnumrec[8];    /*  should be 4 */
                char frecfst[8];    /*  should be 4 */
                char frecmax[8];    /*  should be 4 */
                char frectype[12];
                char ftypecode[4];
                char phyvolfst[2];  /*  should be 1  */     /*  i2????  */
                char phyvollast[2]; /*  should be 1  */     /*  i2????  */
                char recfirst[8];   /*  should be 4 */
                char reclast[8];    /*  should be 4 */
                char spare1[100];
                char orderline[2];
                char mediaid[16];
                char proccode[3];
                char spare2[79];
                };

struct     recvdr
                {
        int       recnum;
        char      fstsub;
        char      rectyp;
        char      secsub;
        char      thrsub;
        int       reclen;
        char      asciiflg[2];
        char      blank2[2];
        char      supdoc[12];
        char      revnumber[2];
        char      revletter[2];
        char      software[12];
        char      tapeid[16];
        char      logi_vol[16];
        char      volsetid[16];
        char      totvol[2];
        char      phyfirst[2];
        char      phylast[2];
        char      phycurr[2];
        char      fstfile[4];
        char      preslogi[4];
        char      logvolnum[4];
        char      logvoldate[8];
        char      logvoltime[8];
        char      country[12];
        char      agency[8];
        char      facility[12];
        char      pointers[4];
        char      numrecdir[4];
        char      totlogi[4];
        char      spare[188];
        };


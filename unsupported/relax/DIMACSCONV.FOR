C **********************************************************************
C
C		  DIMACS DATA FORMAT CONVERTER
C          	          BY P. TSENG
C                        NOVEMBER 1994 
C **********************************************************************
C 
C     THIS PROGRAM READS IN DATA FOR A LINEAR COST ORDINARY NETWORK
C     FLOW PROBLEM IN DIMACS FORMAT FROM THE ASCII FILE `DIMACS.INP',
C     CONVERTS THE DATA TO A FORMAT READABLE BY RELAX4.FOR 
C     AND WRITES THE DATA TO THE ASCII FILE `RELAX4.INP'.
C     AN INTERMEDIATE ASCII FILE `TEMP' IS CREATED IN THE PROCESS.
C
      program main
      integer supply(9000)
      character*1 a1(80)
      character*80 a2
      equivalence(a1,a2)
c
c open input file.
c
      print*,' read data in DIMACS format from DIMACS.INP'
      OPEN(13,FILE='DIMACS.INP',STATUS='OLD')
      rewind(13)
c 
c open output file to store intermediate results.
c
      open(14,file='TEMP',form='formatted',status='new')
      rewind(14)
c
c read in the lines and convert the key words 'p', 'n', 'a'
c to `0', '1', '2' so they can be read by unformatted read
c 
      nsupply=0
1     read(13,fmt='(a80)',end=998)a2
      if ((a1(1).eq.'p').and.(a1(2).eq.' ')) then
        a1(1)='0'
        a1(3)=' '
        a1(4)=' '
        a1(5)=' '
        write(14,fmt='(a80)')a2
      else if ((a1(1).eq.'n').and.(a1(2).eq.' ')) then
        nsupply=nsupply+1
        a1(1)='1'
        write(14,fmt='(a80)')a2
      else if ((a1(1).eq.'a').and.(a1(2).eq.' ')) then
        a1(1)='2'
        write(14,fmt='(a80)')a2
      end if
      go to 1
998   print*,' save intermediate data in TEMP'
c
c close input file 
c
      close(13)
      rewind(14)
c
c open output file
c
      print*,'write data in RELAX4.FOR format to RELAX4.INP'
      open(11,file='RELAX4.INP',form='formatted', status='new')
      rewind(11)
c
c read in n and na
c 
      read(14,*)n0,n,na
      write(11,1020)n,na
c
c read in supply at nodes
c
      do 10 i = 1,n
10      supply(i) = 0
      do 20 i = 1,nsupply
        read(14,*)n0,n1,n2
        supply(n1) = n2
20    continue
c
c read in starting node, end node, lower bd, upper bd, cost at arcs
c
30    read(14,*,end=999)n0,n1,n2,n3,n4,n5
      if (n0.eq.2) then
c
c make change of variable so lower bd equals zero
c
        n4=n4-n3
        supply(n1)=supply(n1)-n3
        supply(n2)=supply(n2)+n3
        write(11,1020)n1,n2,n5,n4
      end if
      go to 30
1020  format(4I8)
999   do 40 i=1,n
         write(11,1020) supply(i)
40    continue
      close(11)
      close(14)
      end





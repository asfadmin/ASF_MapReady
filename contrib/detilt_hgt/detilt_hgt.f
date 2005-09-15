      program detilt_hgt

c****************************************************************
c**     
c**   FILE NAME: detilt_hgt.f
c**     
c**   DATE WRITTEN: 
c**     
c**   PROGRAMMER: Scott Hensley
c**     
c**   FUNCTIONAL DESCRIPTION: Remove cross track, along track
c**   tilts in height data and adds an offset.
c**     
c**   ROUTINES CALLED:
c**     
c**   NOTES: 
c**     
c**   UPDATE LOG:
c**
c**   Date Changed        Reason Changed                  CR # and Version #
c**   ------------       ----------------                 -----------------
c**     
c*****************************************************************


      implicit none


c     INCLUDE FILES:


c     PARAMETER STATEMENTS:


      integer SIZE
      parameter(SIZE=30000)


c     INPUT VARIABLES:
        
c     OUTPUT VARIABLES:


c     LOCAL VARIABLES:


      character*3 a_type
      real a(SIZE),b(SIZE),sum,pha,phd,c(SIZE),d(SIZE),e(SIZE),suma
      character*120 fin,fout,a_string
      real*4 pa,pd,r_h0,r_null


      integer na,la,ld,lineout,line,nd,k,jpix,i,j,i_recmult


c     COMMON BLOCKS:


c     EQUIVALENCE STATEMENTS:


c     DATA STATEMENTS:


c     FUNCTION STATEMENTS:


      integer iargc


c     SAVE STATEMENTS:


c     PROCESSING STEPS:


      write(6,*) ' '
      write(6,*) '   <<   DELTILT HEIGHT DATA   >> '
      write(6,*) ' '


      if(iargc() .lt. 10)then
         write(6,'(a)') 'Usage: detilt_hgt infile outfile type samples looks_xt looks_at delhgt_xt delhgt_at h0 null' 
         write(6,'(a)') 'Units: delhgt_at and delhgt_xt are meters/pixel '
         write(6,'(a)') 'type: s = separate file , rmg = RMG file' 
         write(6,*) ' '
         stop
      endif


      call getarg(1,fin)
      call getarg(2,fout)
      call getarg(3,a_type)
      call getarg(4,a_string)
      read(a_string,*) na
      call getarg(5,a_string)
      read(a_string,*) la
      call getarg(6,a_string)
      read(a_string,*) ld
      call getarg(7,a_string)
      read(a_string,*) pa
      call getarg(8,a_string)
      read(a_string,*) pd
      call getarg(9,a_string)
      read(a_string,*) r_h0
      call getarg(10,a_string)
      read(a_string,*) r_null


c     compute height ramps ramps


      pha = pa
      phd = pd


c     opne input and output files


      if(index(a_type,'rmg') .ne. 0)then
         open(21,file=fin,form='unformatted',access='direct',recl=na*8)
         open(22,file=fout,form='unformatted',access='direct',recl=na/la*8)
         i_recmult = 2
      elseif(index(a_type,'s') .ne. 0)then
         open(21,file=fin,form='unformatted',access='direct',recl=na*4)
         open(22,file=fout,form='unformatted',access='direct',recl=na/la*4)
         i_recmult = 1
      endif
      
      lineout=0
      nd = 10000000


      write(6,*) ' '
      write(6,'(a,x,i5)') 'Number of output samples: ',na/la
      write(6,*) ' '


      do line=1,nd,ld


         if(mod(line,16*ld) .eq. 1)then
            write(6,'(a,x,i10)') 'At line: ',line
         endif


         lineout = lineout + 1


         do j=1,2*na
            b(j) = 0.0
            c(j) = 0.0
         end do


c     take looks down


         do i=0,ld-1


            read(21,rec=line+i,err=999)(a(k),k=1,i_recmult*na)
            if(index(a_type,'rmg') .ne. 0)then            
               do j=1,na
                  if(a(j+na) .ne. r_null)then
                     c(j) = c(j) + 1
                     b(j) = b(j) + a(j)
                     b(j+na) = b(j+na) + a(j+na) + pha*j + phd*(line+i)
                  endif
               end do
            elseif(index(a_type,'s') .ne. 0)then
               do j=1,na
                  if(a(j) .ne. r_null)then
                     c(j) = c(j) + 1
                     b(j) = b(j) + a(j) + pha*j + phd*(line+i)
                  endif
               end do
            endif


         end do


c  take looks across


         jpix = 0
         do j=1,na,la
            jpix = jpix + 1
            e(jpix) = 0.0
            sum = 0.0
            suma = 0.0
            do k=0,la-1
               if(index(a_type,'rmg') .ne. 0)then
                  e(jpix) = e(jpix) + c(j+k) 
                  sum = sum + b(j+k+na)
                  suma = suma + b(j+k)
               elseif(index(a_type,'s') .ne. 0)then
                  e(jpix) = e(jpix) + c(j+k) 
                  sum = sum + b(j+k)
               endif
            end do
            if(index(a_type,'rmg') .ne. 0)then            
               if(e(jpix) .ne. 0)then
                  d(jpix+na/la) = sum/e(jpix) + r_h0
                  d(jpix) = suma/e(jpix)
               else
                  d(jpix+na/la) = r_null
                  d(jpix) = 0.0
               endif
            elseif(index(a_type,'s') .ne. 0)then
               if(e(jpix) .ne. 0)then
                  d(jpix) = sum/e(jpix) + r_h0
               else
                  d(jpix) = r_null
               endif
            endif
         end do


         write(22,rec=lineout) (d(k),k=1,i_recmult*(na/la))


      end do


 999  write(6,*) ' '
      write(6,'(a)') 'Less resolution and with luck less noise...'


      end

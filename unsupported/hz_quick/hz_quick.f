
c  ersunfbrowse - range compress a series of ers lines, single bin in azimuth

      parameter (m=16384, looksrange=10)
      byte in(m)
      complex signal(m/2),ref(m/2),arr(m/2,128)
      complex out(m/2/looksrange),cphase,temp(128)
      real rarr(m,128),power(m/2/looksrange,1024)
      equivalence (arr,rarr)
      character*60 string,file

      pi=4.*atan(1.)

      if(iargc().lt.1)then
         type *,'usage: ersunf file len centroid(prf) <extend-pts(hi-res
spacing)> <az looks> <fft size> <lines>'
         stop
      end if

      call getarg(1,file)
      call getarg(2,string)
      read(string,*)len
      call getarg(3,string)
      read(string,*)centroid
      nextend=0
      if(iargc().ge.4)then
         call getarg(4,string)
         read(string,*)nextend
      end if
      iazlooks=1
      if(iargc().ge.5)then
         call getarg(5,string)
         read(string,*)iazlooks
      end if
      kfft=0
      if(iargc().ge.6)then
         call getarg(6,string)
         read(string,*)kfft
      end if
      lines=100000
      if(iargc().ge.7)then
         call getarg(7,string)
         read(string,*)lines
      end if

c  get some geometry stuff, ers approximations
c  change these values for other satellites
      prf=1647.
      v=7500.
      wvl=0.0566
      r0=850000.
      ambdist=r0*prf*wvl/2./v
      type *,'azimuth ambiguity distance: ',ambdist

c  define the chirp parameters
c  again change for non-ERS satellites
      s=4.189166e11
      tau=37.12e-6
      fs=18.96d6

c  zero out reference array
      do i=1,m/2
         ref(i)=cmplx(0.,0.)
      end do

c  create the reference chirp
      npts=fs*tau
      do i=-npts/2,npts/2
         t=i/fs
         phase=pi*s*t*t
         ref(i+npts/2+1)=cexp(cmplx(0.,phase))
      end do

c  transform the reference for use below
      call fft(m/2,ref,0)
      call fft(m/2,ref,-1)

c  how many good points ?
      igoodrange=(len-412)/2-npts+2*nextend
      type *,'Good points in range (with extension): ',igoodrange

c  open the data file
      open(20,file=file,access='direct',recl=len)

c  open the output file
      spacingrange=8./sind(23.)*looksrange
      linesazimuth=spacingrange/(v/prf)
      type *,'Looks, spacing in range, lines in azimuth:',looksrange,spacingrange,linesazimuth
c  decide on fft length
      do k=1,12
         if(2**k.ge.linesazimuth)go to 10
      end do
 10   nfft=2**(k-1)
      if(kfft.ne.0)nfft=kfft
      call fft(nfft,temp,0)

c  how many lines between bursts ?
      iburstskip=ambdist/(v/prf)
c12345678901234567890123456789012345678901234567890123456789012345678901234567890

      rectmp=igoodrange/looksrange*8
      open(21,file='hz_q.out',access='direct',recl=rectmp)

c Write status info...
      type *,'FFT size, single burst skip lines: ',nfft,iburstskip
      type *,'** Line length of output image: ',igoodrange/looksrange

c  loop over lines for range processing
      lineout=0
      iburst=0
c  set up centroid for steering
      dphase=centroid*2.*pi+pi
      do line=100,lines,iburstskip/iazlooks
         iburst=iburst+1
         type *,'Linein: ',line
c  read in one burst, range process
         do lineaz=1,nfft

c  first zero out signal array
            do i=1,m/2
               signal(i)=cmplx(0.,0.)
            end do

c  read in an input data line, and place good data into the signal array
            read(20,rec=line+lineaz-1,err=99)(in(k),k=1,len)
c  set up and correct for phase centroid on data
            phase=dphase*lineaz
            cphase=cmplx(cos(phase),-sin(phase))

            k=0
            do i=413,len,2
               k=k+1
               signali=(in(i).and.31)-15.5
               signalq=(in(i+1).and.31)-15.5
               signal(k+nextend)=cmplx(signali,signalq)*cphase
            end do

c  forward transform the signal
            call fft(m/2,signal,-1)

c  calculate cross-product
            do i=1,m/2
               signal(i)=signal(i)*conjg(ref(i))
            end do

c  inverse transform
            call fft(m/2,signal,1)

c  save result in array
            do j=1,m/2
               arr(j,lineaz)=signal(j)
            end do
         end do

c  process in azimuth
         do i=1,igoodrange
            do k=1,nfft
               temp(k)=arr(i,k)
            end do
            call fft(nfft,temp,-1)
            do k=1,nfft
               arr(i,k)=temp(k)
            end do
         end do


         do lineaz=1,nfft
            kline=iburst*nfft/iazlooks-nfft/iazlooks+lineaz
            kkk=0
            do k=1,m,looksrange*2
               kkk=kkk+1
               pow=0.
               do kk=k,k+looksrange*2-1
                  pow=pow+rarr(kk,lineaz)**2
               end do
               power(kkk,kline)=pow+power(kkk,kline)
            end do
         end do

      end do    !end line loop

 99   type *,kline,iburst,' kline iburst'
      do line=1,kline
         do k=1,igoodrange/looksrange
            out(k)=cmplx(sqrt(power(k,line)),0.)
         end do
         write(21,rec=line)(out(k),k=1,igoodrange/looksrange)
      end do

      end



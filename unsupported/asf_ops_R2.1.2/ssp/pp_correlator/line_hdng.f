c SccsId = @(#)line_hdng.f	2.41 3/24/98
c  Subroutine:  line_hdng
c
c  Purpose:  Compute line heading from first and last pixel latitude and
c            longitude of each image line.
c
c  Inputs:   line1 --  line number of first image line
c            line2 --  line number of last image line
c            lat_first -- array of latitude of first image pixel in each line
c            lat_last -- array of latitude of last image pixel in each line
c            long_first -- array of longitude of first image pixel in each line
c            long_last -- array of longitude of last image pixel in each line
c            asc_flag -- set to 1 if the satellite is ascending, 0 otherwise
c
c  Outputs:  line_head -- array of heading of each line of image
c
c           
c
c  Processing:  The following steps are taken:
c
c  1.  Compute delta latitude and longitude between first and last pixels
c      of image line.
c
c  2.  Multiply delta longitude by the cosine of the latitude to adjust
c      for projection on Earth surface at image location.
c
c  3.  Compute direction of image line, relative to East (for ascending)
c      or West (for descending).
c
c  4.  Compute direction of perpendicular to this line.
c
c  5.  If the line goes through a pole, the heading is 180 deg (due south)
c      for the northern hemisphere and 0 deg (north) for the southern
c      hemisphere.
c
c  Assumptions:
c
c  1.  The relations used here for computing the heading are approximate;
c      we are assuming they are close enough for this purpose.  If more
c      rigorous relations are required, the code will have to be changed.
c
c  2.  For measuring longitude, east is the positive direction.
c
c History
c 3/26/97: modified by qdn to change the types of input variables
c          from real*4 to real*8
c original code: written by Jeff Schredder (for ScanSAR processor)
c

      subroutine line_hdng(lat_first,lat_last,long_first,
     *long_last,line_head)

        implicit none

        character*128 SccsId_line_hdng
        data SccsId_line_hdng
     +  /'@(#)PPline_hdng.f:2.41'/


c  Miscellaneous declarations, including subroutine arguments
      real*8 lat_first
      real*8 lat_last
      real*8 long_first
      real*8 long_last
      real*8 line_head
      real*8 pie4,dtr,lat1,lat2,long1,long2,coslat,dlat,dlonc,theta

      pie4=datan(1.0d0)
      dtr=pie4/45.0d0

      lat1=lat_first
      lat2=lat_last
      long1=long_first
      long2=long_last
c  Take cosine of average line latitude
      coslat=dcos(dtr*(lat1+lat2)/2.0d0)
c  Compute delta latitude and cosine corrected delta longitude
      dlat=lat2-lat1
      dlonc=coslat*(long2-long1)
c  Compute line angle
      theta=(datan2(dlat,dlonc))/dtr
c  Due to definition of heading, the following simple expression
c  suffices to compute the perpendicular
      line_head=-theta
c  Take care of special case where image straddles a pole
      if ((dabs(long2-long1)).gt.150.0d0) then
        if (lat1.gt.0.0d0) then  ! North pole
          line_head=180.0     ! Heading is south
        else                     ! South pole
          line_head=0.0       ! Heading is north
        endif
      endif

      return
      end


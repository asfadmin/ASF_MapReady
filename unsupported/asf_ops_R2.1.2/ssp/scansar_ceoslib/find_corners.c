/* SccsId[]= @(#)find_corners.c	2.41 3/24/98 */
static char sccsid_find_corners[]= "@(#)PPfind_corners.c:2.41";

/* done by quyen dinh nguyen
 * 1/21/97
 * This subroutine will look for corners of approximate rectangular within
   a image file.
 */
#include <math.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/mman.h>

#define  PERMS 0644

/*
main(argc,argv)
int argc;
char *argv[];
{
   int i,imgsize,xloc[100],yloc[100],weight[100],ncor;
   double xoff,yoff,del_x,del_y;
   double in_xloc[4],in_yloc[4],out_xloc[8],out_yloc[8];
   if(argc<3){
     printf("Usage: %s filename filesize\n",argv[0]);
     printf("     to scan the corners of a byte square file \n");
     exit(1);
   }

   imgsize = atoi(argv[2]);
   printf(" imgsize= %d\n",imgsize);

   del_x = del_y = 1.0;
   xoff  = yoff = 0.0;

   in_xloc[0] = 0.0;
   in_xloc[1] = in_xloc[0] + del_x * imgsize;
   in_xloc[2] = in_xloc[0] ;
   in_xloc[3] = in_xloc[0] + del_x * imgsize;

   in_yloc[0] = 0.0;
   in_yloc[1] = in_yloc[0] ;
   in_yloc[2] = in_yloc[0] + del_x * imgsize;
   in_yloc[3] = in_yloc[0] + del_x * imgsize;

   
   find_corners(argv[1],&imgsize,&imgsize,&xoff,&yoff,&del_x,&del_y,
             in_xloc,in_yloc,out_xloc,out_yloc);

    printf(" Print out the corner location\n");
    for(i=0;i<4;i++) 
       printf(" corner %d : x=%f y=%f (orig x=%f y=%f)\n",
                 i,out_xloc[i],out_yloc[i],in_xloc[i],in_yloc[i]);

}
*/

int find_corners(file_name,npix,nline,xoff,yoff,del_x,del_y,
                 in_xloc,in_yloc, out_xloc, out_yloc)
char *file_name;
int  *npix,*nline;
double  *xoff,*yoff,*del_x,*del_y;
double  *in_xloc,*in_yloc,*out_xloc,*out_yloc;
{
    int  xloc[100],yloc[100],weight[100],select[100],nloc;
    double  xxloc[100],yyloc[100];
    double  min_dist, dist;
    double  xsize,ysize;
    int     i,j,min_loc,ifind_corner;

    get_corner(file_name,npix,nline,xloc,yloc,weight,&nloc);

    for(i=0;i<nloc;i++)
       fprintf(stderr,"IN FIND_CORNERS:npt=%d, x=%d y=%d\n",i,xloc[i],yloc[i]);
    
    /* Now compare with all the points */
    for(i=0;i<nloc;i++){
       xxloc[i] = (*xoff) + (*del_x) * ((double)xloc[i]);
       yyloc[i] = (*yoff) + (*del_y) * ((double)yloc[i]);
       select[i] = 0;
    }
    xsize = (*del_x)*((double)(*npix));
    ysize = (*del_y)*((double)(*nline));
  /*
    fprintf(stderr," IN FIND_CORNERS: xoff=%f yoff=%f\n",*xoff,*yoff);
    for(i=0;i<nloc;i++)
       fprintf(stderr,"IN FIND_CORNERS:npt=%d, xx=%f yy=%f\n",i,xxloc[i],yyloc[i]);
    for(i=0;i<4;i++)
       fprintf(stderr," IN FIND_CORNERS:in_npt=%d, inx=%f iny=%f\n",
              i,in_xloc[i],in_yloc[i]);
  */

    ifind_corner= 0;
    for(i=0;i<4;i++) { /* Search for only 4 points */
       min_dist = xsize*xsize + ysize*ysize;
       min_loc  = -1;
       for(j=0;j<nloc;j++){
           if(select[j]==0){
              dist = (xxloc[j]-in_xloc[i])*(xxloc[j]-in_xloc[i]) +
                     (yyloc[j]-in_yloc[i])*(yyloc[j]-in_yloc[i]) ;
        /*
              printf("Comp input=%d  found%d (%f) %f\n",i,j,dist,min_dist);
         */
              if(min_dist > dist) {
        /*
                  printf("   %d found small value of %f %f\n",j,min_dist,dist);
         */
                  min_dist = dist;
                  min_loc  = j;
              }        
           }
       }
       if(min_loc != -1) {
          out_xloc[i] = xxloc[min_loc];
          out_yloc[i] = yyloc[min_loc];
          select[min_loc] = 1;
          ifind_corner += 1;
       }
    }
    if(ifind_corner != 4){
       fprintf(stderr,"*****HELP HELP HELP, only found %d corners (%d)\n",
              ifind_corner,nloc);
       for(i=0;i<4;i++){
          out_xloc[i] = in_xloc[i];
          out_yloc[i] = in_yloc[i];
       }
    }
                       

}

int get_corner(file_name,in_x,in_y,xxloc,yyloc,wweight,nloc)
char *file_name;
int  *in_x,*in_y;
int  *xxloc,*yyloc,*wweight,*nloc;
{
    int  ifd, nread;
    int  ntotal,ntotal_small;
    int  byte_start,nline,nbyte,ncorners,ncount;
    int  i,j,k;
    int  box_size,xfac;
    int  ifd2;
    int  iline,ipix;
    char *in_img,*img ;
    int  in_byte,in_line,nsearch;
    int  iline_min,ipix_min,iline_max,ipix_max;
    int  xloc[100],yloc[100],weight[100],num_corners;

    int outfd;

    /* Open the file */
    if((ifd = open(file_name,O_RDONLY,0)) < 0){
       printf("Error in open the input file %s\n",file_name);
       exit(1);
    }

    ntotal = (*in_x)*(*in_y);
    byte_start = 0;
    if( (in_img = (char *) mmap(0,ntotal,
             PROT_READ, MAP_SHARED, ifd, byte_start)) == NULL){
        fprintf(stderr," the input file= %d (%s) with %d offset (size=%d)\n",
                 ifd,file_name,byte_start,ntotal);
        fprintf(stderr," Can not memory mapped the input file \n");
        exit(1);
    }

    /* Reduce the image to by factor of 8 */
    xfac = 8;
    xfac = (*in_x)/400;
    fprintf(stderr," the xfactor = %d \n", xfac);
    in_byte = *in_x;
    in_line = *in_y;
    nline = (*in_y) / xfac;
    nbyte = (*in_x) / xfac;
    ntotal_small = nline*nbyte;
    if( !(img = (char *) malloc(ntotal_small))) {
        fprintf(stderr," Error in allocating memory\n");
        exit(1);
    }
    reduce_img( in_img, *in_x, *in_y, img, xfac);

    fprintf(stderr," the dimension is %d %d \n",nbyte, nline);
    /*
    outfd=creat("tmp.dat",PERMS);
    write(outfd,img,ntotal_small);
    */


    /* Scan the four corners within this small image */
    ncorners=0;
    for(iline=0;iline<nline && ncorners<100;iline++){ /* for each image line */
       /* Search from the left side */
       j=0;
       while(j<nbyte && img[iline*nbyte+j] == 0) j +=1 ;
       if( j < nbyte) {   /* found the first nonzero point */
          if( check_corner(img,nbyte,nline,j,iline,&ncount) == 1) 
                update_corner(xloc,yloc,weight,&ncorners,j,iline,ncount);
          k=nbyte-1;
          while(img[iline*nbyte+k] == 0 && k > j ) k -= 1;
          if ( k > j) {
             if( check_corner(img,nbyte,nline,k,iline,&ncount) == 1) {
                update_corner(xloc,yloc,weight,&ncorners,k,iline,ncount);
             }
          }
       }
    }

    *nloc = ncorners;

    fprintf(stderr," Found the %d corners\n",ncorners);

    /* Refine the location */
    num_corners=0;
    nsearch = 3;
    for(i=0;i<ncorners;i++){
      iline_min = ((yloc[i]-nsearch)*xfac > 0) ? (yloc[i]-nsearch)*xfac : 0;
      ipix_min  = ((xloc[i]-nsearch)*xfac > 0) ? (xloc[i]-nsearch)*xfac : 0;
      iline_max = ((yloc[i]+nsearch)*xfac-1 < in_line) ? (yloc[i]+nsearch)*xfac-1 : in_line-1;
      ipix_max  = ((xloc[i]+nsearch)*xfac-1 < in_byte) ? (xloc[i]+nsearch)*xfac-1 : in_byte-1;

       for(iline=iline_min; iline<iline_max; iline++){
           /* Search from the left side */
           j = ipix_min;
           while(j< (ipix_max+1) && in_img[iline*in_byte+j] == 0 ) j += 1;
           if( j < (ipix_max+1)) {     /* found the first non zero point */
             if(check_corner(in_img,in_byte,in_line,j,iline,&ncount) == 1)
                update_corner(xxloc,yyloc,wweight,&num_corners,j,iline,ncount);
             k = ipix_max-1;
             while( in_img[iline*in_byte+k] == 0 && k > j) k -= 1;
             if( k>j ){
               if(check_corner(in_img,in_byte,in_line,k,iline,&ncount)==1){
                    update_corner(xxloc,yyloc,wweight,&num_corners,
                                  k,iline,ncount);
                }
             }
           }
       }

    }

    *nloc = num_corners;
    
}


/* Reduce the image */
int reduce_img (input,in_pix,in_line,output,xfac)
char *input,*output;
int  in_pix,in_line,xfac;
{
  int i,j,k,l,ix,jx;
  int out_pix,out_line,ncount;

  out_pix = in_pix/xfac;
  out_line = in_line/xfac;
  for(j=0; j<out_line; j++)
     for(i=0; i<out_pix; i++){
        ncount = 0;
        ix = i*xfac; jx = j*xfac;
        for(k=jx; k<jx+xfac; k++)
           for(l=ix; l<ix+xfac; l++)
              ncount = (input[k*in_pix+l] == 0)? ncount : ncount+1;
        output[j*out_pix+i] = (ncount == 0) ? 0 : 1;
        /* output[j*out_pix+i] = (ncount == xfac * xfac) ? 1 : 0; */
     }
}


/* update the corner lists */
int update_corner(xloc,yloc,weight,ncorners,ip,il,ncount)
int *xloc,*yloc,*weight,*ncorners;
int ip,il,ncount;
{
  int i,del_x,del_y,dist,min_dist;

  min_dist = 9;
  for(i=0;i< (*ncorners); i++){
    del_x = ip - xloc[i];
    del_y = il - yloc[i];
    dist = del_x*del_x + del_y*del_y;
    if(dist < min_dist) {
        if(ncount < weight[i]) {
           xloc[i] = ip; 
           yloc[i] = il;
           weight[i] = ncount;
        }
        return; 
    }
  }
  xloc[*ncorners] = ip;
  yloc[*ncorners] = il;
  weight[*ncorners] = ncount;
  *ncorners = (*ncorners) + 1;


}

/* Check to see if the point is a corner */
int check_corner(img,nbyte,nline,ip,il,nweight)
char *img;
int  nbyte,nline,ip,il,*nweight;
{
  int box_size,half_box,icorner;
  int i,j;
  int imin,imax,jmin,jmax;
  int ncount, nlimit;

  /* Set up the test */
  box_size = 9;
  half_box = box_size/2;
  nlimit = (half_box+1)*(half_box+1);
  for(i=half_box-1; i>0; i--) nlimit += i;

  /* Look the box around */
  imin = (ip-half_box>0)       ? ip - half_box : 0;
  imax = (ip+half_box<nbyte-1) ? ip + half_box : nbyte-1;
  jmin = (il-half_box>0)       ? il - half_box : 0;
  jmax = (il+half_box<nline-1) ? il + half_box : nline-1;

  /* Found the 1's around the point */
  ncount = 0;
  for(i = imin; i<= imax; i++)
     for( j = jmin; j<= jmax;  j++)
         ncount = (img[j*nbyte+i] == 0) ? ncount : ncount+1 ;
  *nweight = ncount;

  /* Check to see if this point is a corner */

  if( ncount < nlimit) {
      icorner = 1;
  } else {
      icorner = 0;
  }
  
  return icorner;
}

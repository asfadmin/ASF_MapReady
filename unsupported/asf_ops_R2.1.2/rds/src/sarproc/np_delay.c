static char sccsid_np_delay_c[] =
    "@(#)np_delay.c	1.2 96/04/09 19:13:29";

#include <math.h>

/* done by quyen dinh nguyen 
 * 3/15/90:
 * 1/24/91: modified to correct the number of pulses delay.
 *          The delay should be only from the number of frame delay
 *          and the reference length only (the deskew is already
 *          accounted in the delay frame number).
 */

/* np_delay( nlook, frame_delay, bufrdlen, -----------------------------
     prf, pbwx, fr_near, totalskew, azspacing, azsample)

   This routine will calculate the number of pulses delay to the
   beam center of the first output point. 
   The required parameters are: the number of delay frames, the
   number of offset pulses between frames, the reference function 
   length and the total deskew amount.

 INPUT:
   nlook        int*4   the number of looks (either 1 or 4).

   frame_delay  int*4   the output delay frame number (between 1 and 8)
   
   bufrdlen     int*4   the range line number offset between frames
                        (have to be even) (INPUT,CTC).
 
   prf          float   the Pulse repition bandwidth (Hz).

   pbwx         float   the effective processing bandwidth (Hz)

   fr_near      float   the Doppler rate of the 1st output range
                        bins. (Hz/sec)

   totalskew    float   the maximum deskew amount for the whole scene.

   azspacing    double  the azimuth spacing between range lines
                        (meter).
   
   azsample     double  the azimuth output spacing (meter).
                        The value can be either 12.5 meter or
                        azspacing meter.

 OUTPUT:
   np_delay     int*4   the delay pulse number from the first input
                        pulse to the pulse at the beam center of
                        the first output point.

*/

int np_delay( nlook, frame_delay, bufrdlen,
              prf, pbwx, fr_near, totalskew, azspacing, azsample)
int   nlook, frame_delay, bufrdlen;
float totalskew, prf, pbwx, fr_near;
double azspacing, azsample;
{
 int   ie,ireflength,iskew,idelay;
 float e;

 ireflength = (int)( pbwx*prf/((float)(fabs(fr_near))) );
 if(nlook == 1){
    idelay =  (frame_delay-1)*bufrdlen + ireflength/2;
 } else if(nlook == 4) {
    iskew = totalskew;
    e = (float)( azspacing/azsample);
    ie = (int)(8192.0/e);
    e = 1.0/((float)(ie)/8192.0);

    idelay =  (frame_delay-1)*bufrdlen - ireflength/4 ;

 } else {
    idelay = -1;
 }
 printf(" in np_delay.c: frame_delay=%d,(buflen=%d) ireflength=%d, np_delay=%d\n",
       frame_delay,bufrdlen,ireflength,idelay);
 return(idelay);
}


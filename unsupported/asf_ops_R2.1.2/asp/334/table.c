/* Alaska SAR Processor (ASP) %W% %E% %U% */
#include <math.h>
/* done by quyen dinh nguyen
 * 10/2/89
 * 10/6/89: one single table for I,Q. (8192 entries)
 *          This specification is from Ben Charny
 * 1/16/90: modified the code 
 * 8/9/91:  the spec for ERS format is changed again
 *          (the order is I and then Q). In addition,
 *          modify to scale the maximum to 15bit(signed-
 *          extended)(to reduce the input singal)
 */



/* lookup_ers( bias, gain, phase, inbit, table)  ----------

   This routine will generate the lookup table for the I channel
   and the Q channel.

 INPUT:
   bias    vec of float   The bias of the input signal in ADC 
                          quantization units. Vector of 2 for 
                          the channel I and channel Q respectively.
  
   gain    vec of float   The gain of channel I and Q respectively.
                          The unit is in ADC quantization units.
                          
   phase   float          The relative phase between I and Q channel
                          in DEG. Note phase is assumed to be near
                          90 degree. (not zero)
   
   inbit   int            The number of bits for each input sample
                          (per channel). Assume to be 5 bits for JERS-1.
                       
 OUTPUT:
   table    vec of short int  The lookup table for maping I and Q signals
                              to R and I signals. 
                              The I and Q are combined to 13 bits,
                              formatted as below:
                     < inbit for I >< 0 > < inbit for Q >< 0 > <B>
                     |_______6 bits_____| |______6 bits______| 

                              where B is 0 for output R values and 
                              B is 1 for output I values.

*/

lookup_ers( bias, gain, phase, inbit, table)

float bias[2], gain[2], phase;
short int table[1];
int   inbit;

{
 double     pi, relphase;
 float      cosrelphase,tanrelphase;
 float      factor[2],xi,xq,yi,yq,scale;
 float      xi_gain,xq_gain;
 int        n_entry,i,q,n_valid_entry,ni_shift,nq_shift;
 int        iloc;

 n_entry = 1 << 13;
 ni_shift = 7+(6-inbit);
 nq_shift = 1+(6-inbit);
 n_valid_entry = (1<<inbit);
 for(i=0; i<n_entry; i++) table[i] = 0;

 scale = (float)(1<<14)/(float)(1<<(inbit-1));

 if(gain[0] < gain[1]) {
    xi_gain = (gain[0] == 0.0)? 1.0 : gain[1]/gain[0];
    xq_gain = 1.0;
 } else {
    xi_gain = 1.0;
    xq_gain = (gain[1] == 0.0)? 1.0 : gain[0]/gain[1];
 }
 factor[0] = xi_gain  * scale;
 factor[1] = xq_gain  * scale;

 pi = 4.0*atan(1.0);
 relphase = (phase - 90.0)*pi/180.0;
 cosrelphase = (float)(cos(relphase));
 tanrelphase = (float)(tan(relphase));

/*
 printf(" Welcome to table subroutine\n");
 printf(" scale=%f, gain= %f %f\n",scale,gain[0],gain[1]);
 printf(" phase=%f, relphase=%f, cosrelphase=%f tanrelphase=%f\n",
          phase,(float)relphase, cosrelphase,tanrelphase);
*/

 for(i=0; i<n_valid_entry; i++){
    for(q=0; q<n_valid_entry; q++){
        xi = (float)(q);
        xq = (float)(i);
        yi = (xi - bias[0])*factor[0];
        yq = (xq - bias[1])*factor[1];
        yq = yq/cosrelphase - yi*tanrelphase;
        if(yi > 32767.0)  yi= 32767.0;
        if(yi < -32768.0) yi= - 32768.0;
        if(yq > 32767.0)  yq= 32767.0;
        if(yq < -32768.0) yq= - 32768.0;
        iloc = (i << (ni_shift)) + (q << (nq_shift)) ;
        table[iloc] = (short int) yi;
        table[iloc+1] = (short int) yq; 
     }
  }

}

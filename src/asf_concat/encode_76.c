/******************************************************************************
FUNCTION:      encode_76 

PURPOSE:       Encode_76 encodes a -7,6 shift to ends encoding scheme. 

PROGRAM HISTORY:
Version    Date     Author              Change Request
-------    -----    -------------       --------------
  1.0      07/92    T. Mittan           Original implementation

*/

#include "asf.h"
#include "las.h"

#define   SHIFT_DOWN   0x000D            /*  4 		*/
#define   M6           0x007F            /*  7 		*/
#define   M5           0x003E            /*  6 		*/
/*
#define   M4           0x0005                4 		
#define   M3           0x0006                4 		
#define   M2           0x0007                4 	
*/
#define   M1           0x0004          /*    3 		*/
/*
#define   ZERO         0x0000                2 		
#define   P1           0x000A                4 		
#define   P2           0x000B                4 		
#define   P3           0x000C                4 		
#define   P4           0x001C                5 		
#define   P5           0x001D                5 		
#define   P6           0x001E                5 		
*/
#define   P7           0x007E            /*  7 		*/
#define   SHIFT_UP     0x0004            /*  4 		*/


void encode_76(const unsigned char *inpt,unsigned char *outpt,int ns,int *point,int width)
{
    int output_bit;			/* number of output bits	*/
    int pointer;			/* array pointer		*/
    int temp;				/* temporary variable		*/
    int temp1;				/* temporary variable		*/
    int temp2;				/* temporary variable		*/
    const unsigned char  *in_ptr;		/* input buffer			*/
    unsigned char  *out_ptr;		/* input buffer			*/
    unsigned int output_buffer = 0L;	/* output bit buffer		*/
    int      i, j, k;			/* array counters		*/

      pointer = 0;
      out_ptr = outpt;
      output_buffer |= (unsigned long)1 << 24 ;
      output_buffer |= (unsigned long)*(inpt) << 16 ;
      output_bit = 16;  
      *outpt = output_buffer >> 24; 
      outpt++;
      pointer++;
      output_buffer <<= 8; 
      output_bit -= 8; 
      temp = width;
      in_ptr = inpt;
      temp1 = *inpt;
      temp2 = 1;
      /* for each byte of data type encode NS bits of data
      --------------------------------------------------*/
      for (j = 0; j < width; j++)
      {
      for (inpt = in_ptr +j + temp, i = temp2; i < ns; i++, inpt +=width) 
        {
           k = *inpt - temp1;
	   temp1 += k;
          while (k < -6)
          {  
          output_buffer |= (unsigned long) SHIFT_DOWN << (28-output_bit);  
          output_bit += 4;  
          if (output_bit >=16)  
               {	
               *outpt = output_buffer >> 24; 
	       outpt++;
	       pointer++;
	       if (pointer > (ns * width))
		  break;
               output_buffer <<= 8; 
               output_bit -= 8; 
               } 
          k += 13;
          }
          while (k > 7)
          {  
          output_buffer |= (unsigned long) SHIFT_UP << (28-output_bit);  
          output_bit += 4;  
          if (output_bit >=16)  
               {	
               *outpt = output_buffer >> 24; 
	       outpt++;
	       pointer++;
	       if (pointer > (ns * width))
		  break;
               output_buffer <<= 8; 
               output_bit -= 8; 
               } 
          k -= 13;
          }
             if (k == 0)
               output_bit += 2;  
             else
             if (k < 0)
               {
               if (k == -1)
                  {
                  output_buffer |= (unsigned long)  M1 << (29-output_bit);  
                  output_bit += 3;  
                  }
               else
               if (k > -5)
                  {
                  output_buffer |= (unsigned long)(k + 9) <<(28-output_bit); 
                  output_bit += 4;  
                  }
               else
               if (k == -5)
                  {
                  output_buffer |= (unsigned long) M5 << (26-output_bit);  
                  output_bit += 6;  
                  }
               else
                  {
                  output_buffer |= (unsigned long) M6 << (25-output_bit);  
                  output_bit += 7;  
                  }
               }
             else
             if (k < 4)
               {
               output_buffer |= (unsigned long) (k +9) << (28-output_bit); 
               output_bit += 4;  
               }
             else
             if (k < 7)
               {
               output_buffer |= (unsigned long) (k + 24) << (27-output_bit);  
               output_bit += 5;  
               }
             else
               {
               output_buffer |= (unsigned long) P7 << (25-output_bit);  
               output_bit += 7;  
               }
        if (output_bit >=16)  
          {	
          *outpt = output_buffer >> 24; 
	  outpt++;
	  pointer++;
	       if (pointer > (ns * width))
		  break;
          output_buffer <<= 8; 
          output_bit -= 8; 
          } 
        }
      if (pointer > (ns * width))
	  break;
      temp = 0;
      temp2 = 0;
      }
   for (i = 0; i < 7; i++)
      {
      output_buffer |= 0  << (24 - output_bit);
      output_bit += 8;  
      if (output_bit >=16)  
         {	
         *outpt = output_buffer >> 24; 
	 outpt++;
	 pointer++;
         output_buffer <<= 8; 
         output_bit -= 8; 
         } 
      }

/* if compressed line is longer than uncompresed line
   place 0 in first byte and copy input to output
----------------------------------------------------*/

   if (pointer > (ns * width))
     {
     inpt = in_ptr;
     outpt = out_ptr;
     *outpt = 0;
     outpt++;
     for (j = 0; j < width; j++)
      {
      for (i = 0; i < ns; i++, inpt++,outpt++) 
	*outpt = *inpt;
      temp = 0;
      }
    pointer = ns * width + 1;
    }

*point = pointer;
}

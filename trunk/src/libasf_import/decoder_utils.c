/*
Utilites to facilitate data decoding.
Contains bit-shift routines, as well as
metadata handling.
*/
#include "decoder.h"

/********************************
Check_sync:
  Checks synchronization marker values against the given
true value.
*/
void check_sync(const unsigned char *sync,int nBytes,const unsigned char *trueVal)
{
  static int totalErrors=0,totalChecked=0;
  int i;
  for (i=0;i<nBytes;i++) {
    if (sync[i]!=trueVal[i]) {
      totalErrors++;
      asfPrintStatus("   Expected sync %x, got %x!  (%f error rate)\n",
                     trueVal[i],sync[i],((float)totalErrors)/totalChecked);
    }
  }
  totalChecked+=nBytes;
}
/********************************
DetermineSatellite:
  Determines which satellite family created
this file, by examining the first sync code.
*/
sat_type determine_satellite(const char *binName)
{
  sat_type ret=sat_unknown;
/*Open file and read sync*/
  FILE *f=FOPEN(binName,"rb");
  unsigned char sync[3];
  FREAD(sync,3,1,f);

/*Switch to find which satellite created this file*/
  if (sync[0]==0xFA)
    ret=sat_ers;
  else if (sync[0]==0xFF)
    ret=sat_jrs;
  else if (sync[0]==0x1A)
    ret=sat_rsat;
  else {
    asfPrintStatus("   ERROR! Unknown satellite sync code %02x %02x %02x found!\n",
                    sync[0],sync[1],sync[2]);
  }

/*Close file and return*/
  FCLOSE(f);
  return ret;
}

/********************************
db2amp:
  Converts a gain factor in dB into
a scale factor in amplitude.
Since
  dB=10*logbase10(power)
then  db=10*log(power)/log(10)
  db/10*log(10)=log(power)
  exp(db/10*log(10))=power=amp*amp
  sqrt(exp(db/10*log(10)))=sqrt(power)=amp
*/
float db2amp(float dB)
{
  return sqrt(exp(dB/10.0*log(10.0)));
}

/*********************************
realRand: Return a pseudorandom real number on [0,1)*/
double realRand(void)
{
  return ((double)(0x3fff&rand()))/(0x3fff);
}
/*********************************
scaleSamples:
  Amplifies (or attenuates) given
samples by given factor.
*/
void scaleSamples(bin_state *s,iqType *buf,int start,int num,float factor)
{
  int i;
  for (i=start;i<start+num;i++)
  {
    buf[i*2]=factor*((float)buf[i*2]-s->I_BIAS)+s->I_BIAS;
    buf[i*2+1]=factor*((float)buf[i*2+1]-s->Q_BIAS)+s->Q_BIAS;
  }
}

/*Return the number of bit errors between
the bytes A and B.
*/
int num_bit_errors(unsigned char a,unsigned char b)
{
  int numErrs=0;
  int bitNo;
  int xor=a^b;/*Compute bitwise XOR, and count the one bits*/
  for (bitNo=0;bitNo<8;bitNo++)
    numErrs+=((xor>>bitNo)&1);
  return numErrs;
}

/*Create a bit-error table, where table[(a<<8)+b]==num_bit_errors(a,b)
for any unsigned characters a and b.  The table can later be FREE'd.
*/
unsigned char *createBitErrorTable(void)
{
  unsigned char *table=(unsigned char *)MALLOC(256*256);
  int a,b;
  for (a=0;a<256;a++)
    for (b=0;b<256;b++)
      table[(a<<8)+b]=num_bit_errors(
        (unsigned char)a,(unsigned char)b);
  return table;
}



/***********************************
extractBits:
  Pulls the specified number of
bytes from the given bit offset in input.
*/
void extractBits(signalType *in,int bitStart,int nBytes,signalType *out)
{
  int i;
/*Advance as many bytes as we can.*/
  in+=bitStart/8;
  bitStart-=(bitStart/8)*8;
/*Loop over output bytes, reading appropriate location from input.*/
  for (i=0;i<nBytes;i++)
    out[i]=(in[i]<<bitStart) | (in[i+1]>>(8-bitStart));

}






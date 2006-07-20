/*ERS Auxiliary data decoding utilites.*/
#include "asf.h"
#include "decoder.h"
#include "auxiliary.h"

/********************************************************************
 * Information about ERS_raw_aux for function: ERS_decodeAux()
 * Description of the ERS aux data header in raw form
 * View only using a fixed width font

idhtPacket idhtSubCount
 |-------- |-------- |------------------- idht[8] --------------------------->>
 0........ 1........ 2........ 3........ 4........ 5........ 6........ 7.......

                     formatCode orbitNo
>>--idht[8]-(cont)-- |-------- |-------- |----------- onBoardTime -------------
 8........ 9........10........11........12........13........14........15.......


 |activity |-flags-- |-------------- formatCount ----------- |------ swst -----
16........17........18........19........20........21........22........23.......

 |------ prf ------- |calAtten |rfAtten-
24........25........26........27........


~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// Here's the old structure (which used bitfields and broke on Linux) //
// Uchar is unsigned char
// SAT_ULONG is unsigned long (big endian format)
// SAT_USHORT is unsigned short (big endian format)

//Auxiliary data starts 6 bytes from frame start, and has this format://
typedef struct
{
  Uchar idhtPacket:8;
  Uchar idhtSubCount:8;
  Uchar idht[8];
  Uchar formatCode:8;
  Uchar orbitNo:8;
  struct SAT_ULONG onBoardTime;
  Uchar activity:8;
  Uchar flags:8;
  struct SAT_ULONG formatCount;
  struct SAT_USHORT swst;
  struct SAT_USHORT prf;
  Uchar calAtten:8;
  Uchar rfAtten:8;
} ERS_raw_aux;

********************************************************************/

/********************************************************************
 * DecodeAux:
 * Converts a raw satellite downlink (raw_aux) with nasty bit-wise fields and
 * odd meanings to an interpreted structure (aux) to clean floating-point
 * fields and clear meanings.  */
void ERS_decodeAux(unsigned char *in,ERS_aux *out)
{
  unsigned int raw_aux[ERS_datPerAux];
  int ii;

  for (ii=0; ii<ERS_datPerAux; ii++) {
    raw_aux[ii] = (unsigned int)in[ii];
  }
  out->isImaging   = (0x5 == (0x07&(raw_aux[16]>>5)));
  out->onBoardTime =   raw_aux[12]<<24 | raw_aux[13]<<16
                     | raw_aux[14]<<8  | raw_aux[15];
  out->formatCount =   raw_aux[18]<<24 | raw_aux[19]<<16
                     | raw_aux[20]<<8  | raw_aux[21];
  out->rfAtten     = raw_aux[27];
  out->dwp_code    = raw_aux[22]<<8 | raw_aux[23];
  out->dwp         = out->dwp_code * CONF_ERS_timeBase;
  out->prf_code    = raw_aux[24]<<8 | raw_aux[25];
  out->prf         = 1.0 / ((out->prf_code+2)*CONF_ERS_timeBase);
}

/********************************************************************
 * auxPrint:
 * Writes the "important" values from an auxiliary data record to the given
 * file.  */
void ERS_auxPrint(ERS_aux *aux,FILE *f)
{
/*  fprintf(f,
 *          "IsImaging=%d  OnBoardTime=%d\n"
 *          "formatCount=%d, rfAtten=%d\n"
 *          "dwp=%f   prf=%f\n",
 *          aux->isImaging, aux->onBoardTime,
 *          aux->formatCount, aux->rfAtten,
 *          aux->dwp, aux->prf);
 */
}

/********************************************************************
 * auxUpdate:
 * Updates values in satellite record based on values in given raw auxiliary
 * data record.  */
void ERS_auxUpdate(ERS_aux *aux,bin_state *s)
{
  double outputDelay=0;/*Output pulse delay (overridden below)*/

/* Determine if it's ERS-1 or ERS-2. We do this using the manual reciever gain
 * value, which seems like a dumb idea; but works. The two satellites have
 * different circuitry delay values for transmission. The values were checked
 * using the Delta Junction corner reflectors. E1 was sub-pixel accurate; but
 * E2 was off by about 20 meters.
 */

  if (aux->rfAtten==30) {
    /*Reciever attenuation at 30-- is ERS-1*/
    strcpy(s->satName,"ERS1");
    outputDelay=CONF_ERS1_rangePulseDelay;
  }
  else if (aux->rfAtten==38) {/*Reciever attenuation at 38-- is ERS-2*/
    strcpy(s->satName,"ERS2");
    outputDelay=CONF_ERS2_rangePulseDelay;
  }
  else
    asfPrintWarning("Can't tell if this is ERS-1 or ERS-2!!  RF Atten=%d\n",
                    aux->rfAtten);

/*Compute slant range to first pixel:*/
  s->prf=aux->prf;
  s->prf_code=aux->prf_code;
  s->dwp=aux->dwp;
  s->dwp_code=aux->dwp_code;

/*9=# of pulses in air*/
  s->range_gate=s->dwp+9.0/s->prf-outputDelay;

  s->time_code=aux->onBoardTime;

  ERS_auxPrint(aux,stdout);
}

/********************************************************************
 * auxAGC_window:
 * call updateAGC_window with satellite AGC (1.0) and window position for this
 * line.  */

#define SWST_UNIT 0.00000021094
/* unit reported window position corresponds to 210.94 ns */

void ERS_auxAGC_window(bin_state *s,ERS_aux *aux)
{
/*  updateAGC_window(s,1.0,aux->dwp*s->fs);*/
  updateAGC_window(s,1.0,aux->dwp_code*SWST_UNIT*s->fs);
}

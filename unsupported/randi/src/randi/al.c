/* al.c                RADARSAT Modification
* interface for the raw data scan utility.
* decodes the different parts of the formats and grants access to them
*
* first setup 07-18-1994 Hans-Joerg Wagner
* first release 07-25-1994 Hans-Joerg Wagner
*
* RADARSAT Modification
* by Christian Fischer
* 09-25-95
*
*/
#include <stdio.h>
#include <memory.h>
#include <malloc.h>
#include "drdapi.h"    /* for type SCcomplex */
#include "randi.h"

/* global Variables to hold the scanned data until it is retrieved */
union format 	g_myForm;    			/* buffer to read Format */

/*SCcomplex  var.I,var.Q char*/ 
SCcomplex g_ApplicationDataFieldBuffer[MAX_FRAME_NO*DATAFIELD_LENGTH];
int g_AuxSyncPatternCorrect;

/*DA Conversion, see REV F, p. 44*/
char g_DACtable[]={0x0,0x1,0x2,0x3,0x4,0x5,0x6,0x7,
                  -0x8,-0x7,-0x6,-0x5,-0x4,-0x3,-0x2,-0x1};

/*Debug: Manual says F0F0CCAA, in testdata it is CCAAF0F0   ERROR? */
uchar g_EphSyncPattern[]={0xcc,0xaa,0xf0,0xf0};
uchar g_AuxSyncPattern[]={ 0x35, 0x2e, 0xf8, 0x53};
#define AuxSyncPatternLength 4


/**********************************************************************
* alInit 
* initializes the levels below and allocates the memory for
* the different data blocks
*/
int alInit(pszFileName,luStart,luEnd,bSave)
char *pszFileName;
ulong luStart,luEnd;
int bSave;
{
/* reinitialize global variables */

g_stat.format.luImMissCount = 0L;

return foaInit(pszFileName,luStart,luEnd,bSave);
}

/**********************************************************************
* alExit
* frees the allocated memory space and calls the exit routine of the
* level below
*/
void alExit()
{
foaExit();
}

/**********************************************************************
* alNextBlock
* reads the next format data and converts it into the proper fields
* Then it sets the different boolean values for new data.
* returns 0 if no new Format could be found.
*/
int alNextBlock()
{

static ulong luFilePos=0;
ulong luFilePosOld;
ulong luTmp;
ushort uTmp;
static uint PrevImFoCount; 
uint ImFoCount;  /* is each time read new*/
int i;
static int PrevImFoCountValid=0;

luFilePosOld=luFilePos;

/* Default is the use of foaGetNextFormat() */
if (!g_CareFree)
  luFilePos = foaGetNextFormat(&g_myForm);
else 
  luFilePos = foaGetNextFormat_CareFree(&g_myForm);
  
/* return if no more format */
if (!luFilePos)
	return 0;
/* is the  format counter  continous? */
ImFoCount=foaGetFormatCount();
if (PrevImFoCountValid) 
  {
  if ((PrevImFoCount+1) != ImFoCount)
    {
     g_stat.format.luImMissCount+= (uchar)(ImFoCount-PrevImFoCount-1);
        Report(lvFormat,"Missing %u Formats beween %u and %u",
                (uchar)(ImFoCount-PrevImFoCount-1), PrevImFoCount, ImFoCount );
     g_RewriteTime=1;
    }
  }
else
     PrevImFoCountValid=1;
PrevImFoCount=ImFoCount;

/* is the AuxSyncMarker correct ?*/
g_AuxSyncPatternCorrect=1;
for (i=0;i<AuxSyncPatternLength;i++)
  if  (g_myForm.s.AuxSyncMarker[i] != g_AuxSyncPattern[i])
if (!g_AuxSyncPatternCorrect)
  Report(lvFormat,"Aux Sync Marker incorrect in Format %d, File Offset %lu",
         ImFoCount,luFilePosOld);

return 1;
}




/* Access Functions */


/********************************
* returns, if the Aux Sync Marker of the Format just read
* is correct
*/
int alAuxSyncPatternIsCorrect()
{
  return g_AuxSyncPatternCorrect;
}


/*******************************
* alGetAuxData(AuxData, Auxdatavalid) 
*/

void alGetAuxData(ad,adv,EphData)
struct AUX *ad;
struct AUXDATAVALID *adv;
struct SAT_USHORT (*EphData)[NoOfEphWords];

{
  static uchar EphCounter=1;
  static int EphDatachanged;
  static int EphSynchrofound;
  static int alreadyWroteEphData=0;
  static int Firsttime=1;
  static struct  AUX pad; /*previous Values of aux Data */
  static struct SAT_USHORT OldEphemerisData[NoOfEphWords+1];
  uchar i;
  memcpy(ad,&(g_myForm.s),sizeof(struct AUX));
  
  /* Detect changes in the aux Data, if yes, write the new Values into 
   the aux File */

  if (! (ad->PayloadStatus.Msb == pad.PayloadStatus.Msb) && 
     (ad->PayloadStatus.Lsb == pad.PayloadStatus.Lsb))
     adv->PSval=1;
  else adv->PSval=0;
  
  if (! (ad->ReplicaAGC==pad.ReplicaAGC))
     adv->Rval=1;
  else adv->Rval=0;

  if (!(ad->CALNAttenuatorSetting == pad.CALNAttenuatorSetting))
     adv->Attenval=1;
  else adv->Attenval=0;

  if (!(ad->PulseWaveformNo == pad.PulseWaveformNo))
     adv->PWval=1;
  else adv->PWval=0;

  if (!( (ad->Temperature.Msw.Msb == pad.Temperature.Msw.Msb)&&
        (ad->Temperature.Msw.Msb == pad.Temperature.Msw.Msb)&&
	 (ad->Temperature.Msw.Msb == pad.Temperature.Msw.Msb)&&
        (ad->Temperature.Msw.Msb == pad.Temperature.Msw.Msb)))
     adv->Tempval=1;
  else adv->Tempval=0;

   if (! ((ad->BeamSequence.Msb == pad.BeamSequence.Msb) &&
     (ad->BeamSequence.Lsb == pad.BeamSequence.Lsb)) )
     adv->BSqval=1;
  else adv->BSqval=0;

/* Ephemeris must is handled different 
 * (data is spread over 55 Formats)  */
  adv->Eval=0;
  if (EphCounter==1)
    if ((ad->Ephemeris.Msb==g_EphSyncPattern[0])&&(ad->Ephemeris.Lsb==g_EphSyncPattern[1]))
      EphCounter=2;
    else {
      EphSynchrofound=0;
      EphCounter=1;
      /* only report when already Ephemeris Data was written and then
      * Synchronisation lost */
      if (alreadyWroteEphData)
        Report(lvSync,"Synchronisation in Ephemeris lost at Format %lu",foaGetFormatCount());
    }
  else
    if (EphCounter==2)
      if ((ad->Ephemeris.Msb==g_EphSyncPattern[2])&&(ad->Ephemeris.Lsb==g_EphSyncPattern[3]))
        {
          EphCounter=3;
        }  
      else {
        EphSynchrofound=0;
        EphCounter=1;
        /* only report when already Ephemeris Data was written and then
         * Synchronisation lost */
        if (alreadyWroteEphData)
          Report(lvSync,"Synchronisation in Ephemeris lost at Format %lu",foaGetFormatCount());
      }
    else   /* full Ephemeris data  was transmitted*/ 
    {
      if (EphSynchrofound)
      { /*if last time Synchronisation was found and this
         *time again, the Ephemeridisdata between is valid. It has
         * only to be written if it changed, or the first time*/
         EphDatachanged=0;
         i=3;
         do {
           if (! (((*EphData)[i].Msb==OldEphemerisData[i].Msb)&
                  ((*EphData)[i].Lsb==OldEphemerisData[i].Lsb)) )
                EphDatachanged=1;     
           i++;
         }       
         while ( (!EphDatachanged)&&(i<=NoOfEphWords) );
         if ((EphDatachanged)||(!alreadyWroteEphData))
         {   
           /* yes, data is valid and changed, so write new */
           adv->Eval=1;
           alreadyWroteEphData=1;
           EphSynchrofound=0;
           for (i=1;i<=NoOfEphWords;i++) {
             OldEphemerisData[i].Msb=(*EphData)[i].Msb;
             OldEphemerisData[i].Lsb=(*EphData)[i].Lsb;
           }
         }
      }   
    /*Synchro was ok, now read in each Time a Dataword for changes */
    (*EphData)[EphCounter].Msb=ad->Ephemeris.Msb;
    (*EphData)[EphCounter].Lsb=ad->Ephemeris.Lsb; 
    EphCounter++;
    if (EphCounter==56)  /*end of the 55 Words reached?*/
      {
      EphCounter=1;
      EphSynchrofound=1;
      }
    }
      

  if (!(ad-> NoOfBeams== pad. NoOfBeams))
     adv->NBval=1;
  else adv->NBval=0;

  if (!(ad-> ADCSamplingRate== pad.ADCSamplingRate))
     adv->ADCval=1;
  else adv->ADCval=0;
 
  if (!(ad-> PulseCount1 == pad. PulseCount1))
     adv->PC1val=1;
  else adv->PC1val=0;

  if (!(ad-> PulseCount2== pad.PulseCount2))
     adv->PC2val=1;
  else adv->PC2val=0;

  if (!((ad->PRFBeam1 == pad.PRFBeam1)&&(ad->PRFBeam2 == pad.PRFBeam2)))
     adv->PRFval=1;
  else adv->PRFval=0;

  if (!(ad->BeamSelect == pad.BeamSelect))
     adv->BSlval=1;
  else adv->BSlval=0;

  if (!((ad-> RxWindowStartTime1== pad.RxWindowStartTime1)&&
        (ad-> RxWindowStartTime2== pad.RxWindowStartTime2)))
     adv->RXSval=1;
  else adv->RXSval=0;

  if (!((ad-> RxWindowDuration1== pad.RxWindowDuration1))&&
        (ad-> RxWindowDuration2== pad.RxWindowDuration2))
     adv->RXDval=1;
  else adv->RXDval=0;

  adv->Attval=0;
  for (i=0;i<6;i++)
     if (!(ad->Attitude[i] == pad.Attitude[i]))
       adv->Attval=1;
  
  if (!(ad->RxAGCSetting==pad.RxAGCSetting))
     adv->RAGCval=1;
  else adv->RAGCval=0;

  if (Firsttime) /*in the beginning write everything you know in
                   the  aux File, later only changes */
  {
  adv->PSval=1;
  adv->Rval =1;
  adv->Attenval =1;
  adv->PWval =1;
  adv->Tempval =1;
  adv->BSqval =1;
  adv->NBval =1;
  adv->ADCval =1;
  adv->PC1val =1;
  adv->PC2val =1;
  adv->PRFval =1;
  adv->BSlval =1;
  adv->RXSval =1;
  adv->RXDval =1;
  adv->Attval =1;
  adv->RAGCval =1;
  Firsttime=0;
  }
  
  /* Jason Feature: if any auxiliary data changed, the
   * time shall be rewritten*/
  if (adv-> PSval) g_RewriteTime=1;
  else if(adv-> Rval)g_RewriteTime=1;
  else if(adv-> Attenval)g_RewriteTime=1;
  else if(adv-> PWval)g_RewriteTime=1;
  else if(adv-> Tempval)g_RewriteTime=1;
  else if(adv-> BSqval)g_RewriteTime=1;
  else if(adv-> Eval)g_RewriteTime=1;
  else if(adv-> NBval)g_RewriteTime=1;
  else if(adv-> ADCval)g_RewriteTime=1;
  else if(adv-> PC1val)g_RewriteTime=1;
  else if(adv-> PC2val)g_RewriteTime=1;
  else if(adv-> PRFval)g_RewriteTime=1;
  else if(adv-> BSlval)g_RewriteTime=1;
  else if(adv-> RXSval)g_RewriteTime=1;
  else if(adv-> RXDval)g_RewriteTime=1;
  else if(adv-> Attval)g_RewriteTime=1;
  else if(adv-> RAGCval)g_RewriteTime=1;

  /* pad stores the old values of the aux data*/
  memcpy(&pad,&(g_myForm.s),sizeof(struct AUX));
  
}

/**********************************************
*  alIsRepData()
* is this a Pulse, that  contains Replica?
* to detect, the Replica present bit is used, better
* would be to check if the format contains more
* Frames than usually
*/
int alIsRepData()
{   
  
  if (g_myForm.s.ReplicaPresent)
    return 1;
  else return 0;
}

/**********************************************
* alGetSizeOfReplica
* determines, how many Samples of Replica are in
* this Format, dependent on the ADC Sampling Rate
* REV F, p. 36
*/
uint alGetSizeOfReplica()
{
  uint back;
  
  switch (g_myForm.s.ADCSamplingRate)
  {
    case 0:back=1440;break;
    case 1:back=822;break;
    case 2:back=576;break;
    default:back=0; /*in case of an  Error in ADC Rate*/
  }
  return back;
}

  

/*****************************************************
* alGetSizeOfApplDataField
* returns Size of the Application Data Field
* Number of Frames of this format times DATAFIELD_LENGTH
*/
uint alGetSizeOfApplDataField()
{
  return (foaGetNoOfFrames()*DATAFIELD_LENGTH);
}

/*******************************************************
* alDecodeApplicationData(length of the Application Data Field)
* reads the Echo/Replica Data from myForm and converts it into
* the g_ApplicationDataFieldBuffer (extracts the 4 Bits I, 4 Bits
* Q in separated Bytes) REV F, p. 43
* moves the buffer pointer (passed to the DRD WriteScan functions in
* randi.c, Scan() ) to the beginnning of g_ApplicationDataFieldBuffer
*/
char *alDecodeApplicationData(length)
uint length;
{
  register uint count;
  char *BufferBegin;
  
  BufferBegin=(char *) g_ApplicationDataFieldBuffer;
  for (count=0;count<length;count++)
  {
    /*the ucDataField is read out sequentially without  taking care
     * of the division into frames
     * store signed data
     */
    g_ApplicationDataFieldBuffer[count].I=
      g_DACtable[(g_myForm.ucFrameData[0][count+AUX_DATAFIELD_LENGTH]>>4)&0xf] ;
    g_ApplicationDataFieldBuffer[count].Q=
      g_DACtable[g_myForm.ucFrameData[0][count+AUX_DATAFIELD_LENGTH]&0x0f];
  };
return BufferBegin;  
}

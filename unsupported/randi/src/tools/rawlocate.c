#include <stdio.h>
#include <malloc.h>
#include "drdapi.h"

#define FACILITY	"Alaska SAR Facility"
#define PROGRAM 	"RAWLOCATE"
#define VER_MAJOR	0
#define VER_MINOR	1
/* V 0.1 by Christian Fischer */

#define NUMBER_OF_USED_DRDFILE_TYPES 5
/*definitions used for the graphical output */
#define SCALE 1000       /* how many Pixels per kByte */
#define SCANSPERLINE 30  /* how many Scans per Imageline*/
#define SCANRESERVE 20   /* maximum Pixels for one Scan
                          * SCANRESERVE*SCALE is max Size  of
                          * a Scan the program can handle*/


enum drdFileTypes {Echo,Noise,Cal,Rep,Zero};
/*Colors in the image for the filetypes and a fill color*/
char Color[]={5,8,13,14,1,0};
char *ColorName[]={"green","blue","yellow","red","black"};
#define Fill 6

FILE *Image;

/* prototypes */
void Hello();
void Bye();
void Help();
void Location();
void getScanInfo();
int Compare();


struct tagStore {
       ulong ImScanNr;
       ushort BlockLength;
       enum drdFileTypes Type;
       } ;
       
       
struct {
	char *pszInFile;
	char *Outfile;
	ulong luPulseNumber;
	} g_cmdLine;




main(argc,argv)
int argc;
char **argv;
{
DRDFILE *pFile,*pAux;
char *pTmp;

/*drd-debugging*/
bDrdDebug=0;

Hello();

if (!CmdLine(argc,argv))
	Help();
else if (!drdInit(PROGRAM,VER_MAJOR,VER_MINOR,FACILITY) )
	printf("%s cannot initialize drd library\n",PROGRAM);
else {  
        Image=fopen(g_cmdLine.Outfile,"wb");
        if (Image==NULL) 
          {
             printf("Error opening Image File %s\n",g_cmdLine.Outfile);
             exit();
          }
	Location();
	drdClose(pFile);
	}
Bye();
}

/**********************************************************************
* Hello says hello
* Bye says bye
* Help prints help
*/
void Hello()
{
printf("%s  \n  provides analysis of the location of different data \n",PROGRAM);
printf("  types in a raw data file by using the corresponding\n");
printf("  drd files created by ANDI/RANDI\n");
}

void Bye()
{
printf("%s end. You guys have fun.\n",PROGRAM);
}

void Help()
{
printf("usage:\n");
printf("\t rawlocate infile [outfile]\n");
printf("\t       infile   name of the set of DRD input files\n");
printf("\t                without extention!\n");
printf("\t       outfile  name of ouput image file\n");
}

/* CmdLine analyzes the command line */
int CmdLine(argc,argv)
int argc;
char **argv;
{
char *pTmp;

g_cmdLine.Outfile=(char *)malloc(255);
if ((argc >= 2)&&(argc<=3))
  {
  g_cmdLine.pszInFile = argv[1];
  if (argc ==3)
    sprintf(g_cmdLine.Outfile,argv[2]);
  else 
    {
    sprintf(g_cmdLine.Outfile,g_cmdLine.pszInFile);
    strcat(g_cmdLine.Outfile,".rwl.image");
    }
  }

else return 0;  

return 1;
}

/********************************************************
* Location()
* tries to find out, how different data types are located
* in the raw data input file
*/

void Location()
{

DRDFILE *echo,*zero,*cal,*rep,*noise;
char *pTmp;
struct tagStore *Store;
uint count;
ulong Bytes[NUMBER_OF_USED_DRDFILE_TYPES];
ulong TotalBytes;
ulong NrOfBlocks;
ulong Length[NUMBER_OF_USED_DRDFILE_TYPES];
ulong maxLength=0;
uint i,k,Blocklength;
char cTmp;


for (i=0;i<NUMBER_OF_USED_DRDFILE_TYPES;i++)
  Length[i]=0;
pTmp=(char *)malloc (255);

sprintf(pTmp,g_cmdLine.pszInFile);
strcat (pTmp,".echo");
if (! (NULL==(echo=drdOpen(pTmp ) )) )
   {
   printf("%s opened %s\n",PROGRAM,pTmp);
   fseek(echo->file,0,2);
   Length[Echo]=ftell(echo->file);
   rewind(echo->file);
   }
else printf("%s not found\n",pTmp);

sprintf(pTmp,g_cmdLine.pszInFile);
strcat (pTmp,".noise");
if (! (NULL==(noise=drdOpen(pTmp ) )) )
   {
   printf("%s opened %s\n",PROGRAM,pTmp);
   fseek(noise->file,0,2);
   Length[Noise]=ftell(noise->file);
   rewind(noise->file);
   }
else printf("%s not found\n",pTmp);

sprintf(pTmp,g_cmdLine.pszInFile);
strcat (pTmp,".cal");
if (! (NULL==(cal=drdOpen(pTmp ) )) )
   {
   printf("%s opened %s\n",PROGRAM,pTmp);
   fseek(cal->file,0,2);
   Length[Cal]=ftell(cal->file);
   rewind(cal->file);
   }
else printf("%s not found\n",pTmp);

sprintf(pTmp,g_cmdLine.pszInFile);
strcat (pTmp,".rep");
if (! (NULL==(rep=drdOpen(pTmp ) )) )
   {
   printf("%s opened %s\n",PROGRAM,pTmp);
   fseek(rep->file,0,2);
   Length[Rep]=ftell(rep->file);
   rewind(rep->file);
   }
else printf("%s not found\n",pTmp);

sprintf(pTmp,g_cmdLine.pszInFile);
strcat (pTmp,".zero");
if (! (NULL==(zero=drdOpen(pTmp ) )) )
   {
   printf("%s opened %s\n",PROGRAM,pTmp);
   fseek(zero->file,0,2);
   Length[Zero]=ftell(zero->file);
   rewind(zero->file);
   }
else printf("%s not found\n",pTmp);


/*Inits*/
for (i=0;i<NUMBER_OF_USED_DRDFILE_TYPES;i++)
  {
  if (Length[i]>maxLength)
    maxLength=Length[i];
  }  

/*reserve hopefully enough space, otherwise segm fault */
NrOfBlocks=(int) maxLength/5000 +1000;    
Store=(struct tagStore *) calloc(NrOfBlocks,sizeof(struct tagStore));
count=0;
Bytes[Echo]=0;
Bytes[Noise]=0;
Bytes[Cal]=0;
Bytes[Rep]=0;
Bytes[Zero]=0;
TotalBytes=0;


printf("%s extracts the information out of the drd files\n",PROGRAM);

if (!(echo==NULL))
  {
  getScanInfo(echo,Store,&count,Echo);
  echoClose(echo);
  }

if (!(noise==NULL))
  {
  getScanInfo(noise,Store,&count,Noise);
  noiseClose(noise);
  }

if (!(cal==NULL))
  {
  getScanInfo(cal,Store,&count,Cal);
  calClose(cal);
  }

if (!(rep==NULL))
  {
  getScanInfo(rep,Store,&count,Rep);
  repClose(rep);
  }

if (!(zero==NULL))
  {
  getScanInfo(zero,Store,&count,Zero);
  zeroClose(zero);
  }
  
if (count!=0) {  
  /*sort the list: 1st criteria is ImageScanNr, 2nd
   *is the filetype (that means, inside one scan 
   * the location of the data is not restored 
   * correctly, that would need knowledge of the 
   * satellite type and this tool shall be independent)
   * on the type
   */
  printf("%s sorts the data                                        \n",
           PROGRAM); 
  qsort(Store,count,sizeof(struct tagStore),Compare);
  
  /*display the results*/
  printf("%s displays the data                               \n",
            PROGRAM);
  
  for (i=0;i<count;i++)
  {
    /*Debug
    printf("\nmemno: %d Scannr %lu length %d type %d",i,Store[i].ImScanNr,
    Store[i].BlockLength,Store[i].Type);  */
    
    /*graphical output? Only if enough Bytes*/
      cTmp=Color[Store[i].Type];
      for (k=0;k<=( (int) Store[i].BlockLength/SCALE);k++)
        {
          if (!fwrite( &cTmp,sizeof(uchar),1,Image) )
          printf("writing error\n");
        }
      if (! ((i+1)%SCANSPERLINE) )                                             
        {
        for (k=0;k<=ftell(Image)-(i+1)*SCANRESERVE;k++)
          {
            if (!fwrite( &(Color[Fill]),sizeof(uchar),1,Image) )
            printf("writing error\n");
          }
                                
        }
          
    /*calculate Histogram*/
    Bytes[Store[i].Type]+=Store[i].BlockLength;
    TotalBytes+=Store[i].BlockLength;
  }
  close(Image);
  if (count>2*SCANSPERLINE)
    {
    sprintf(pTmp,"dx %s -d %u %lu -l %s &",g_cmdLine.Outfile,
              (uint)SCANSPERLINE*SCANRESERVE,(ulong)count/SCANSPERLINE,
              g_cmdLine.Outfile);
    printf("%s calls DX\n",PROGRAM);
    printf("%s\n",pTmp);
    printf("I use the following colors in the image:\n");
    printf("%s for the %s data\n",ColorName[Echo],"echo");
    printf("%s for the %s data\n",ColorName[Noise],"noise");
    printf("%s for the %s data\n",ColorName[Cal],"Calpulse");
    printf("%s for the %s data\n",ColorName[Rep],"replica");
    printf("%s for the %s data\n",ColorName[Zero],"zero");
    system(pTmp);
    }
  else 
    printf("%s has not enough data for graphical output\n",PROGRAM);
    
    
  /*output Histogram*/
  printf("\nEcho Bytes:\t%.3f%%\t%lu \n",(float) 100.0* Bytes[Echo]/TotalBytes,Bytes[Echo]);
  printf("Noise Bytes:\t%.3f%%\t%lu \n",(float) 100.0* Bytes[Noise]/TotalBytes,Bytes[Noise]);
  printf("Calpulse Bytes:\t%.3f%%\t%lu \n",(float) 100.0* Bytes[Cal]/TotalBytes,Bytes[Cal]);
  printf("Rep Bytes:\t%.3f%%\t%lu \n",(float) 100.0* Bytes[Rep]/TotalBytes,Bytes[Rep]);
  printf("Zero Bytes:\t%.3f%%\t%lu \n",(float) 100.0* Bytes[Zero]/TotalBytes,Bytes[Zero]);
  printf("\nTotal Bytes:\t\b100.000%%\t%lu\n",TotalBytes);
} /*end if*/
close(Image);
free (pTmp);
free(g_cmdLine.Outfile);
}
/*********************************************
* GetScanInfo()
* reads the imagescan numbers and the according
* block lengths 
* out of a drd file
*/
void getScanInfo(drdFile,lStore,count,fileType)
DRDFILE *drdFile;
struct tagStore *lStore;
uint *count;
enum drdFileTypes fileType;
{
long int lCurPos;
ulong fileLength;

fseek(drdFile->file,0L,2);
  fileLength=ftell(drdFile->file);
rewind(drdFile->file);

if (!drdGotoFirstBlock(drdFile))
  return;
else 
  {
  do
    {
    lStore[*count].BlockLength=(ulong)drdGetCurrentBlockLength(drdFile);   
    lStore[*count].Type=fileType;
    /* save current possition */
    lCurPos = ftell(drdFile->file);
    /* read ImageScanNumber */
    fread(&(lStore[*count].ImScanNr),sizeof(ulong),1,drdFile->file);
    /* restore file position*/
    fseek(drdFile->file,lCurPos,0);
      
    /*percentage counter to show the user  where I am*/
    printf("Processing %s -->> %d%%   \r",drdFile->pszFileName,
            (int) (100.0*lCurPos/fileLength));
    
    (*count)++;
    } while (drdGotoNextBlock(drdFile));
  }  
}

/*****************
* int Compare()
* makes the comparison for qsort
*/
int Compare(First,Second)
struct tagStore First,Second;
{
struct tagStore Swap;
if (First.ImScanNr<Second.ImScanNr)
  return -1;
else if (First.ImScanNr>Second.ImScanNr)
  return 1;
else if (First.Type<Second.Type)
  return -1;
else if (First.Type>Second.Type)
  return 1;
else return 0;
}
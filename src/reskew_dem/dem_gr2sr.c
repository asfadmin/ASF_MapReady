#include "deskew.h"

float badDEMht=0.0;
float unInitDEM=-1.0;
int maxBreakLen=20;
float unitRand(void)
{
	return (float)(0x0ffff&rand())/(0x0ffff);
}

float gaussRand(int num)
{
	int i;
	float ret=0;
	for (i=0;i<num;i++)
		ret+=unitRand();
	return 2*ret/num;
}

/*Note: it may seem stupid to add speckle to our simulated
SAR images, but the speckle has an important statistical 
contribution to the image.*/
float *createSpeckle(void)
{
	int i;
#define speckleLen 0x0fff
#define getSpeckle speckle[specklePtr=((specklePtr+7)&speckleLen)]
	float *speckle=(float *)MALLOC(sizeof(float)*(speckleLen+1));
	for (i=0;i<=speckleLen;i++)
		speckle[i]=gaussRand(4);
	return speckle;
}

/*dem_gr2sr: map one line of a ground range DEM into
one line of a slant range DEM && one line of simulated amplitude image.*/
void dem_gr2sr(float *grDEM, float *srDEM,float *amp)
{
	int x,grX;
	double lastSrX=-1;/*Slant range pixels up to (and including) here have been filled.*/
	float lastOutValue=badDEMht;
	static float *speckle=NULL;
	int specklePtr=rand();
	if (speckle==NULL)
		speckle=createSpeckle();
	
/*Initialize amplitude to zero, and DEM to -1.*/
	for (x=0;x<sr_ns;x++)
	{
		amp[x]=0;
		srDEM[x]=unInitDEM;
	}

/*Step through the ground range line using grX.
Convert each grX to an srX.  Update amplitude and height images.*/
	for (grX=1;grX<gr_ns;grX++)
	{
		double height=grDEM[grX];
	/*srX: float slant range pixel position.*/
		double srX=srE2srH(grX,height);
		int sriX=(int)srX;
		if ((height!=badDEMht)&&(srX>=0)&&(srX<sr_ns))
		{
			double runLen=srX-lastSrX;
			int intRun=(int)runLen;
			if ((runLen<maxBreakLen)&&(lastOutValue!=badDEMht))
			{
				double currAmp;
			/*Update the amplitude image.*/
			    	if (runLen<0) 
			    		runLen=-runLen;
			    	currAmp=50.0/(runLen*runLen*5+0.1);
				for (x=lastSrX+1;x<=sriX;x++)
					amp[x]+=currAmp*getSpeckle;
			/*Then, update the height image.*/
				if (intRun!=0)
				{
					float curr=lastOutValue;
					float delt=(height-lastOutValue)/intRun;
					curr+=delt;
					for (x=lastSrX+1;x<=sriX;x++)
					{
						if (srDEM[x]==unInitDEM)
							srDEM[x]=curr;/*Only write on fresh pixels.*/
					/*	else
							srDEM[x]=badDEMht; Black out layover regions.*/
						curr+=delt;
					}
				} else 
					if (srDEM[(int)lastSrX+1]==unInitDEM)
						srDEM[(int)lastSrX+1]=height;
			} else {
				for (x=lastSrX+1;x<=sriX;x++)
					srDEM[x]=badDEMht;
			}
			lastOutValue=height;
			lastSrX=srX;
		}
	}
/*Fill to end of line with zeros.*/
	for (x=lastSrX+1;x<sr_ns;x++)
		srDEM[x]=badDEMht;
/* Just plug all the holes and see what happens */
/*Attempt to plug one-pixel holes, by interpolating over them.*/
	for (x=1;x<(sr_ns-1);x++)
	{
		if (srDEM[x]==badDEMht)
		   /* &&
		    srDEM[x-1]!=badDEMht &&
		    srDEM[x+1]!=badDEMht)*/
			srDEM[x]=(srDEM[x-1]+srDEM[x+1])/2;
	}
}

/*
Diffuse (lambertian) reflection:
	reflPower=cosIncidAng[sriX]+
		sinIncidAng[sriX]*(grDEM[grX]-grDEM[grX-1])/grPixelSize;
   	if (reflPower<0) reflPower=0;Radar Shadow.
   	currAmp=reflPower/runLen;
*/

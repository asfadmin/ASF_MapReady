#include "main.h"

float getRand(float minV,float maxV)
{
	unsigned int value=0x7Fff&rand();
	return minV+((float)value)/((float)0x7Fff)*(maxV-minV);
}

#define makeGrey16(value) ((value)|((value)<<5)|((value)<<10))
#define makeGrey32(value) ((value)|((value)<<8)|((value)<<8))
long makeGrey(float value)
{
	int intVal;
	switch(screenBits)
	{
		case 8:
			return startColor+((int)((100.0+value)*lenColor))%lenColor;
		case 15:
		case 16:
			intVal=value*31.0;
			intVal&=0x1F;
			return makeGrey16(intVal);
		case 24:
		case 32:
			intVal=value*255.0;
			intVal&=0xFF;
			return makeGrey32(intVal);
	}
}

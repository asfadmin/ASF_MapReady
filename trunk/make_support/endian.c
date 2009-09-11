/*
	Endian.c:
A tiny program used to determine if a machine is big-
or little- endian, and other sundry machine-related details.

The program is called by the "config" script to make the
"system_rules" file.

*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

int main(argc,argv)
	int argc;
	char *argv[];
{
	static char test[64]={0};
	int i;
	int writeRules=0;/*Write "system_rules" file?*/
	int littleEndian=0;
	int ieee=1,is_cray=0;
	if (argc==2&& 0==strcmp(argv[1],"-r"))
		writeRules=1;
	for (i=0;i<32;i++)
		test[i]=0;
	
	test[0]=0x01;
	if (0x0001==*(int *)(&test[0]))
		littleEndian=1;
	
	if (writeRules)
	{
		printf("# ENDIAN_FLAGS variable determined by endian.c\n");
		printf("ENDIAN_FLAGS = -D%s_endian ",littleEndian?"lil":"big");
	}
	else
	{
		printf("Computer Information:\n");
		printf("\tshort = %d bytes.\n",sizeof(short));
		printf("\tint = %d bytes.\n",sizeof(int));
		printf("\tlong = %d bytes.\n",sizeof(long));
		printf("\t-> Integers are %s-endian.\n\n",
			littleEndian?"little":"big");
		printf("\tfloat = %d bytes.\n",sizeof(float));
		printf("\tdouble = %d bytes.\n",sizeof(double));
	}
	
	if (sizeof(float)!=4)
	{
		ieee=0;
#ifdef cray
		if (sizeof(float)==8)
			is_cray=1;
#endif
	}
	
	*(float *)(&test[0])=(float)(-9.5);
	if (!writeRules)
	{
		printf("\t(float)(-9.5)= 0x%x %x %x %x\n",
			0x00ff&(int)test[0],0x00ff&(int)test[1],
			0x00ff&(int)test[2],0x00ff&(int)test[3]);
		printf("\t->Floats are ");
	}
#define isTest(a,b,c,d) \
	(test[0]==(char)a) && \
	(test[1]==(char)b) && \
	(test[2]==(char)c) && \
	(test[3]==(char)d)

	if      (isTest(0xC1,0x18,0x00,0x00))
		ieee=1,littleEndian=0;
	else if (isTest(0x00,0x00,0x18,0xC1))
		ieee=1,littleEndian=1;
	else if (isTest(0xC0,0x04,0x98,0x00))
		ieee=0,is_cray=1;
	else
		ieee=0;
	if (writeRules)
	{/*This is a continuation of the "ENDIAN_FLAGS" line from the previous write*/
		printf("%s\n",
			ieee?(littleEndian?"-Dlil_ieee":  "-Dbig_ieee"):
			          (is_cray?"-Dcray_float":"-Dnon_ieee "));
	}
	else
		printf("%s %s-endian.\n\n",
			ieee?"IEEE standard ":
			(is_cray?"cray proprietary":"non-IEEE proprietary "),
			littleEndian?"little":"big");
	return(0);
}

#include <stdio.h>
main(int argc,char *argv[])
{
	char line[1024];
	int xCol,yCol;
	if (argc!=3)
	{
		printf("Usage: plotcol <x column #> <y column #>\n"
			" plotcol will read points from stdin and plot them\n"
			" pseudographically to the screen.\n"
			" Example:\n"
			"\tplot 1 3\n"
			"1 0 10\n"
			"2 0 13\n"
			"3 0 17\n"
			"^D\n"
			" Plots an increasing sequence y=(10,13,17) on x=(1,2,3).\n");
		exit(1);
	}
	xCol=atoi(argv[1]);
	yCol=atoi(argv[2]);
	while (NULL!=fgets(line,1024,stdin))
	{
		int itemsRead;
		double vals[30];
		itemsRead=sscanf(line,"%lf %lf %lf %lf %lf %lf %lf %lf %lf %lf"
		"%lf %lf %lf %lf %lf %lf %lf %lf %lf %lf"
		"%lf %lf %lf %lf %lf %lf %lf %lf %lf %lf",
		&vals[ 0],&vals[ 1],&vals[ 2],&vals[ 3],&vals[ 4],&vals[ 5],&vals[ 6],&vals[ 7],&vals[ 8],&vals[ 9],
		&vals[10],&vals[11],&vals[12],&vals[13],&vals[14],&vals[15],&vals[16],&vals[17],&vals[18],&vals[19],
		&vals[20],&vals[21],&vals[22],&vals[23],&vals[24],&vals[25],&vals[26],&vals[27],&vals[28],&vals[29]);
		if (xCol<=itemsRead && yCol<=itemsRead)
			AscGraf_AddPlotPoint(vals[xCol-1],vals[yCol-1]);
	}
	AscGraf_PlotPoints();
	return 0;
}

/* Alaska SAR Processor (ASP) %W% %E% %U% */
/*  j1_write_pcm( pcm_buf, start_time, pcm_start, file ) -----------------
	Write contents of buffer containing sequence of J-ERS-1 PCM data sets

    Inputs:

	PCM_BUF:  Buffer containing PCM data
	START_TIME:  String containing GMT of starting bit of PCM data
	PCM_START:  Bit offset to first bit of PCM data
	FILE:  Name of file into which interpreted PCM data will be written

Each PCM minor frame consists of 128 bytes, each major frame consists of 64
minor frames.  This routine will write out the spacecraft attitude and state
vector information extracted from the PCM frames.
*/
#include <stdio.h>

#define HPA_A_T	52*8+6*1024	/*  High Power Amp A temp 6th minor frame  */
#define HPA_B_T	53*8+6*1024	/*  High Power Amp B temp 6th minor frame  */
#define HPA_R_T	54*8+6*1024	/*  High Power Amp R temp 6th minor frame  */
#define PLSG_A_T 58*8+6*1024	/*  Pulse Generator A temp 6th minor frame  */
#define PLSG_B_T 59*8+6*1024	/*  Pulse Generator B temp 6th minor frame  */

#define ROLL	68*8	/*  bit offset to roll parameters  */
#define PITCH	69*8	/*  bit offset to pitch parameters  */
#define YAW	70*8	/*  bit offset to yaw parameters  */
#define RFOL_A 92*8	/*  RF Output Level A  */
#define RFOL_B 96*8	/*  RF Output Level B  */

/*  Table B3  SAR-HPA-A Temp  */
float b3x[10] = {1.524,1.671,1.825,1.989,2.161,2.329,2.530,2.727,2.935,3.150};
float b3y[10] = {-30.0,-20.0,-10.0,  0.0, 10.0, 20.0, 30.0, 40.0, 50.0, 60.0};

/*  Table B4  SAR-HPA-B Temp  */
float b4x[10] = {1.523,1.671,1.825,1.986,2.157,2.338,2.525,2.720,2.925,3.143};
float b4y[10] = {-30.0,-20.0,-10.0,  0.0, 10.0, 20.0, 30.0, 40.0, 50.0, 60.0};

/*  Table B5  SAR-HPA-R Temp  */
float b5x[10] = {1.527,1.669,1.823,1.985,2.156,2.345,2.538,2.741,2.954,3.173};
float b5y[10] = {-30.0,-20.0,-10.0,  0.0, 10.0, 20.0, 30.0, 40.0, 50.0, 60.0};

/*  Table B9  SAR-PLSG-A Temp  */
float b9x[10] = {1.526,1.665,1.827,1.993,2.167,2.354,2.550,2.753,2.969,3.189};
float b9y[10] = {-30.0,-20.0,-10.0,  0.0, 10.0, 20.0, 30.0, 40.0, 50.0, 60.0};

/*  Table B10  SAR-PLSG-B Temp  */
float b10x[10] = {1.504,1.650,1.801,1.962,2.137,2.315,2.503,2.700,2.904,3.120};
float b10y[10] = {-30.0,-20.0,-10.0,  0.0, 10.0, 20.0, 30.0, 40.0, 50.0, 60.0};

/*  Table B13  Transmitter Power Level Telemetry (A) Polynomials  */
float b13a[14] = { 0.000016, 0.000009, 0.000001, -0.000004, -0.000009, 
		-0.000118, 0.000017, 0.000005, -0.000003, -0.000017,
		-0.000037, -0.000065, -0.000099, -0.000138 };
float b13b[14] = { 0.000742, 0.001438, 0.002089, 0.002617, 0.003072,
		0.010053, 0.002137, 0.002869, 0.003125, 0.003631,
		0.004390, 0.005405, 0.006670, 0.008136 };
float b13c[14] = { 0.099347, 0.172202, 0.270221, 0.393023, 0.539895,
		0.616459, 0.921128, 1.105430, 1.349668, 1.641242,
		1.981603, 2.371202, 2.808138, 3.293122 };

/*  Table B14 Polynomials  */
float b14a[10] = { 0.000067, 0.000072, 0.000077, 0.000077, 0.000074,
		0.000068, 0.000059, 0.000048, 0.000035, 0.000018 };
float b14b[10] = { 0.001427, 0.001201, 0.000954, 0.000816, 0.000709,
		0.000632, 0.000613, 0.000623, 0.000662, 0.000761 };
float b14c[10] = { 1.063745, 1.274052, 1.528167, 1.828659, 2.173697,
		2.563281, 2.998503, 3.478272, 4.002134, 4.572088 };

/*  Table B16 Polynomials  */
float b15a[19] = { 0.000064, 0.000042, 0.000022, 0.000003, -0.000004,
		-0.000013, -0.000017, -0.000015, 0.000007, 0.000024, 
		0.000030, 0.000030, -0.000049, -0.000018, 0.000003, 
		0.000013, 0.000013, 0.000003, -0.000017 };
float b15b[19] = { 0.000041, 0.000595, 0.001003, 0.001269, 0.001360, 
		0.001341, 0.001145, 0.001139, 0.000631, 0.000278, 
		0.000166, 0.000190, 0.002379, 0.002077, 0.001875, 
		0.001729, 0.001659, 0.001643, 0.001701 };
float b15c[19] = { 1.202316, 1.315142, 1.454107, 1.619113, 1.804749, 
		2.018972, 2.257789, 2.493887, 2.751898, 3.013122, 
		3.280293, 3.551937, 3.766179, 3.896761, 4.012332, 
		4.114507, 4.201027, 4.273702, 4.331721 };

/*  Table B16 Polynomials  */
float b16a[19] = { -0.000073, -0.000059, -0.000047, -0.000038, -0.000032,
		-0.000028, -0.000026, -0.000028, -0.000034, -0.000087,
		-0.000066, -0.000009, 0.000011, 0.000017, 0.000022,
		0.000025, 0.000027, 0.000027, 0.000028 };
float b16b[19] = { 0.002520, 0.002106, 0.001768, 0.001485, 0.001281,
		0.001107, 0.001009, 0.000969, 0.000987, 0.001216,
		0.000662, 0.000026, 0.000338, 0.000553, 0.000654,
		0.000670, 0.000548, 0.000315, -0.000061 };
float b16c[19] = { 1.286482, 1.394580, 1.526058, 1.681726, 1.860226,
		2.063271, 2.288697, 2.538763, 2.813469, 3.131301,
		3.395476, 3.643059, 3.800177, 3.943101, 4.066161,
		4.169452, 4.252234, 4.315605, 4.357565 };

/*  Table B17 Polynomials  */
float b17a[14] = { 0.000032, 0.000022, 0.000017, 0.000011, 0.000008,
		0.000005, 0.000015, 0.000010, -0.000001, -0.000016,
		-0.000037, -0.000063, -0.000095, -0.000131 };
float b17b[14] = { 0.000392, 0.001073, 0.001601, 0.002038, 0.002350,
		0.002591, 0.002467, 0.002622, 0.003003, 0.003582,
		0.004390, 0.005399, 0.006610, 0.008021 };
float b17c[14] = { 0.083021, 0.158586, 0.257388, 0.382518, 0.530981,
		0.704964, 0.899332, 1.095545, 1.339451, 1.630955,
		1.971603, 2.360301, 2.798047, 3.282843 };

/*  Table B18 Polynomials  */
float b18a[10] = {	0.000023, 0.000023, 0.000022, 0.000022, 0.000020,
		0.000018, 0.000016, 0.000012, 0.000007, 0.000003 };
float b18b[10] = {	0.001767, 0.001535, 0.001351, 0.001168, 0.001033,
		0.000923, 0.000837, 0.000803, 0.000793, 0.000805 };
float b18c[10] = {	1.091026, 1.304395, 1.562052, 1.863708, 2.208652,
		2.598239, 3.032469, 3.510437, 4.033049, 4.598853 };

/*  Table B19 Polynomials  */
float b19a[19] = {	-0.000002, 0.000006, 0.000011, 0.000008, 0.000004,
		-0.000007, -0.000020, 0.000022, 0.000000, -0.000007,
		0.000001, 0.000025, 0.000000, -0.000009, -0.000012,
		-0.000010, -0.000001, 0.000012, 0.000030 };
float b19b[19] = {	0.001703, 0.001563, 0.001387, 0.001230, 0.001059,
		0.000858, 0.000643, -0.000205, 0.000535, 0.000858,
		0.000811, 0.000367, 0.000560, 0.001072, 0.001408,
		0.001565, 0.001563, 0.001384, 0.001049 };
float b19c[19] = {	1.222793, 1.333870, 1.471464, 1.637215, 1.828675,
		2.049006, 2.294046, 2.501120, 2.787740, 3.065006,
		3.332755, 3.590343, 3.802383, 3.953895, 4.081000,
		4.182249, 4.256834, 4.307013, 4.330978 };

/*  Table B20 Polynomials  */
float b20a[19] = {	0.000006, 0.000009, 0.000007, 0.000004, -0.000004,
		-0.000013, -0.000026, -0.000050, 0.000008, 0.000032,
		0.000021, -0.000023, 0.000033, 0.000005, -0.000012,
		-0.000022, -0.000024, -0.000018, -0.000003 };
float b20b[19] = {	0.002472, 0.001928, 0.001560, 0.001291, 0.001223,
		0.001249, 0.001428, 0.001545, 0.000649, 0.000368,
		0.000798, 0.001814, 0.000738, 0.001496, 0.001943,
		0.002159, 0.002119, 0.001821, 0.001266 };
float b20c[19] = {	1.190198, 1.322105, 1.473419, 1.642305, 1.830242,
		2.035300, 2.259122, 2.525781, 2.775639, 3.039378,
		3.316571, 3.606550, 3.748516, 3.911841, 4.048740,
		4.159044, 4.244111, 4.302487, 4.335627 };

float pl[14] = { 50., 51., 52., 53., 54., 55., 56.,
		57., 58., 59., 60., 61., 62., 63. };
float col[10] = { 10., 11., 12., 13., 14., 15., 16., 17., 18., 19. };
float vso[19] = { -10., -9., -8., -7., -6., -5., -4., -3., -2., -1.,
		0., 1., 2., 3., 4., 5., 6., 7., 8. };

int j1_write_pcm( pcm_buf, start_time, pcm_start, file )
unsigned char *pcm_buf;
char *file, *start_time;
int pcm_start;

{
int i;
unsigned char buf[4];
float roll, pitch, yaw, v1, v2, v3, v4, t1, t2, t3, t4;
float hpa_at, hpa_av, hpa_bt, hpa_bv, hpa_rt, hpa_rv;
float plsg_at, plsg_av, plsg_bt, plsg_bv;
float pla[14], plb[14], cola[10], colb[10];
float ivsoa[19], qvsoa[19], ivsob[19], qvsob[19];
FILE *out, *fopen();
float flut();

out = fopen( file, "w" );	/*  open PCM preamble file  */
if( out == NULL ){		/*  check file open status  */
    printf( "J1_WRITE_PCM error opening PCM file" );
    return( 0 );
}
fprintf( out, "PCM data start time:  %s\n\n", start_time );

/*  HPA A, B, R temperatures  */

get_bits( pcm_buf, pcm_start+HPA_A_T, buf, 8 );
hpa_av = (float) *buf * (5.12/255.);
hpa_at = flut( b3x, b3y, hpa_av, 10 );
get_bits( pcm_buf, pcm_start+HPA_B_T, buf, 8 );
hpa_bv = (float) *buf * (5.12/255.);
hpa_bt = flut( b4x, b4y, hpa_av, 10 );
get_bits( pcm_buf, pcm_start+HPA_R_T, buf, 8 );
hpa_rv = (float) *buf * (5.12/255.);
hpa_rt = flut( b5x, b5y, hpa_av, 10 );
fprintf( out, "High Power Amp-A Temp=%12.7f degC (%12.7f volts)\n",
		hpa_at, hpa_av );
fprintf( out, "High Power Amp-B Temp=%12.7f degC (%12.7f volts)\n",
		hpa_bt, hpa_bv );
fprintf( out, "High Power Amp-R Temp=%12.7f degC (%12.7f volts)\n",
		hpa_rt, hpa_rv );
printf("pass 7\n");

/*  Calculate look-up tables for transmitter power output levels  */

for( i=0; i<14; i++ ){
    pla[i] = ( b13a[i]*hpa_at + b13b[i] ) * hpa_at + b13c[i];
    plb[i] = ( b17a[i]*hpa_bt + b17b[i] ) * hpa_bt + b17c[i];
}

/*  Calculate look-up tables for Calibration unit output levels  */

for( i=0; i<10; i++ ){
    cola[i] = ( b14a[i]*hpa_at + b14b[i] ) * hpa_at + b14c[i];
    colb[i] = ( b18a[i]*hpa_bt + b18b[i] ) * hpa_bt + b18c[i];
}

/*  Calculate look-up tables for Video Signal output levels  */

for( i=0; i<19; i++ ){
    ivsoa[i] = ( b15a[i]*hpa_at + b15b[i] ) * hpa_at + b15c[i];
    ivsob[i] = ( b19a[i]*hpa_bt + b19b[i] ) * hpa_bt + b19c[i];
    qvsoa[i] = ( b16a[i]*hpa_at + b16b[i] ) * hpa_at + b16c[i];
    qvsob[i] = ( b20a[i]*hpa_bt + b20b[i] ) * hpa_bt + b20c[i];
}

/*  Pulse Generator status  */

get_bits( pcm_buf, pcm_start+PLSG_A_T, buf, 8 );
plsg_av = (float) *buf * (5.12/255.);
plsg_at = flut( b9x, b9y, plsg_av, 10 );
get_bits( pcm_buf, pcm_start+PLSG_B_T, buf, 8 );
plsg_bv = (float) *buf * (5.12/255.);
plsg_bt = flut( b10x, b10y, plsg_bv, 10 );
get_bits( pcm_buf, pcm_start+54*8+7*1024, buf, 8 );

fprintf( out, "Pulse Generator-A:" );
if( (*buf&0x4)==0 ) fprintf( out, "OFF");
else fprintf( out, "ON ");
fprintf( out, "  Temp=%12.7f degC (%12.7f volts)\n", plsg_at, plsg_av );

fprintf( out, "Pulse Generator-B:" );
if( (*buf&0x8)==0 ) fprintf( out, "OFF");
else fprintf( out, "ON ");
fprintf( out, "  Temp=%12.7f degC (%12.7f volts)\n", plsg_bt, plsg_bv );

fprintf( out, "\nX'tal Oscillator A:" );
if( (*buf&0x10)==0 ) fprintf( out, "OFF");
else fprintf( out, "ON ");
fprintf( out, "    X'tal Oscillator B:" );
if( (*buf&0x20)==0 ) fprintf( out, "OFF");
else fprintf( out, "ON ");
fprintf( out, "\n\n" );

for( i=0; i<64; i++ ){

    fprintf( out, "\nMinor frame=%d\n", i );

    get_bits( pcm_buf, pcm_start+105*8, buf, 16 );
    fprintf( out, "HPA-A Power Source:" );
    if( (*buf&0x20)==0 ) fprintf( out, "OFF");
    else fprintf( out, "ON ");
    fprintf( out, "    HPA-B Power Source:" );
    if( (*buf&0x40)==0 ) fprintf( out, "OFF");
    else fprintf( out, "ON ");
    fprintf( out, "    HPA-R Power Source:" );
    if( (*buf&0x80)==0 ) fprintf( out, "OFF");
    else fprintf( out, "ON ");
    fprintf( out, "\nCombiner SW(1)/(2)/(3): %d\n", buf[1]&7 );

    get_bits( pcm_buf, pcm_start+RFOL_A, buf, 32 );
    v1 = (float) buf[0] * (5.12/255.);
    t1 = flut( pl, pla, v1, 14 );
    v2 = (float) buf[1] * (5.12/255.);
    t2 = flut( col, cola, v2, 10 );
    v3 = (float) buf[2] * (5.12/255.);
    t3 = flut( vso, ivsoa, v3, 19 );
    v4 = (float) buf[3] * (5.12/255.);
    t4 = flut( vso, qvsoa, v4, 19 );
fprintf(out,"PLA:%12.7f, CAL:%12.7f, IV:%12.7f, QV:%12.7f\n", v1, v2, v3, v4 );
fprintf(out, "Transmitter A RF Output=%12.7fdB, Calibrator Output=%12.7fdB\n",
	t1, t2 );
    fprintf( out, "Video Output level: I=%12.7fdB, Q=%12.7fdB\n", t3, t4 );

    get_bits( pcm_buf, pcm_start+RFOL_B, buf, 32 );
    v1 = (float) buf[0] * (5.12/255.);
    t1 = flut( pl, plb, v1, 14 );
    v2 = (float) buf[1] * (5.12/255.);
    t2 = flut( col, colb, v2, 10 );
    v3 = (float) buf[2] * (5.12/255.);
    t3 = flut( vso, ivsob, v3, 19 );
    v4 = (float) buf[3] * (5.12/255.);
    t4 = flut( vso, qvsob, v4, 19 );
fprintf(out,"PLB:%12.7f, CAL:%12.7f, IV:%12.7f, QV:%12.7f\n", v1, v2, v3, v4 );
fprintf(out, "Transmitter B RF Output=%12.7fdB, Calibrator Output=%12.7fdB\n",
	t1, t2 );
    fprintf( out, "Video Output level: I=%12.7fdB, Q=%12.7fdB\n", t3, t4 );

    if( (i&3)==0 ){	/*  check interleave factor  */
	get_bits( pcm_buf, pcm_start+ROLL, buf, 8 );
	roll = (float) *buf / (float) ( 1<<14 );
	get_bits( pcm_buf, pcm_start+PITCH, buf, 8 );
	pitch = (float) *buf / (float) (1<<14);
	get_bits( pcm_buf, pcm_start+YAW, buf, 8 );
	yaw = (float) *buf / (float) (1<<14);
	fprintf( out, "    Measured Rate:   R=%12.7f Y=%12.7f P=%12.7f (d/s)\n",
		roll, pitch, yaw );
    } else if( (i&3)==1 ){
	get_bits( pcm_buf, pcm_start+ROLL, buf, 8 );
	roll = (float) *buf / (float) (1<<7);
	get_bits( pcm_buf, pcm_start+PITCH, buf, 8 );
	pitch = (float) *buf / (float) (1<<7);
	get_bits( pcm_buf, pcm_start+YAW, buf, 8 );
	yaw = (float) *buf / (float) (1<<7);
	fprintf( out, "    Estimated Angle: R=%12.7f Y=%12.7f P=%12.7f (d)\n",
		roll, pitch, yaw );
    } else if( (i&3)==2 ){
	get_bits( pcm_buf, pcm_start+ROLL, buf, 8 );
	roll = (float) *buf / (float) (1<<5);
	get_bits( pcm_buf, pcm_start+PITCH, buf, 8 );
	pitch = (float) *buf / (float) (1<<5);
	get_bits( pcm_buf, pcm_start+YAW, buf, 8 );
	yaw = (float) *buf / (float) (1<<5);
	fprintf( out, "    Measured Angle:  R=%12.7f Y=%12.7f P=%12.7f (d)\n",
		roll, pitch, yaw );
    }
    pcm_start += 1024;		/*  next minor frame  */
}
fclose( out );
return( 1 );
}

/*  flut( x, y, val, n ) ----------------------------------------------
	Returns a floating point value from a look-up table given a
	floating point input.  Interpolates if necessary.

    Inputs:

	X:  A table of ordinates in which VAL is to be looked up
	    The values must be monotonically increasing
	Y:  A table of values being looked up
	VAL:  The value used to access the tables
	N:  The number of elements in the table

    Outputs:

	Routine returns an interpolated (or exact) value from Y if VAL
	is within the values of corresponding elements in X.  If VAL is
	out of range of X, the limits of Y value is returned.
*/
float flut( x, y, val, n )
float *x, *y, val;
int n;
{
while( n>0 ){
    n--;
    if( val==x[n] ) return( y[n] ); 
    else if (n != 0) { 		/* add CV 2.28/96 */
	if( val<x[n] && val>x[n-1] )
	    return( (val-x[n-1]) * (y[n]-y[n-1]) / (x[n]-x[n-1]) );
    }
}
return( y[0] );
}

#ifndef _T_FDR_H
#define _T_FDR_H

static char sccsid_t_fdr_h[] = 
    "@(#)t_fdr.h	1.2 96/04/09 20:29:12";

/**********************************************************************/
/*trailer file facility data record structure 't_fdr.h'               */
/**********************************************************************/


typedef struct {

unsigned long int    seq_number; 	/*Sequence Number	      */
unsigned      char   rec1_code; 	/*1-st Record sub-type code   */
unsigned      char   rec_type;	 	/*Record Type code	      */
unsigned      char   rec2_code; 	/*2-nd Record sub-type code   */
unsigned      char   rec3_code; 	/*3-rd Record sub-type code   */
unsigned long int    rec_len;	 	/*Record length		      */
char	fdr006a[4];	/*facility sequence number	              */
char	fdr006b[4];	/*filler blanks				      */

char	fdr007[14];	/*DataTakeID SS/X/rrrrr.nn format	      */
                   	/*SS = Satellite, X = Sensor, rrrrr = Rev No. */
                        /*nn = Datatakes within a Rev.		      */
char	fdr008[11];	/*ImageID NNNNNNNTss format where	      */
                   	/*T	= 1.Full-res image          	      */
			/*	= 2, Lo-res image 		      */
                	/*	= 3, Geocoded full-res 		      */
                	/*    	= 4, Geocoded lo-res 		      */
                	/*   	= 5, Complex   			      */
                	/*    	= 6, CCSD 			      */
char	fdr009[5];	/*Year of Correlation			      */
char  	fdr010[17];	/*Greenwich Mean Time of correlation	      */
           		/*date in ddd:hh:mm:ss.ccc format where:      */
			/*ddd = Julian days			      */
                	/*hh = Hours, mm = Minutes		      */
			/*ss = Seconds, ccc = Milliseconds	      */
char	fdr011[33];	/*The name of the site covered by the data.   */
char	fdr012[5];	/*The year the data was recorded.	      */
char	fdr013[17];	/*The Greenwich Mean Time at the image center */
			/*Format: ddd:hh:mm:ss.ccc, where: 	      */
                	/*ddd = Julian days, hh = Hours		      */
			/*mm = Minutes, ss = Seconds  		      */
                	/*ccc = Milliseconds			      */
char  	fdr014[17];    	/*Latitude of image center(deg)	Format:F16.7  */
char  	fdr015[17];	/*Longitude of image center.	Format:F16.7  */
char  	fdr016[17];	/*Latitude at the start  of the image frame   */
			/*in near swath.		Format:F16.7  */
char  	fdr017[17];	/*Longitude at the start of the image frame   */
			/*in the near swath.		Format:F16.7  */
char  	fdr018[17];	/*Latitude at the end of the image frame in   */
			/*the near swath.		Format:F16.7  */
char  	fdr019[17];	/*Longitude at the end of the image frame     */
			/*in the near swath.		Format:F16.7  */
char  	fdr020[17];	/*Latitude at the start of the image frame in */
			/*the far swath.		Format:F16.7  */
char  	fdr021[17];	/*Longitude at the start of the image frame in*/
			/*the far swath.		Format.F16.7  */
char  	fdr022[17];	/*Latitude at the end of the image frame in   */
			/*the far swath.		Format:F16.7  */
char  	fdr023[17];	/*Longitude at the end of the image frame in  */
			/*the far swath.		Format:F16.7  */
char  	fdr024[17]; 	/*The actual swath width (km) coverage in the */
			/*azimuth direction.		Format:F16.7  */
char  	fdr025[17];	/*The actual swath width (km) coverage in the */
			/*range direction.		Format:F16.7  */
char  	fdr026[9];	/*The actual (without filler data) number of  */
			/*pixels per line.		Format:I8     */
char  	fdr027[9];	/*The actual (without filler data) number of  */
			/*image lines.			Format:I8     */
char  	fdr028[9];    	/*The total (with filler data) number of      */
			/*pixels per line.		Format: I8    */
char  	fdr029[9];	/*The total (with filler data) number of      */
			/*image lines.			Format: I8    */
char  	fdr030[7];	/*Identification label of the media that the  */
			/*ID image was written to in format: ccnnnn   */
			/* cc 	=FE, ESA tape; 			      */
                	/*    	=FN, NASDA tape; 		      */
                	/*   	=FC, CCRS tape; 		      */
                	/*  	=AS, Archive signal DCRSi; 	      */
                	/*	=WS, Working signal DCRSi 	      */
char  	fdr031[17];	/*Location on the DCRSi where data begins.    */
char  	fdr032[17];	/*Location on the DCRSi where data ends.      */
char  	fdr033[17];	/*The name of the platform for the sensor     */
			/*that transmitted the SAR data.Format:A16    */
			/*= E-ERS1, European Space Agency Remote Earth*/
			/*Sensing Satellite 1 			      */
                        /*= J-ERS1,Japanese Earth Resource Satellite 1*/
                        /*= RADARSAT				      */
char  	fdr034[33]; 	/*Sensor and mode of operation.		      */
			/*AAAAAA-BB-CCCCCCDDDDDD-EFbbbbbbb            */
                       	/*AAAAAA= TBD, Sensor ID 		      */
                        /*BB 	= Lb, L-band; 			      */
                        /*  	= Cb, C-band (b is a trailing  blank) */
                        /*CCCCCC= HIGH, High resolution 	      */
                        /*    	= LOW, Low resolution 		      */
                        /*DDDDDD= TBD, Code for imaging mode 	      */
			/*(e.g., near,  far,  browse, scan, etc.)     */
                        /*E 	= H, Horizontal 		      */
                        /*   	= V, Vertical 			      */
                        /*F 	= H, Horizontal 		      */
                        /*   	= V, Vertical 			      */
char  	fdr035[17];	/*The pulse repetition frequency (PRF).       */
			/*				Format:F16.7  */
char  	fdr036[17];	/*The SAR antenna look angle.	Format:F16.7  */
char  	fdr037[17];	/*Data rate.			Format:F16.7  */
char  	fdr038[17];	/*Data window position.		Format:F16.7  */
char  	fdr039[17];	/*Range gate delay.		Format:F16.7  */
char  	fdr040[17];	/*Track angle to True North.	Format:F16.7  */
char  	fdr041[2];	/*Flag indicating whether the pass is	      */
			/*ascending or descending		      */
                        /* = A, Ascending, = D, Descending 	      */
char  	fdr042[17];	/*Altitude of the S/C at the image center.    */
			/*				Format:F16.7  */
char  	fdr043[23];	/*The S/C X-position at the image center.     */
			/*				Format: D22.15*/
char  	fdr044[23]; 	/*The S/C Y-position at the image center.     */
			/*				Format: D22.15*/
char  	fdr045[23];	/*The S/C Z-position at the image center.     */
			/*				Format: D22.15*/
char  	fdr046[23];	/*The S/C X-velocity at the image center.     */
			/*				Format: D22.15*/
char  	fdr047[23];	/*The S/C Y-velocity at the image center.     */
			/*				Format: D22.16*/
char  	fdr048[23];	/*The S/C Z-velocity at the image center.     */
			/*				Format: D22.16*/
char  	fdr049[15];	/*The S/C roll at the image center.           */
			/*				Format: E14.6 */
char  	fdr050[15];	/*The S/C yaw at the image center.	      */
			/*				Format: E14.6 */
char  	fdr051[15];	/*The S/C pitch at the image center.          */
			/*				Format: E14.6 */
char  	fdr052[5];	/*A quality flag for the S/C roll at the      */
			/*image center.			Format:I4     */
char  	fdr053[5];	/*A quality flag for the S/C yaw at the       */
			/*image center.			Format:I4     */
char  	fdr054[5]; 	/*A quality flag for the S/C pitch at the     */
			/*image center.			Format:I4     */
char  	fdr055[15];	/*The S/C roll rate at the image center.      */
			/*				Format: E14.6 */
char  	fdr056[15];	/*The S/C yaw rate at the image center.       */
			/*				Format: E14.6 */
char  	fdr057[15];	/*The S/C pitch rate at the image center.     */
			/*				Format: E14.6 */
char  	fdr058[5];	/*A quality flag for the  S/C roll rate at the*/
			/*image center.			Format: I4    */
char  	fdr059[5];	/*A quality flag for the S/C yaw rate at the  */
			/*image center.			Format: I4    */
char  	fdr060[5];	/*Quality flag for the S/C pitch rate at the  */
			/*image cente.			Format: I4    */
char  	fdr061[17];	/*Radius of the Earth at nadir.	Forma.F16.7   */
char  	fdr062[17];	/*Radius of the Earth at center of the image. */
			/*				Format:F16.7  */
char  	fdr063[17];	/*Incidence angle at the center of the image. */
			/*				Format:F16.7  */
char  	fdr064[8]; 	/*The version number of the ASP.	      */
			/*				Format:F7.2   */
char  	fdr065[3];	/*A two-character identifier describing the   */
			/*image processing			      */
			/* type = SF, Standard, full-res	      */
			/*      = QF, Quick-look, full-res	      */
char  	fdr066[2];	/*An identifier describing the type of 	      */
			/*ephemeris used.			      */
			/*1=Given/predicts			      */
			/*2=Restituted				      */
			/*3=Preliminary				      */
			/*4=Precise				      */
char  	fdr067[17];	/*The effective number of looks in azimuth.   */
			/*				Format:F16.7  */
char  	fdr068[17];	/*The effective number of looks in range.     */
			/*				Format:F16.7  */
char  	fdr069[17];	/*The weighting pedestal height in azimuth.   */
			/*				Format:F16.7  */
char  	fdr070[17];	/*The weighting pedestal height in range.     */
			/*				Format:F16.7  */
char  	fdr071[4];	/*The look energy normalization flag.         */
			/*= NOT,No				      */
char  	fdr072[17];	/*Known processing induced distortions        */
			/*in azimuth.Format:F16.7		      */
char  	fdr073[17];	/*Known processing induced distortions in     */
			/*range.			Format:F16.7  */
char  	fdr074[17]; 	/*Receiver gain.		Format:F16.7  */
char  	fdr075[17];	/*Swath velocity.		Format:F16.7  */
char  	fdr076[17];	/*Squint angle.			Format:F16.7  */
char  	fdr077[17];	/*Average terrain height above Geoid at       */
			/*image center.			Format:F16.7  */
char  	fdr078[4];	/*Processor gain.			      */
char  	fdr079[4];	/*A flag to indicate whether Doppler Skew     */
			/*was removed.				      */
			/*= NOT, Deskew not applied		      */
			/*= YES, Deskew applied			      */
char  	fdr080[7];	/*Ground range / Slant range flag.	      */
			/*= GROUND, ground range      		      */
			/*= SLANT, slant range			      */
char  	fdr081[17];	/*Slant range to the first image pixel.       */
			/*				Format:F16.7  */
char  	fdr082[17];	/*Slant range to the last image pixel.        */
			/*				Format:F16.7  */
char  	fdr083[9];	/*Starting sample of signal data in the range */
			/*line processed.		Format: I8    */
char  	fdr084[4];  	/*A flag to indicate whether Clutterlock      */
			/*was used.Format:A4; = NOT,No; = YES, Yes    */
char  	fdr085[17];	/*The Doppler frequency at the near range.    */
			/*				Format:F16.7  */
char  	fdr086[17];	/*The Doppler frequency slope.	Format:F16.7  */
char  	fdr087[17];	/*The Doppler frequency quadratic term.       */
			/*				Format:F16.7  */
char  	fdr088[4];	/*A flag to indicate whether Autofocusing.    */
			/*Format: A4,= NOT, No, = YES, Yes	      */
char  	fdr089[17];	/*The Doppler frequency rate at near range.   */
			/*				Format:F16.7  */
char  	fdr090[17];	/*The Doppler frequency rate slope.           */
			/*				Format:F16.7  */
char  	fdr091[17]; 	/*The Doppler frequency rate quadratic term.  */
			/*				Format:F16.7  */
char  	fdr092[17];	/*Nominal image resolution in azimuth.        */
			/*				Format:F16.7  */
char  	fdr093[17];	/*Nominal image resolution in range.          */
			/*				Format:F16.7  */
char  	fdr094[17]; 	/*Pixel spacing in azimuth.	Format:F16.7  */
char  	fdr095[17];	/*Pixel spacing in range.	Format:F16.7  */
char  	fdr096[4];	/*On-board range compression flag.	      */
			/*= NOT, No on-board range compression	      */
                        /*= YES, On-board range compression applied   */
char  	fdr097[5];	/*Bits per sample of the  SAR signal data.    */
			/*Format: I4				      */
char  	fdr098[17];	/*Calibrator estimate.		Format:F16.7  */
char  	fdr099[17];	/*Data transfer bit error rate.	Format:F16.7  */
char  	fdr100[17];	/*Signal to noise ratio.	Format:F16.7  */
char  	fdr101[17];	/*Estimated noise floor.	Format:F16.7  */
char  	fdr102[17];	/*Radiometric resolution.	Format:F16.7  */
char  	fdr103[9];     /*The number of saturated points determined    */
			/*from image histogram.			      */
char  	fdr104[4];    	/*A flag to indicate whether image is  within */
			/*specification.			      */
char  	fdr105[100];  	/*A comments field reserved for documenting   */
			/*image anomalies.			      */
}T_FDR_FILE,*T_FDR_PTR;


#endif /* ! _T_FDR_H */

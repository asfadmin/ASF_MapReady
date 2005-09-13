/*Interface to missing data & data window position 
routines.

Orion Sky Lawlor	5/13/1999
*/


/*This structure describes the number of missing 
(dropped, or skipped) frames and lines (aka auxiliary 
frames).
*/
typedef struct {
	int frames;/*Number of skipped frames*/
	int lines;/*Number of skipped auxiliary frames (lines)*/
} missing_data;


/************************* read data **********************
Find the number of missing (dropped) frames between frame
number 1 and frame number 2 in the given file.*/
void findMissing(bin_state *s,int frame1,int frame2,missing_data *missing);

/*Find the number of missing (dropped) frames 
just before the given frame in the given file.*/
void find1Missing(bin_state *s,int frame,missing_data *missing);

/*Return the expected data window position code at the given frame*/
int expectedDWP_code(bin_state *s,int frameNo);


/********************** write (initialize) data **********************
Create & initialize the missing data/DWP_code array for the 
given bin_state structure (which must already have its binary file open
and satellite initialized).
*/
void createMissing(bin_state *s);
void freeMissing(bin_state *s);

/*Add the given missing frames immediately before the given frame.
Missing data must be added in frame order.*/
void addMissing(bin_state *s,int frame,const missing_data *missing);

/*Add the given data window code starting at the given frame, 
and continuing onward.*/
void addDWP_code(bin_state *s,int frame,int DWP_code);



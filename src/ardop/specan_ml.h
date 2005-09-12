/*Perform multilooking on SPECAN output.
Only multilooks in range.*/

typedef struct {
	int nlooks;/*Number of looks in this set.*/
	float *looks;/*Array of floats (look power)*/
} lookset;

typedef struct {
	int wid,ht;/*Width and height of destination*/
	lookset *s;/*Array [wid] of look sets.*/
} specan_ml;


/*Create a specan_ml struct of the given size*/
specan_ml *specan_ml_init(int wid,int ht);


/*Add another radiometric look to the buffer.
DestX may be arbitrary (not necessarily in bounds).
Input buffer is one column of data, at least ht long.
*/
void specan_ml_look(specan_ml *ml,int destX,complexFloat *in);


/*Write the wid x ht floating-point output block to 
(row-by-row) dest,
multiplying each line by one coordinate of scallop,
which must be at least ht long.*/
void specan_ml_out(specan_ml *ml,float *scallop,float *dest);


/*Free a specan_ml struct*/
void specan_ml_free(specan_ml *ml);

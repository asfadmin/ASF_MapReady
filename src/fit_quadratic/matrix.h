/************************************************************
Include file for C matrix subroutines.

Orion Sky Lawlor 3/99
*/

typedef struct {
	int rows;/*# of rows (horiz. lines) in matrix*/
	int columns;/*# of columns (vert. lines) in matrix*/
	double **coeff;/*a [rows][columns] array of matrix coefficents.*/
} matrix;

matrix *matrix_alloc(int rows,int columns);/*Create a zero-valued matrix*/
matrix *matrix_dup(const matrix *source);
void matrix_free(matrix *doomed);
void matrix_print(matrix *this,const char *message,FILE *stream);

/*Row operations: Swap rows A and B.*/
void matrix_rowSwap(matrix *this,int A,int B);

/*Row operations: multiply [row dest] by scale*/
void matrix_rowScale(matrix *this,int dest,double scale);

/*Row operations: add scale*[row source] to [row dest]*/
void matrix_rowAddScale(matrix *this,int dest,double scale,int source);

/*Solve the given (columns>rows) matrix
by Gaussian Elimination.  Exits with error if
given matrix is singular.*/
void matrix_solve(matrix *this);




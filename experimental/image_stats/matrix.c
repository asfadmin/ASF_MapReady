/************************************************************
Implementation file for C matrix subroutines. 

Orion Sky Lawlor 3/99
*/
#include "asf.h"
#include "matrix.h"

/*A #define to make accessing matrix coefficients easier*/
#define m this->coeff

matrix *matrix_alloc(int rows,int columns)
{
	int row,col;
	matrix *this=(matrix *)MALLOC(sizeof(matrix));
	this->rows=rows;
	this->columns=columns;
	this->coeff=(double **)MALLOC(sizeof(double *)*rows);
	for (row=0;row<rows;row++)
	{
		m[row]=(double *)MALLOC(sizeof(double)*columns);
		for (col=0;col<columns;col++)
			m[row][col]=0.0;/*Initialize to zero*/
	}
	return this;
}
matrix *matrix_dup(const matrix *source)
{
	int r,c;
	matrix *dup=matrix_alloc(source->rows,source->columns);
	for (r=0;r<dup->rows;r++)
		for (c=0;c<dup->columns;c++)
			dup->coeff[r][c]=source->coeff[r][c];
	return dup;
}
void matrix_free(matrix *doomed)
{
	int row;
	for (row=0;row<doomed->rows;row++)
		FREE(doomed->coeff[row]);
	FREE(doomed->coeff);
	doomed->coeff=NULL;
	doomed->rows=-1;
	doomed->columns=-1;
	FREE(doomed);
}

void matrix_print(matrix *this,const char *message,FILE *stream)
{
	int r,c;
	fprintf(stream,"%s (%d rows/%d columns)\n",message,
		this->rows,this->columns);
	for (r=0;r<this->rows;r++)
	{
		for (c=0;c<this->columns;c++)
			fprintf(stream,"%f\t",m[r][c]);
		fprintf(stream,"\n");
	}
}

/*Row operations: Swap rows A and B.*/
void matrix_rowSwap(matrix *this,int A,int B)
{
	double *row_tmp=m[A];
	m[A]=m[B];
	m[B]=row_tmp;
}

/*Row operations: multiply [row dest] by scale*/
void matrix_rowScale(matrix *this,int dest,double scale)
{
	int col;
	for (col=0;col<this->columns;col++)
		m[dest][col]*=scale;
}

/*Row operations: add scale*[row source] to [row dest]*/
void matrix_rowAddScale(matrix *this,int dest,double scale,int source)
{
	int col;
	for (col=0;col<this->columns;col++)
		m[dest][col]+=scale*m[source][col];
}


/*Solve the given (columns>rows) matrix
by Gaussian Elimination.  Exits with error if
given matrix is singular.*/
void matrix_solve(matrix *this)
{
	int row,pivotRow,pivotCol;
	if (this->columns<=this->rows) {
	  sprintf(errbuf, "   ERROR: Matrix passed to matrix_solve doesn't have enough columns!\n");
	  printErr(errbuf);
	}
	for (pivotCol=0;pivotCol<this->rows;pivotCol++)
	{
		/*Search for a nonzero pivot for this column*/
		pivotRow=pivotCol;/*Start search at pivotCol*/
		while (fabs(m[pivotRow][pivotCol])<0.00000000001)
		{
			pivotRow++;
			if (pivotRow>=this->rows) {
			  sprintf(errbuf, "   ERROR: Matrix passed to matrix_solve is singular--\n"
					"   Cannot find a pivot for column %d!\n",pivotCol);
			  printErr(errbuf);
			}
		}
		/*Now that we've found a pivot row, swap it into place*/
		matrix_rowSwap(this,pivotRow,pivotCol);
		pivotRow=pivotCol;

		/*Scale the pivot row so the pivot has value 1.0*/
		matrix_rowScale(this,pivotRow,1.0/m[pivotRow][pivotCol]);
		
		/*Subtract the pivot from every other row*/
		for (row=0;row<this->rows;row++)
			if (row!=pivotRow)
				matrix_rowAddScale(this,
					row,-m[row][pivotCol],pivotRow);
	}
}








/*
	Matrix2D.c and Matrix2D.h:
A general Matrix2D data type, allowing: creation of translation, 
scale, and rotation matricies; multiplication of matricies; and
inversion of 2D matricies.

1997, ASF ISAR Tools.  By Orion Lawlor.
*/

#include "asf.h"


#include "Matrix2D.h"

#define i retMatrix->e
fPoint transformPoint(const Matrix2D *retMatrix,double x,double y)
{
	fPoint retPt;
	retPt.x=x*i[0][0]+y*i[1][0]+i[2][0];
	retPt.y=x*i[0][1]+y*i[1][1]+i[2][1];
	return retPt;
}
Matrix2D *identityMatrix2D(void)
{
	Matrix2D *retMatrix=(Matrix2D *)MALLOC(sizeof(Matrix2D));
	i[0][0]=1;i[1][0]=0;i[2][0]=0;
	i[0][1]=0;i[1][1]=1;i[2][1]=0;
	return retMatrix;
}
Matrix2D *translationMatrix2D(double tX,double tY)
{
	Matrix2D *retMatrix=(Matrix2D *)MALLOC(sizeof(Matrix2D));
	i[0][0]=1;i[1][0]=0;i[2][0]=tX;
	i[0][1]=0;i[1][1]=1;i[2][1]=tY;
	return retMatrix;
}
Matrix2D *scaleMatrix2D(double scaleX,double scaleY)
{
	Matrix2D *retMatrix=(Matrix2D *)MALLOC(sizeof(Matrix2D));
	i[0][0]=scaleX;i[1][0]=0;i[2][0]=0;
	i[0][1]=0;i[1][1]=scaleY;i[2][1]=0;
	return retMatrix;
}
Matrix2D *rotationMatrix2D(double angle)
{
	Matrix2D *m=identityMatrix2D();
	double cosine=cos(angle),sine=sin(angle);
	m->e[0][0]=cosine;
	m->e[1][0]=-sine;
	m->e[0][1]=sine;
	m->e[1][1]=cosine;
	return m;
}
Matrix2D *postMult2D(const Matrix2D *a,const Matrix2D *b)
{
	int x,y,k;
	Matrix2D *retMatrix=(Matrix2D *)MALLOC(sizeof(Matrix2D));
	for (x=0;x<3;x++)
		for (y=0;y<2;y++)
		{
			double tval=0;
			for (k=0;k<2;k++)
				tval+=a->e[x][k]*b->e[k][y];
			if (x==2) tval+=b->e[2][y];
			i[x][y]=tval;
		}
	return retMatrix;
}
Matrix2D *invertMatrix2D(const Matrix2D *source)
{
	Matrix2D *retMatrix=(Matrix2D *)MALLOC(sizeof(Matrix2D));
	double det=1.0/(source->e[0][0]*source->e[1][1]-source->e[1][0]*source->e[0][1]);
	double a=source->e[0][0],b=source->e[1][0],e=source->e[2][0],
		c=source->e[0][1],d=source->e[1][1],f=source->e[2][1];
	i[0][0]=det*d;i[1][0]=-det*b;i[2][0]=det*(b*f-d*e);
	i[0][1]=-det*c;i[1][1]=det*a;i[2][1]=det*(c*e-a*f);
	return retMatrix;
}
void printMatrix2D(const Matrix2D *retMatrix)
{
	printf("\t|-                                    -|\n");
	printf("\t|  %10.6f  %10.6f  %10.3f  |\n",i[0][0],i[1][0],i[2][0]);
	printf("\t|  %10.6f  %10.6f  %10.3f  |\n",i[0][1],i[1][1],i[2][1]);
	printf("\t|-                                    -|\n");
}

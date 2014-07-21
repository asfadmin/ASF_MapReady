typedef struct {
	double e[3][2];
} Matrix2D;

typedef struct {
	double x,y;
} fPoint;

fPoint transformPoint(const Matrix2D *retMatrix,double x,double y);
Matrix2D *identityMatrix2D(void);
Matrix2D *translationMatrix2D(double tX,double tY);
Matrix2D *scaleMatrix2D(double scaleX,double scaleY);
Matrix2D *rotationMatrix2D(double angle);
Matrix2D *postMult2D(const Matrix2D *a,const Matrix2D *b);
Matrix2D *invertMatrix2D(const Matrix2D *source);
void printMatrix2D(const Matrix2D *);

#include <stdio.h>
/* Eccentricity of the earth. */
#define ECC_E	8.1827385E-2
#define ECC2	(ECC_E*ECC_E)
#define DTR	0.01745329252
#define cosd(x) (double) cos((x)*DTR)
#define sind(x) (double) sin((x)*DTR)
#define SQR(A)  (double)((A)*(A))

double dot(double *vec0, double *vec1);
double mag(double *vec);
double calc_distance(double R, double lon0, double lat0, double lon1, double lat1);
/* get line from open file stream fptr */
int getFromFilei(FILE *fptr, char *seekFor);
double getFromFiled(FILE *fptr, char *seekFor);
void getFromFilec(FILE *fptr, char *seekFor, char *found);
double getSecd(double datetime);
double getSecc(char *datetime);

void swapi(int *first, int *second);
void swapd(double *first, double *second);
void swapf(float *first, float *second);
void swapc(char **first, char **second);

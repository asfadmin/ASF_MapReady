typedef struct {
	int rev,frame,vers;
	char name[200];
	char procvers[20];
	double yaw;
	double ceosDoppler;
	double prf;
	double dopChange;
} image;

image *readImage(char *name);
int dupImage(image *img,image *images[],int nImg);

typedef struct {
        double A,B,C,D,E,F,G,H,I,J,K;
} quadratic_2d;

double get_term(int termNo,double x,double y);
quadratic_2d find_quadratic(const double *out, const double *x,
                            const double *y, int numPts);
void quadratic_write(const quadratic_2d *c,FILE *stream);

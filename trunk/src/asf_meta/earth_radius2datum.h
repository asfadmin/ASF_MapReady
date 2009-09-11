/* some temporary functions that translate datum codes into minor and major axes - forth and back */

int earth_radius2datum(double re_major, double re_minor);
void datum2earth_radius(int datum, double *re_major, double *re_minor);

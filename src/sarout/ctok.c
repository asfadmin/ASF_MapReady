#include <stdio.h>
#include <stdlib.h>
#include <math.h>

void ctok (double sc_vec[6], double kepler[6]);
double tanfix(double a, double b);

/*  double  ctok (rx, ry, rz, vx, vy, vz, kepler)  --------------

**********************************************************************
*                                                                    *
* 'ctok.c'  converts the Cartesian State Vector to Kepler elements   *
*                                                                    *
**********************************************************************

INPUTS:
variables:      rx,ry,rz
type:           double
description:    Cartesian coordinates in the x,y,z axis in Km.

variables:      vx,vy,vz
type:           double
description:    Velocity in the x,y,z direction in Km/sec.

OUTPUTS:
variable:       kepler[6]
type:           double
description:    the resultant Kepler transformation consisting of the
                six Kepler elements,namely,
                'a' the semi-major axis in Km
                'e' the eccentricity
                'i' angle of inclination (deg)
                'Omega' longitude of ascending node (deg.)
                'w' argument of periapsis (deg.)
                'M' mean anomaly (deg.)
*/

void ctok (double sc_vec[6], double kepler[6])
{
    double  rx, ry, rz, vx, vy, vz;
    double  a, u, r, v, es, ec, e, E, M_deg, V2, H, i, i_deg;
    double  w_deg, cu, su, somega, comega, omega;
    double  pi, mu;

    pi = 3.141592653589793;
    mu = 3.9860045e+5;
    rx = sc_vec[0]; ry = sc_vec[1]; rz = sc_vec[2];
    vx = sc_vec[3]/1000.0; vy = sc_vec[4]/1000.0; vz = sc_vec[5]/1000.0;

    /* determine semi major axis 'a' */
    r = sqrt ((rx * rx) + (ry * ry) + (rz * rz));
    V2 = (vx * vx) + (vy * vy) + (vz * vz);
    a = (mu * r) / ((2.0 * mu) - (r * V2));

    /* determine eccentricity 'e' */
    es = ((rx * vx) + (ry * vy) + (rz * vz)) / sqrt (mu * a);
    ec = 1.0 - (r / a);
    e = sqrt ((es * es) + (ec * ec));

    /* determine mean anomaly'M' */
    E = 2.0 * tanfix ((e - ec), es);
    M_deg = (180.0 / pi) * (E - es);
    if (M_deg < 0.0)
        M_deg = M_deg + 360.0;

    /* determine angle of inclination 'i' */
    H = sqrt (mu * a * (1.0 - (e * e)));
    i = acos (((rx * vy) - (ry * vx)) / H);
    i_deg = i * (180.0 / pi);

    /* determine omega */
    somega = ((ry * vz) - (rz * vy)) / (sin (i) * H);
    comega = ((rx * vz) - (rz * vx)) / (sin (i) * H);
    omega = (180.0 / pi) * 2.0 * tanfix ((1.0 - comega), somega);
    if (omega < 0.0)
        omega = omega + 360.0;

    /* determine w_deg */
    su = rz / (r * sin (i));
    cu = ((ry / r) * somega) + ((rx / r) * comega);
    if (rz == 0.0)
        cu = 1.0;
    u = 2 * tanfix ((1.0 - cu), su);
    v = 2 * atan (sqrt ((1.0 + e) / (1.0 - e)) * tan (E / 2.0));
    w_deg = (180.0 / pi) * (u - v);
    if (w_deg < 0.0)
        w_deg = w_deg + 360.0;

    kepler[0] = a;
    kepler[1] = e;
    kepler[2] = i_deg;
    kepler[3] = omega;
    kepler[4] = w_deg;
    kepler[5] = M_deg;
}


/* tanfix(a,b) --------------------------------------------------------

        This routine calculates the tangent of a/b, protecting
        against b=0.
*/

double tanfix(double a, double b)
{
    double pi = 3.141592653589793;

    if (b == 0.0) {
        if (a < 0.0)
            return (-pi / 2.0);
        else if (a > 0.0)
            return (pi / 2.0);
        else
            return (0.0);
    }
    return (atan (a/b));
}


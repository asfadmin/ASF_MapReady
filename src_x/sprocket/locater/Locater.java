/*****************************************************************
CLASS: Locater

DESCRIPTION:
    Locates the targets inside a image.

HISTORY:
    DATE:    AUTHOR:    PURPOSE:
    --------------------------------------------------------------
    ??/????  ?. ?????   Initial Development

*****************************************************************/

import java.util.* ;
import java.awt.* ;
import java.io.* ;
import java.lang.*;

public class Locater {
    double northings[], eastings[];
    double Lat[], Lon[];

    /************************************************************************
     * Constructor:
     * Geolocates the point targets "targets" on the image described by "m" */
    public Locater ( Vector targets, Metadata m) {
        // Compute the "inverse flattening" as required by the converters
        m.ellipsoid_minor_axis = m.ellipsoid_minor_axis*1000.0;
        m.ellipsoid_major_axis = m.ellipsoid_major_axis*1000.0;

        double f83 = (m.ellipsoid_major_axis-m.ellipsoid_minor_axis)
                     / m.ellipsoid_major_axis;
        double f = (2*f83) - (f83*f83);

        // Allocate space for lon/lats
        Lon = new double[4];
        Lat = new double[4];

        // Longitudes
        Lon[0] = m.near_early_long ;
        Lon[1] = m.near_late_long ;
        Lon[2] = m.far_early_long ;
        Lon[3] = m.far_late_long ;

        // Latitudes
        Lat[0] = m.near_early_lat;
        Lat[1] = m.near_late_lat;
        Lat[2] = m.far_early_lat;
        Lat[3] = m.far_late_lat;

        /* Put the corner cordinates into the format required for geolocation
         * equations. */
        double zone[] = new double[4];

        eastings = new double[4];
        northings = new double[4];
        double Eastings[] = new double[4];
        double Northings[] = new double[4];
        utmc(Lon[0], 4, Lat, Lon, Eastings, Northings, m.ellipsoid_major_axis,
             f, zone);

        /* Notes from fortran code:
         *   !
         *   ! get image corner lat and lon values
         *   !
         *   cornerlat(1) = ne_lat   !
         *   cornerlon(1) = ne_lon   !     -----RNG---->
         *   cornerlat(2) = nl_lat   !    | 1         3
         *   cornerlon(2) = nl_lon   !    A
         *   cornerlat(3) = fe_lat   !    Z
         *   cornerlon(3) = fe_lon   !    I
         *   cornerlat(4) = fl_lat   !    | 2         4
         *   cornerlon(4) = fl_lon   !    V
         */

        eastings[0] = Eastings[0];  //ne
        eastings[1] = Eastings[1];  //nl
        eastings[2] = Eastings[2];  //fe
        eastings[3] = Eastings[3];  //fl

        northings[0] = Northings[0];
        northings[1] = Northings[1];
        northings[2] = Northings[2];
        northings[3] = Northings[3];

        for ( int a = 0; a < 4 ; a ++)
            System.err.println("(" + Lat[a] + "," + Lon[a] +")");
        for ( int a = 0; a < 4 ; a++)
            System.err.println("(" + eastings[a] + "," + northings[a] +")");

        //  Locate each target
        for ( int a = 0; a < targets.size(); a ++ )
           locate ( (Target) targets.elementAt(a) , m);

        try {
            PrintStream F = new PrintStream(new FileOutputStream(m.ground_targets_org));
            for ( int b = 0; b < targets.size(); b++ )
              ((Target) targets.elementAt(b)).print(F);
        }
        catch ( Exception e )  {
            System.err.println("ERROR: An error occured while writing to the file \""
                               + m.ground_targets_located+"\"." );
            System.err.println("ERROR: (" + e +")" );
        }
    }


    /**********************************************************
     * extract_double:
     * Function to read a double from a string.  If the value
     * is not a double then a error message is printed.      */
    private double extract_double ( String field_name, String field ) {
        try {
            return Double.parseDouble(field);
        }
        catch (NumberFormatException e) {
            System.err.println ( "ERROR:Could not convert field \""+field_name
                                 + "\" with a value of \"" + field +"\"");
            return 0;
        }
    }


    /*********************************************
     * locate:
     * 'locate's the target                     */
    private void locate ( Target t, Metadata m ) {
        double lat[], lon[];
        double n[],e[],zone[];

        n = new double[1];
        e = new double[1];
        zone = new double[1];
        lat = new double[1];
        lon = new double[1];

        /* Compute the "inverse flattening" required by the ellipsoid
         * computations */
        double f83 = (m.ellipsoid_major_axis-m.ellipsoid_minor_axis)
                     / m.ellipsoid_major_axis;
        double f = (2*f83) - (f83*f83);

        lat[0] = extract_double ("CS_LAT", t.cs_lat);
        lon[0] = extract_double ("CS_LONG", t.cs_long);

        utmc(lon[0],1, lat, lon,e, n, m.ellipsoid_major_axis, f, zone);

        t.op_x = e[0];
        t.op_y = n[0];
        t.zone = zone[0];

        en2normxy(eastings,northings,t,m.number_of_pixels,m.number_of_lines);
    }


    /****************************************************************
     * en2normxy:
     * Geolocation routine from pvs software "mpt"
     * Locates the target t in the trapazoid (E,N) */
    private void en2normxy ( double E[], double N[], Target t, double samples,
                             double lines) {
        double Ea, Eb, Ec, Ed, Na, Nb, Nc, Nd, Ei, Ni;
        double a, b, c, ac_b2;
        double xi, yi;
        Ei = t.op_x;
        Ni = t.op_y;

        Ea = E[0] - E[1] ;
        Eb = E[0] - E[1] - E[2] + E[3];
        Ed = E[2] - E[0];

        Na = N[0] - N[1];
        Nb = N[0] - N[1] - N[2] + N[3];
        Nd = N[2] - N[0];

        a  = Ed*Nb - Eb*Nd;
        Ec = E[0] - Ei;
        Nc = N[0] - Ni;
        b  = Ea*Nd - Ed*Na + Ec*Nb - Eb*Nc;
        c  = Ea*Nc - Ec*Na;
        ac_b2 = a*c/(b*b);
        if (Math.abs(ac_b2) < (1E-6)) {
            yi = c*(-1)/b;
        }
        else {
            yi = b*(-1)*(1.0 - Math.sqrt(1.0 - 4.0*ac_b2))/(2*a);
        }

        xi = (Ec + yi*Ed)/(Ea - yi*Eb);
        t.projected_image_y = xi * lines;
        t.projected_image_x = yi * samples;

        if( (yi < 0.0) || (yi > 1.0) || (xi < 0.0) || (xi > 1.0) ) {
            System.err.println("ERROR:Targeti \"" + t.cs_dev_id
                               + "\" is not in image");
            return;
        }
        System.err.println( t.cs_dev_id + " ( " +  xi + "," +  yi +")");
        System.err.println( "X = " + t.projected_image_x );
        System.err.println( "or = " + ((c*(-1)/b) * samples) );
    }


    /*************************************************************************
     * utmc:
     ! utmc.f contains coordinate system transformation subroutines
     !
     ! author:  Joanne Shimada
     !
     ! utmc(): convert lat/lon to universal transverse mercator (utm)
     ! coordinates (easting, northing)
     !
     !        NOTE: modified UTM ! Delta y is always 1e+7 also on the
     !        northern hemisphere. Therefore the northing is 1e+7 larger
     !        than the true northing for latitudes larger than 0.
     !        The advantage of this approach is that inversion is possible
     !        without additional information on whether the location is on the
     !        southern or northern hemisphere.
     !
     ! input parameters:
     !       blon- base longitude
     !       tlat- array of latitude coordinates
     !       tlon- array of longitude coordinates
     !     numpts- number of points to convert
     !    eoffset- cross track error \ of the input image frame, use in
     !    noffset- along track error / terrain correction processing only
     !          a- semi-major (equatorial) axis of the earth (m)
     !        esq- eccectricity square
     !         k0- central scale factor
     !
     ! output parameters:
     !         tx- array of easting coordinates
     !         ty- array of northing coordinates
     !       zone- zone
     !
     ! variables:
     !        esq- eccentricity square
     !       epsq- e-prime squared
     !          a- semi major axis
     !         k0- center scale factor
     !          n- zone
     !      long0- central meridian
     !        dtr- degree to radian
     !       xadj- easting adjustment
     !       yadj- northing adjustment
     !
     ! Reference: Snyder, J. P. 1983, Map projection used by the US
     !            geological survey(bulletin 1532)
     !
     **************************************************************************
     * 04/21/97 - Devin Lando - Added new parameter to contain the base
     *                          longitude. Previously, utmc used the first
     *                          element in the tlon parameter to find the base
     *                          zone.                                        */
    void utmc(double blon, int number, double tlat[], double tlon[],
              double tx[], double ty[], double a, double esq, double zone[]) {
        int n;
        double lat, LONG, long0, a1, a2, a3, rn;
        double t, b1, b2, rm, rm0, dtr;
        double tanlat, c;
        double yadj, xadj;
        double e4, e6, c1, c2, c3, c4;
        double epsq;
        double eoffset = 0.0;
        double noffset = 0.0;
        double k0 = 0.9996;       /* Don't know about this one.. */

        dtr  = 4.0 * Math.atan(1.0)/180.0;
        epsq = esq/(1.0 - esq) ;

        //calculate the zone no. and long0

        n     = (int) ((180 + blon)/6+1);
        long0 = (n-1)*6.0 + 3.0 - 180.0;

        for ( int i = 0; i < number ; i++ ) {
            lat    = tlat[i] * dtr;
            LONG   = tlon[i] * dtr;
            rn     = a/Math.sqrt(1.0 - esq*(Math.pow( Math.sin(lat), 2.0))) ;
            tanlat = Math.sin(lat)/Math.cos(lat);
            t      = tanlat * tanlat;
            c      = epsq * Math.pow( Math.cos(lat), 2.0);
            a1     = Math.cos(lat) * (tlon[i] - long0) * dtr;
            a2     = (1.0 - t + c) * Math.pow(a1,3) / 6.0 ;
            a3     = (5.0 - 18.0*t + Math.pow(t,2) + 72.0*c - 58.0*epsq)
                     * Math.pow(a1,5) / 120.0;
            xadj   = 5E5;
            tx[i]  = (k0 * rn * (a1 + a2 + a3) + xadj + eoffset );
            e4     = esq * esq;
            e6     = e4 * esq;
            c1     = 1.0 - esq/4.0 - 3.0*e4/64.0 - 5.0*e6/256.0;
            c2     = 3.0*esq/8.0 + 3.0*e4/32.0 + 45.0*e6/1024.0;
            c3     = 15.0*e4/256.0 + 45.0*e6/1024.0;
            c4     = 35.0*e6/3072.0;
            rm     = a*(c1*lat - c2*Math.sin(2.0*lat) + c3*Math.sin(4.0*lat)
                     - c4*Math.sin(6.0*lat));
            rm0    = 0.0;
            b1     = Math.pow(a1,2)/2.0+ (5.0 - t + 9.0*c + 4.0 * c*c)
                     * Math.pow(a1,4)/24.0;
            b2     = (61.0 - 58.0*t + t*t + 600.0*c - 330.0*epsq)
                     * Math.pow(a1,6) / 720.0;
            yadj   = 1E7;
            ty[i]  = (k0*(rm - rm0 + rn*tanlat*(b1 + b2)) + yadj +noffset ) ;
            zone[i] = n;
        }
    }


    /******************************************************
     * Main Function                                     */
    public static void main ( String [] args) {
        //First verify that the proper arguements are in place
        if (args.length != 1 ) {
            System.err.println("Usage:  Locator <basename>\n");
            return;     //Whats the return type of main in java anyway?
        }

        InputStream in;

        try {
            // Read the metadata
            Metadata m = new Metadata(args[0]);

            // Open the targets
            in = new FileInputStream(
                            new File (args[0] + Constants.ground_target_file));

            //Read targets
            TargetIngester t = new TargetIngester(in);
            Vector v = t.getElements();

            //Locate targets
            Locater L = new Locater (v, m);
        }
        catch (Exception e) {
            System.err.println( "An Error occured while attempting to locate targets:" + e );
        }
    }

}

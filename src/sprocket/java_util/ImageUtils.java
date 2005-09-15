class ImageUtils {
  static double log10 ( double x )
    {
	return ( Math.log(x) / Math.log(10.0) );
    }

  static double sigma02gama0 (double inc, double sigma0)
     {
  	return (sigma0 - 10.0 * log10 (Math.cos (toRad (inc))));
     }

  static double toDeg (double theta)
     {
  	return ((theta / Math.PI) * 180.0);
     }

  static double toRad (double theta)
     {
  	return ((theta / 180.0) * Math.PI);
     }

  static double look2incidence (double look, double Re, double H)
     {
  	return toDeg ((Math.asin ((Re + H) * Math.sin (toRad (look)) / Re)));
     }

   static double incidence2look (double inc, double Re, double H)
     {
  	return toDeg (Math.asin ((Math.sin (toRad (inc)) / (Re + H)) * Re));
     }

   static double slant2ground (double sl, double Re, double H)
     {
  	double ReH;
  	ReH = Re + H;
  	return ((Re * Math.acos ((Re * Re + ReH * ReH - sl * sl) / (2.0 * Re * ReH))));
     }

   static double ground2slant_range (double r /* ground range */ ,
                           double Re /* height above platform */ ,
                           double H /* Radius of Earth */ )
      {
  	double p;
  	p = r / Re;
  	return (Math.sqrt (2.0 * Re * (Re + H) * (1.0 - Math.cos (p)) + H * H));
      }

   static double slant2look (double R /* slant range */ ,
                   double Re /* height */ ,
                   double H     /* Radus of earth at nadir */
  		)
      {

  	/* (R2 + 2HRe + H2) / {2R(Re + H)} */
  	return toDeg (Math.acos ((R * R + 2.0 * H * Re + H * H) / (2.0 * R * (Re + H))));
       }


   static double look2slant (double look, double Re, double H)
      {
  	/* R^2 = 2Re(Re + H)(1 - cos P ) + H2  */
  	double P, R2;
  	P = look2incidence (look, Re, H) - look;
  	R2 = 2.0 * Re * (Re + H) * (1.0 - Math.cos (toRad (P))) + H * H;
  	return (Math.sqrt (R2));
      }
 }

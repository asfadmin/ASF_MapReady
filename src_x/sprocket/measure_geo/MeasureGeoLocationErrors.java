//      MeasureGeoLocationErrors
//	   Reports the geolocation errors 
import java.util.* ;
import java.awt.* ;
import java.io.* ;
import java.lang.*;

public class MeasureGeoLocationErrors  
   {



      private void printer( String s)
        {
		 System.out.println(s);
        }

      public MeasureGeoLocationErrors ( Vector v, Metadata m )
        {

	   int a;	
	   double dir;
	   if ( m.asending_or_desending.equals("DESCENDING") )
		dir = -1.0;
	   else
		dir = 1.0;

           for ( a = 0; a < v.size(); a ++)
	     {

		Target t = (Target)v.elementAt(a);

			/* Compute difference in X */

			/* Compute slant range for operator selected pixel */
		double actual_target_location_sl = 
				m.slant_range_to_first_pixel + 
				dir*m.pixel_size_range * t.actual_image_x / 1000.0 ;

			/* Compute slant range for computed pixel */
                double computed_target_location_sl = m.slant_range_to_first_pixel + 
				dir*m.pixel_size_range * t.projected_image_x /1000.0;

			/* Compute actual target location */
		double actual_target_location_gr = ImageUtils.slant2ground(
				actual_target_location_sl, m.earth_radius_at_center,
				m.platform_altitude );

		double computed_target_location_gr = ImageUtils.slant2ground(
                               computed_target_location_sl, m.earth_radius_at_center, 
                               m.platform_altitude );
		

		printer (" ");
		printer ("For " + t.cs_dev_id );
                printer ( "Slant Range of target :\t" + actual_target_location_sl);
                printer ( "Slant Range of target (predicted):\t" + computed_target_location_sl);


		printer( "Ground Range of target:\t" + actual_target_location_gr);


		actual_target_location_gr =
                        adjust_for_height ( actual_target_location_gr,
                                Double.parseDouble(t.cs_elev)/1000.0,
                                m.earth_radius_at_center,
                                m.platform_altitude) ;

		printer( "Ground Range of target with height adjustment:" + actual_target_location_gr );
		printer( "Ground Range of target (predicted):" 
				+ computed_target_location_gr);
		printer( "Error in range : " + 
				(actual_target_location_gr - computed_target_location_gr) *1000.0  );

			/* Compute difference in Y */
		printer( "Error in azmuth: " + 
				(t.projected_image_y - t.actual_image_y)*m.pixel_size_az );
	     }
        }

      public static void main ( String [] args)
        {
                //First verify that the proper arguements are in place
         if (args.length != 1 )
            {
                System.err.println("Ussage Locator <basename>\n");
                return;         //Whats the return type of main in java anyway?
            }

         InputStream in;

         try {
                        // Read the metadata
                Metadata m = new Metadata(args[0]);

                        // Open the targets
                in = new FileInputStream(
                        new File (args[0] + Constants.ground_target_file_located));

                        //Read targets
                TargetIngester t = new TargetIngester( in);
                Vector v = t.getElements();

                        //Locate targets
                MeasureGeoLocationErrors measure = new MeasureGeoLocationErrors ( v, m);
           }
         catch (  Exception e )
           {
                 System.err.println( "An Error occured while attempting to locate targets:" + e );
           }

        }

 double incirad(double sl, double trgelv, double earthrd, double platalt)
    {
      double r,rph;
      r = earthrd + trgelv;
      out("in incirad, r = " + r );
      rph = earthrd + platalt;
      out ("in incirad, rph = " + rph);
      return Math.asin( Math.sin( Math.acos( (sl*sl+rph*rph-r*r)/(2.0*sl*rph)))*rph/r);
	
	/*  dasin( dsin( dacos( (sl**2+rph**2-r**2)/(2.*sl*rph)))*rph/r) */

     }

 double adjust_for_height ( double gr, double trgelv, double Re, double H )
    {
       //double hadj_sl =  slant(gr, trgelv, earthrd, platalt);
       //double incid = incirad(  hadj_sl, 0.0, earthrd, platalt );

       double incid = ImageUtils.look2incidence(
                        ImageUtils.slant2look(
                                ImageUtils.ground2slant_range(gr,Re,H), Re, H ), Re, H );
      double adj = ( trgelv) / Math.tan (ImageUtils.toRad(incid) );
      return gr + adj;
    }


	/* Ground to slant w/ target height adjustment*/
	/* From old PVS code, just converted from fortran */
  double slant(double gr, double trgelv, double Re, double H)
        {

             double r = Re + trgelv;
             double rph = Re + H ;
             return (Math.sqrt(r*r+rph*rph-2.0*r*rph*Math.cos(gr/r) ));
        }

	/* Slant to ground w/ target height adjustment */
	/* From old PVS code, just converted from fortran */
  double ground(double sl,double trgelv, double Re, double H)
      {
       double r = Re + trgelv;
       double rph = Re + H;
       return  r *Math.acos( (r*r+rph*rph-sl*sl)/(2.0*r*rph)  );
      }

  void out ( String in )
   {
	System.err.println ("MeasureGeoLocationErrors: " + in);
   }
}

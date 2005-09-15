import java.io.*;
import java.text.*;


public class point_target_analysis_results {
    public double THETA;
    public double clutter_power;
    public double azimuth_pslr;
    public double range_pslr;
    public double azimuth_islr;
    public double range_islr;
    public double azimuth_res;
    public double range_res;
    public double peak_power;
    public double SCR;
    public double err_azimuth_res;
    public double err_range_res;
    public double err_azimuth_pslr;
    public double err_range_pslr;
    public double err_azimuth_islr;
    public double err_range_islr;
    String ln;

    point_target_analysis_results (  double theta,
                                     clutter ct,
                                     pslr pslr_rg,
                                     pslr pslr_az,
                                     islr islr_rg,
                                     islr islr_az,
                                     resolution res_rg,
                                     resolution res_az,
                                     double pp,
                                     double scr)
    {

		// Save values
        THETA = theta;
        clutter_power = ct.clutter_power;
        azimuth_pslr = pslr_az.value;
        range_pslr = pslr_rg.value;
        azimuth_islr = islr_az.value;
        range_islr = islr_rg.value;
        peak_power = pp;
        SCR = scr;

        azimuth_res = res_az.res;
        range_res = res_rg.res;

        err_azimuth_res = res_az.error;
        err_range_res = res_rg.error;
        err_azimuth_pslr = pslr_az.err;
        err_range_pslr = pslr_rg.err;
        err_azimuth_islr = islr_az.err;
        err_range_islr = islr_rg.err;

	


	ln = new String();

        DecimalFormat dm = new DecimalFormat();
        dm.setMaximumFractionDigits(2);

	ln  = ln + pad(dm.format(THETA));
	ln  = ln + pad( dm.format(SCR) ); 

	if ( res_rg.valid == true )
		ln  = ln +    pad(dm.format(range_res));
	else
		ln = ln + pad ("  -  ");

 	if ( res_az.valid == true )
		ln = ln + pad(dm.format(azimuth_res));
	else
                ln = ln + pad ("  -  ");

	if ( pslr_rg.valid == true )
                ln = ln  + pad(dm.format(range_pslr)) ;
	else 
		ln = ln + pad ("  -  ");

	if ( pslr_az.valid == true )
		ln = ln  + pad(dm.format(azimuth_pslr)) ;
	else 
		ln = ln + pad ("  -  ");

	if ( pslr_rg.valid == true )
		ln = ln + pad(dm.format(range_islr));
	else 
		ln = ln + pad ("  -  ");

	if ( pslr_az.valid == true )
                ln = ln + pad(dm.format(azimuth_islr));
        else 
                ln = ln + pad ("  -  ");

        ln = ln + pad(dm.format(peak_power));
	
    }

    public void print (String target_name)
    {
        print ( System.err, target_name);
    }

	/* 
		pad -> pads string out to 11 chars long 
	*/
    private String pad( String foo )
	{
	   int g = 11 - foo.length();
	   for (; g > 0; g--)
		foo = foo + " ";
	   return foo;
	}

    static void print_header ( int sz, int ov )
	{
		print_header( System.out, sz, ov);
	}
    static void print_header ( PrintStream pr , int sz, int ov)
	{

	pr.println("    **** IMAGE QUALITY ANALYSIS ****");
	pr.println("       Block Size: " + sz);
	pr.println("       Over Sample Factor:" + ov);
	String header = "TARGET     " +
                        "THETA      " +
                        "SCR        " +
                        "Res(ra)    " +
                        "Res(az)    " +
                        "PSLR(ra)   " +
                        "PSLR(az)   " +
                        "ISLR(ra)   " +
                        "ISLR(az)   " +
                        "Peak Pwr   " ;

        pr.println(header);

	}
	

    public void print ( PrintStream pr, String target_name)
    {
        DecimalFormat dm = new DecimalFormat();
        dm.setMaximumFractionDigits(2);

	/*
        String ln =  pad(target_name)
		     + pad(dm.format(THETA)) 
                     + pad(dm.format(SCR)) 
                     + pad(dm.format(range_res)) 
                     + pad(dm.format(azimuth_res)) 
                     + pad(dm.format(range_pslr))
                     + pad(dm.format(azimuth_pslr))
                     + pad(dm.format(range_islr))
                     + pad(dm.format(azimuth_islr))
                     + pad(dm.format(peak_power));
	*/
        pr.println(pad(target_name) + ln);
    }

}


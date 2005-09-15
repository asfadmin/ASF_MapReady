//*   data_cuts.java
//*	Code to compute the 1d measurements for image quality.
//*
//*   Public Member Functions
//*	get_1d_meas -> Returns the 1d measurements for the point target.
//*
//*   Private Member Functions
//*	cabs -> returns the absolute value of i/q.
//*
//*	alog10 -> computes the log base ten (10)
//*
//*	islr_1d -> Calculate the 1-dimensional ISLR.
//*
//*	integrate -> sum the power of the mainlobe.
//*
//*	find_sidelobe_1d -> finds the sidelobe in one direction.
//*
//*	get_clutter  -> Calculate the average clutter power around a point target.
//*
//*	find_1d_res -> computes the resolution of the main lobe.
//*
//*	uncert_res -> This function returns the uncertainty in the measurement of the 1-D
//*               resolution.
//*
//*	find_mainlobe_1d ->  Find the bounds of the mainlobe within the 1-dimensional impulse response.
//*
//*	uncert_pslr -> Returns the uncertainty in the measurement of the PSLR.
//*
//*	uncert_islr ->  Returns the uncertainty in the measurement of the ISLR.
//*

import java.util.* ;
import java.awt.* ;
import java.io.* ;
import java.lang.*;

public class data_cuts
{
    double azmuth[];
    double range[];

    int peak_azmuth;
    int peak_range;
    image_data oversampled_data;
    int size;
    Metadata m;
    double THETA;		//elev angle

    data_cuts ( image_data ov, Metadata mm , double theta)
    {

	DEBUG.logger("data_cuts( theta -> " + theta + ")");

        //Save elev angle
        THETA = theta;

        //Save metadata
        m = mm;

        //Allocate space
        oversampled_data = ov;

        //Allocate space
        azmuth = new double[ov.oversampled_size];
        range = new double[ov.oversampled_size];

        size = ov.oversampled_size;
        peak_azmuth = ov.peak_y;
        peak_range = ov.peak_x;
        int peak_x = ov.peak_x;
        int peak_y = ov.peak_y;

        for ( int i = 0; i < size; i ++ )
        {

             azmuth[i] = Math.sqrt (ov.OI[peak_x][i]*ov.OI[peak_x][i] +
                                  ov.OQ[peak_x][i]*ov.OQ[peak_x][i] );

	     range[i] = Math.sqrt(ov.OI[i][peak_y]*ov.OI[i][peak_y] +
                                 ov.OQ[i][peak_y]*ov.OQ[i][peak_y] );
        }


	DEBUG.logger("data_cuts( END )");
    }
	
	
    void cuts_write ( String s )
	{
        try {
            PrintWriter out = new PrintWriter( new FileOutputStream (s+"_data_cuts.cdf") );

	    out.println("\"Range\",\"Azmuth\"");
	    double max_range = range[0];
	    double max_azmuth = azmuth[0];
	
	    for ( int x= 0; x<size; x++)
	       {
		 if (range[x] > max_range)
			max_range = range[x];
		 if (azmuth[x] > max_azmuth)
			max_azmuth = azmuth[x];
	       }
	     max_range =  Math.log( max_range);
	    max_azmuth = Math.log(max_azmuth);
	

            for ( int x= 0; x<size; x++)
		out.println(   (20*(Math.log(range[x]) - max_range) / Math.log(10) )+ ","+
			 (20*(Math.log(azmuth[x]) - max_azmuth) / Math.log(10) ) );

	    out.close();
        }
        catch (java.io.IOException e)
        {
            System.err.println("ERROR:\t -> \""+e+"\"");
            System.exit(-1);
        }
	}

    /*
    	get_1d_meas -> returns the 1d measurements for a point target
    */

    /*
    c Reference
    c     S. N. Madsen, "Specification of the Single Point Target Analysis
    c         Module, SIR-C Calibration Processor," JPL IOM 3348-90-40,
    c         March 16, 1990.
     */

    point_target_analysis_results get_1d_meas( double crit )
    {

	DEBUG.logger ("get_1d_meas( crit -> " + crit + ")");

        /*
                c------------------------------------------------------------------------------
                c First, calculate nominal azimuth and range resolutions from the oversampled
                c data by assuming that the clutter power is zero.  Use the nominal resolutions
                c to determine the bounds of the area over which the clutter power will be
                c calculated using the ORIGINAL data.  After calculating the clutter power,
                c recalculate the azimuth and range resolutions using the OVERSAMPLED data
                c and then calculate the PSLR, ISLR, peak power, and SCR.
                c------------------------------------------------------------------------------
         */

        /*
        c Find the bounds of the mainlobe within each one-dimensional cut, assuming
        c that the clutter power is zero.
         */

        double clutter_power = 0.0;
        int box = 20;

        lobe az_lobe = find_mainlobe_1d(false, clutter_power, crit);
	lobe range_lobe = find_mainlobe_1d(true,clutter_power , crit);

        /*
        c Calculate the corresponding azimuth and range resolutions from the
        c one-dimensional cuts.
         */

        resolution az_res = find_1d_res(az_lobe, m.pixel_size_az,  azmuth, peak_azmuth, clutter_power );
        resolution rng_res = find_1d_res(range_lobe, m.pixel_size_range, range, peak_range, clutter_power );

        /*
        c Find the peak of the original data.
         */
        location org_loc = oversampled_data.findmax_cmpx2d_win(oversampled_data.I, oversampled_data.Q);

        /*
        c Calculate the (two-dimensional) clutter power.
         */

        //Not sure why this is done
        az_res.res  = az_res.res/m.pixel_size_az;
        rng_res.res = rng_res.res/m.pixel_size_range;

        clutter clutter_2d = get_clutter( az_res, rng_res, org_loc);
        clutter_power = clutter_2d.clutter_power;

        //Recalculate resolution with the computed clutter power
        az_res = find_1d_res(az_lobe, m.pixel_size_az,  azmuth, peak_azmuth, clutter_power );
        rng_res = find_1d_res(range_lobe, m.pixel_size_range, range, peak_range, clutter_power );


        double mwidth =  2.6;  		//default mainlobe width
        double swidth = 20.0;		//default sidelobe width

        side_lobe neg_az = find_sidelobe_1d(azmuth, 1, peak_azmuth,
                                            mwidth, az_lobe.pos);
        side_lobe pos_az = find_sidelobe_1d(azmuth, -1, peak_azmuth,
                                            mwidth, az_lobe.neg);

        side_lobe neg_rg = find_sidelobe_1d(range,1 , peak_range,
                                            mwidth, range_lobe.pos);
        side_lobe pos_rg = find_sidelobe_1d(range,-1 , peak_range,
                                            mwidth, range_lobe.neg);

        pslr pslr_az = pslr_1d( azmuth, peak_azmuth, neg_az, pos_az, clutter_power);
        pslr pslr_rg = pslr_1d( range, peak_range, neg_rg, pos_rg, clutter_power);

        islr islr_az = islr_1d( peak_azmuth, azmuth, mwidth, swidth, clutter_2d, az_lobe, box );
        islr islr_rg = islr_1d( peak_range, range, mwidth, swidth, clutter_2d, range_lobe, box );

        double peak_power = Math.pow(
                                cabs(
                                    oversampled_data.OI[oversampled_data.peak_x][oversampled_data.peak_y],
                                    oversampled_data.OQ[oversampled_data.peak_x][oversampled_data.peak_y]
                                ), 2);

        double SCR = 10 * alog10((peak_power - clutter_2d.clutter_power)/clutter_2d.clutter_power);
        peak_power = 10 * alog10(peak_power);

	DEBUG.logger ("get_1d_meas( END )");

        return ( new point_target_analysis_results (THETA, clutter_2d,
                 pslr_rg,
                 pslr_az,
                 islr_rg,
                 islr_az,
                 rng_res,
                 az_res,
                 peak_power,
                 SCR) );
    }


    /*
    	cabs -> returns the absolute value of i/q.
	
    */
    private double cabs ( double i, double q )
      {
    	return ( Math.sqrt(i*i + q*q ) );
      }


    /*
    alog10 -> computes the log base ten (10) 
     */
    private double alog10( double x )
    {
        return ( Math.log(x) / Math.log(10) );
    }


    /*
    islr_1d -> Calculate the 1-dimensional ISLR.

    c Calculate the 1-dimensional ISLR.  The ISLR and the uncertainty estimate
    c for the ISLR are defined in S. Madsen's memo (see reference below).
    c
    c External routines
    c     uncert_islr            Calculate an uncertainty estimate for the ISLR
    c
    c Inputs
    c     peak                   Location of mainlobe peak within impulse response
    c     N                      Length of impulse response
    c     cut                    One-dimensional impulse response
    c     mwidth, swidth         Multiplicative factors defining widths of mainlobe
    c                                and sidelobe regions relative to the resolution
    c     clutter_power          Clutter power
    c     mainlobe_start,        Locations of start and end of mainlobe within
    c                                one-dimensional impulse response
    c     mainlobe_end
    c     box                    Width of region used to calculate clutter power,
    c                                in resolution cells
    c     id                     Identifier ("azimuth" or "range") for debugging
    c
    c Outputs
    c     islr_db                ISLR (dB)
    c     err_islr_db            Uncertainty estimate for ISLR (dB)
    c
    c Variables
    c     err_islr               Uncertainty estimate for ISLR (%)
    c     islr                   ISLR (dimensionless)
    c     msum                   Energy contained within mainlobe region
    c     ncells                 Number of resolution cells contained within
    c                                sidelobe region
    c     ncorr                  Number of pixels within resolution width
    c     ssum                   Energy contained within sidelobe region
    c
    c Reference
    c     S. N. Madsen, "Specification of the Single Point Target Analysis
    c         Module, SIR-C Calibration Processor," JPL IOM 3348-90-40,
    c         March 16, 1990.
    c
    */

    islr islr_1d( int peak,
                  double cut[],
                  double mwidth,
                  double swidth,
                  clutter ct,
                  lobe lb,
                  int box )
    {

	DEBUG.logger("islr_1d ( peak" + peak + " mwidth -> " + mwidth + " swidth -> " + swidth + ")");

		//Verify that both the clutter and the lobes are valid
	if ( ct.valid == false || lb.valid == false )
		return new islr (false);

	int mainlobe_start = lb.neg;
	int mainlobe_end = lb.pos;
	double clutter_power = ct.clutter_power;
	int n_clutter =  ct.n_clutter;
	int ncells = (int) ( box - mwidth );
        double ncorr = mainlobe_end - mainlobe_start + 2;


        /*
        c Calculate the energy contained in the mainlobe.
        */

        double msum = integrate(peak, cut, mwidth, mainlobe_start, mainlobe_end,
                                clutter_power);

        /*
        c Calculate the energy contained in the region spanned by the sidelobes.
        c The actual sidelobe energy will be this sum minus the mainlobe energy.
        */

        double ssum = integrate(peak, cut, swidth, mainlobe_start, mainlobe_end,
                                clutter_power);

        /*
        c Calculate the ISLR and the uncertainty estimates.  First check that
        c the clutter power is not larger than the signal power; if so, set
        c the calculated ISLR and uncertainty estimate to illegal values.
        */

        ncorr = mainlobe_end - mainlobe_start + 2;

        double tislr, islr_db, err_islr, err_islr_db;

        if (msum > 0.0 && ssum > 0.0)
        {
            tislr = (ssum - msum) / msum;
            if (ssum > msum)
            {
                islr_db = 10.0 * Math.log(tislr)/Math.log(10);
                err_islr = 100.0 * uncert_islr(clutter_power, ncells,
                                               n_clutter, msum, ssum-msum, ncorr);
                if (err_islr > 0)
                {
                    err_islr_db = 10*Math.log(1.0 + err_islr / 100.0)/ Math.log(10);
		    DEBUG.logger("islr_1d (END) ");
                    return new islr ( islr_db, err_islr_db);
                }
                else
		   {
		    DEBUG.logger("islr_1d (END) ");
                    return new islr ();
		   }
            }
            else
	      {
		DEBUG.logger("islr_1d (END) ");
                return new islr ();
	      }
        }
        else
	  {
	    DEBUG.logger("islr_1d (END) ");
            return new islr();
	  }
    }


    /*
    integrate -> sum the power of the mainlobe.
    */
    private double integrate( int peak,
                              double cut[],
                              double width_factor,
                              int mainlobe_start,
                              int mainlobe_end,
                              double clutter_power)
    {
        int dist;
        int i;
        int lower_bound, upper_bound;
        int N = cut.length;

        /*
        c Find the lower and upper bounds of the area to integrate.
        */


        dist = (int)((peak - mainlobe_start) * width_factor +.5);

        lower_bound = peak-dist;
        if ( lower_bound < 0 )
            lower_bound = 0;

        dist = (int)((mainlobe_end - peak) * width_factor+.5);

        upper_bound = peak+dist;
        if (upper_bound > N - 1)
            upper_bound = N -1;

        /*
        c Integrate the power contained within the above-defined area.
        */

        double sum = 0.0;
        for (i = lower_bound; i <= upper_bound; i++)
            sum = sum + cut[i]*cut[i];

        /*
        c Indicate an error if the clutter power is larger than the (average) value
        c of the signal power over the region of interest.
        */

        if ((upper_bound-lower_bound+1)*clutter_power > sum)
            sum = -1.0;

        return sum;
    }

    /*
    find_sidelobe_1d -> finds the sidelobe in one direction.
    */
    /*
    c
    c Inputs
    c     N                      Length of one-dimensional cut
    c     cut                    One-dimensional impulse response
    c     sign                   Direction in which to search from peak
    c     peakpos                Location of mainlobe peak in cut
    c     mwidth                 Mainlobe width factor
    c     mainlobe_bound         Mainlobe bound determined by m-dB criterion
    c
    c Outputs
    c     maxpos                 Position of sidelobe in indicated direction
    c     error                  Error flag
    c
    c Variables
    c     i                      Indexing variable
    c     current                Current maximum power
    c     previous               Previous maximum power
    c     done                   TRUE if relative minimum or maximum was found
    c     notfound               TRUE until sidelobe is found
    */

    side_lobe  find_sidelobe_1d (
        double cut[],
        int sign,
        int peakpos,
        double mwidth,
        double mainlobe_bound)

    {
        /*
        c
        c Search for the first relative minimum in the region immediately outside
        c the mainlobe region.
        c
        */
        boolean done = false;
        double current;
        int N = cut.length;

        int i = peakpos + (int)((mainlobe_bound - peakpos)*mwidth+.5);
        if (i > N-1)
            i = N-1;
        else if (i < 0)
            i = 0;

        DEBUG.logger("I = " + i );

        double previous = cut[i];

        while (((i < N) && (i > 0)) && (!done))
        {
            i = i + sign;
            current = cut[i];
            done = (current > previous);
            previous = current;
        }


        DEBUG.logger("In find_sidelobe_1d:  peak position:"+ peakpos);
        DEBUG.logger("In find_sidelobe_1d:  rel min: "+ (i-sign )+"  val:"+cut[i-sign]+"  done:"+ done );

        /*
        c
        c If a relative minimum has been found, find the first relative maximum
        c past this minimum in the same direction.  The maximum is the first
        c sidelobe.
        c
        */
        boolean notfound = false;
        if (done)
        {
            done = false;
            i = i + sign;
            while (((i < N) && (i > 0)) && (!done))
            {
                current = cut[i];
                if (current < previous)
                    done = true;
                else
                {
                    previous = current;
                    i = i + sign;
                }
            }
            notfound = (!done);
        }
        else
            notfound = true;
        int maxpos = i - sign;
        boolean fnd = notfound;

        DEBUG.logger("In find_sidelobe_1d:  sidelobe:"+ maxpos+"  val:"+ cut[maxpos] + " done:"+ done);

        return new side_lobe ( maxpos, fnd);

    }



    /*

    get_clutter  -> Calculate the average clutter power around a point target.
    c
    c Calculate the average clutter power around a point target.
    c
    c External Routines
    c     integrate_2d           Integrate the power over a two-dimensional area
    c     valid_window           TRUE if the window can be included in the clutter
    c                                power calculation
    c
    c Inputs
    c     N                      Dimension of (square) image
    c     image                  Image data
    c     peak_samp,
    c     peak_line              Location of mainlobe peak in image
    c     azimuth_res            Azimuth resolution, in pixels
    c     range_res              Range resolution, in pixels
    c
    c Outputs
    c     box                    Dimension of the area from which corner windows
    c                                will be taken; in units of resolution cells
    c     clutter_power          Average clutter power
    c     n_clutter              Number of resolution cells used in clutter power
    c                                calculation
    c
    c Variables
    c     max_tl, max_tr,        Maximum value of the power in each clutter window
    c     max_bl, max_br             (top left, top right, bottom left, bottom
    c                                right)
    c     sum                    Integral of clutter power over current window
    c     sum_tl, sum_tr,        Sum of the power in each clutter window (top
    c     sum_bl, sum_br             left, top right, bottom left, bottom right)
    c     count                  Number of pixels contained within each clutter
    c                                window
    c     NA, NR                 Azimuth, range dimensions of the area from which
    c                                corner windows will be taken; in units of
    c                                pixels
    c     n_valid                Number of windows to include in calculation
    c     window_size            Size of (square) corner windows over which clutter
    c                                power is calculated; in units of resolution
    c                                cells
    c
    c
    c     +---------------+     -       (Not to scale)
    c     |X|   |   |   |X|     ^        O is the point target.
    c     |--   |   |   --|     |        Each clutter window is marked with an X.
    c     |     |   |     |     |        w = window_size
    c     |-----+---+-----|     |        b = box
    c     |     | O |     |     b
    c     |-----+---+-----|     |
    c     |     |   |     |     |
    c     |--   |   |   --|  -  |
    c     |X|   |   |   |X|  w  v
    c     +---------------+  -  -
    c
     */

    private clutter get_clutter ( resolution az_res, resolution rng_res, location max )
    {
        /*
        c Integrate the power over each corner of the area around the target.
         */

        double sum = 0.0;
        int n_valid = 0;
        int count;
        int window_size = 7;
        int box = 20;
        int NA = (int)(box * az_res.res);
        int NR = (int)(box * rng_res.res);
        int n_clutter =0;
        double clutter_power = 0;

	DEBUG.logger("get_clutter -> center at (" + max.x + "," + max.y+")");
	DEBUG.logger("get_clutter -> NR " + NR);
	DEBUG.logger("get_clutter -> NA " + NA);
	DEBUG.logger("get_clutter -> az_res.res = " + az_res.res);
	DEBUG.logger("get_clutter -> rng_res.res = " + rng_res.res);
	
        int_results tl = integrate_2d( oversampled_data.I, oversampled_data.Q,
                                       max.x-NR/2,
                                       max.y-NA/2,
                                       window_size*az_res.res,
                                       window_size*rng_res.res);

        int_results bl = integrate_2d( oversampled_data.I, oversampled_data.Q,
                                       max.x-NR/2,
                                       (int)(max.y+NA/2-window_size*az_res.res),
                                       window_size*az_res.res,
                                       window_size*rng_res.res);

        int_results tr = integrate_2d( oversampled_data.I, oversampled_data.Q,
                                       (int)(max.x+NR/2-window_size*rng_res.res),
                                       max.y-NA/2,
                                       window_size*az_res.res,
                                       window_size*rng_res.res);

        int_results br = integrate_2d( oversampled_data.I, oversampled_data.Q,
                                       (int)(max.x+NR/2-window_size*rng_res.res),
                                       (int)(max.y+NA/2-window_size*az_res.res),
                                       window_size*az_res.res,
                                       window_size*rng_res.res);
        /*
        c Check that each window can be included in the clutter power calculation.
        c The validity check tries to exclude windows that may contain other targets.
        c If a window is valid, update the running power sum.
         */
        count = (int)(window_size*az_res.res)*(int)(window_size*rng_res.res);
        n_valid = 0;
        if (valid_window(tl.maxval, tl.sum/count))
        {
            sum = sum + tl.sum;
            n_valid = n_valid + 1;
        }
        if (valid_window(bl.maxval, bl.sum/count))
        {
            sum = sum + bl.sum;
            n_valid = n_valid + 1;
        }
        if (valid_window(tr.maxval, tr.sum/count))
        {
            sum = sum + tr.sum;
            n_valid = n_valid + 1;
        }
        if (valid_window(br.maxval, br.sum/count))
        {
            sum = sum + br.sum;
            n_valid = n_valid + 1;
        }

        /*
        c Use the valid windows to calculate the clutter power.  If no valid windows
        c were found, assign null values to the clutter power and the clutter cell
        c count.
         */
        if (n_valid > 0)
        {
            n_clutter = window_size * window_size * n_valid;
            clutter_power = sum / n_clutter;
        }
        else
        {
            DEBUG.error("No valid clutter windows where found.");
            n_clutter = 0;
            clutter_power = 0.0;
	    return new clutter ( false );
        }

        return new clutter ( clutter_power, n_clutter);
    }


    boolean valid_window(double maxval, double mean)
    {
        return (maxval < 8*mean);
    }

    /*
    c subroutine integrate_2d
     */
    int_results integrate_2d(
        double vi[][],
        double vq[][],
        int samp_tl,
        int line_tl,
        double azimuth_len,
        double range_len)
    {
        double sum=0.0, maxval, power;
        int modr;
        int i, j;

        maxval = vi[samp_tl][line_tl]*vi[samp_tl][line_tl] +vq[samp_tl][line_tl]*vq[samp_tl][line_tl];

        //Rount up -- apparently fortran does this durring conversion..
        int end_x = samp_tl+(int)(range_len+.5);
        int end_y =  line_tl+(int)(azimuth_len+.5);

        for (i = samp_tl; i < end_x; i++)
            for(j = line_tl; j < end_y; j++)
            {
                int I,J;
                power = vi[i][j]*vi[i][j]+vq[i][j]*vq[i][j];
                sum += power;
                //DEBUG.logger("power at " +i+"/"+j+"->"+power);
                if (power > maxval)
                    maxval = power;
            }

        return new int_results ( sum, maxval);
    }
    /*
    find_1d_res -> computes the resolution of the main lobe.

    Notes:
     		added one pixel re SM's instructions 2/8/90
    */

    resolution find_1d_res( lobe mlobe, double ncells,double cut[], int max, double clutter_power )
    {
		//Verify that the lobe is good 
	if ( mlobe.valid == false )	
		return new resolution ( false );

        double error_resolution = -1.0;

        double npixels = (double)(mlobe.pos - mlobe.neg ) /
                         (double)(oversampled_data.oversampled_size/oversampled_data.size);

        double res = npixels * ncells;
        double peak_power = cut[max]*cut[max] - clutter_power;

        double error_estimate = 100.0 * uncert_res(peak_power, clutter_power, ncells, res);

        if (error_estimate > 0.0)
            error_resolution = (error_estimate/100.0) * res;
        else
	  {
            DEBUG.error("ERROR:Resolution could not be computed.");
	    return new resolution ( false );
	  }
	


        DEBUG.logger("find_1d_res = " + res + " w/err or "+ error_resolution);

        return new resolution ( res, error_resolution);
    }

    /*
    uncert_res -> This function returns the uncertainty in the measurement of the 1-D
    		  resolution.
    c
    c Inputs
    c     mainlobe_power    (Total) mainlobe peak power
    c     clutter_power     Average clutter power
    c     spacing           Given pixel spacing (m)
    c     resolution        Calculated resolution width (m)
    c
    c Reference
    c     S. N. Madsen, "Specification of the Single Point Target Analysis
    c         Module, SIR-C Calibration Processor," JPL IOM 3348-90-40,
    c         March 16, 1990.
     */

    double uncert_res( 	double mainlobe_power,
                        double clutter_power,
                        double spacing,
                        double resolution)
    {

        /*
        c Check that the argument to the sqrt() function is positive.  If it is
        c not, assign an illegal (negative) value to the uncertainty.
         */
        int OVERSAMP_FAC = (oversampled_data.oversampled_size/oversampled_data.size);

        if (mainlobe_power > clutter_power)
            return Math.sqrt(
                       clutter_power/(mainlobe_power - clutter_power) +
                       ( (spacing/(resolution*OVERSAMP_FAC))* (spacing/(resolution*OVERSAMP_FAC)) ) / 6.0 );

        return -1.0;
    }

    /*
     	find_mainlobe_1d ->  Find the bounds of the mainlobe within the 1-dimensional impulse response.
    c
    c Algorithm
    c     Search away from the peak in each direction to find the point at which
    c     the power drops below the cutoff value determined from the input
    c     criterion.  Return an error if either the cutoff power or either edge
    c     could not be found.
    c
    c Inputs
    c     N                      Length of impulse response
    c     cut                    1-dimensional impulse response
    c     peak                   Location of mainlobe peak
    c     crit                   Criterion for determining mainlobe bounds (dB)
    c     clutter_power          Clutter power
    c     direction              AZIMUTH or RANGE
    c
    c Outputs
    c     mainlobes              Mainlobe bounds; first dimension indicates
    c                                the direction of the cut and second dimension
    c                                indicates the direction away from the peak
    c                                (1 = left or down, 2 = right or up)
    c     error                  Error flag
    c
    c Variables
    c     cutoff                 Power above which a point is still considered
    c                                part of the mainlobe
    c     index                  Indexing variable
    c     found_pos,             TRUE if the edge of the mainlobe was found in
    c     found_neg                  the positive or negative direction,
    c                                respectively
    c
    c Error return values
    c     0                      No error
    c     1                      Clutter power greater than mainlobe peak power
    c     2                      Power never dropped below cutoff value
    c
     */

    lobe find_mainlobe_1d( boolean rng_or_az, double clutter_power, double crit )
    {
        double cutoff= -1;
        int peak =0;
        double cut[];
        if ( rng_or_az )		//Range
        {
	    DEBUG.logger("find_mainlobe_1d -> Range."); 
            cut = range;
            peak = peak_range;
	    
        }
        else
        {
	    DEBUG.logger("find_mainlobe_1d -> Azimuth.");
            cut = azmuth;
            peak = peak_azmuth;
        }

        /*
        c
        c Check that the argument of the sqrt() function is positive.  If not,
        c set the error flag and return.
        c
         */

	clutter_power = 0;

        if (cut[peak]*cut[peak] > clutter_power)
        {
            //cutoff = sqrt((cut(peak)**2.0 - clutter_power) *  10**(-crit/10.0))
            cutoff = Math.sqrt((cut[peak]*cut[peak] - clutter_power) *  Math.pow(10,(0-crit/10.0)));
	    //cutoff = Math.sqrt((cut[peak]*cut[peak] - clutter_power) *  Math.pow(10,(0-crit/10.0)));
	    //cutoff = (cut[peak]*cut[peak] - clutter_power) * .5;
        }
        else
        {
            DEBUG.error("ERROR:\tCould not find mainlobe.");
	    return new lobe(false);
        }


        DEBUG.logger(" find_mainlobe_1d -> cutoff => " + cutoff);
        DEBUG.logger(" Peak => " + cut[peak]*cut[peak]);
        DEBUG.logger(" crit => " + crit);

        // Search from the peak in the positive direction.

        boolean found_pos = false;
        int index = peak;


        while (index < size && (!found_pos) )
        {
            if ((cut[index]) >= cutoff)
                index = index + 1;
            else
                found_pos = true;
	 
        }
        int mainlobe_positive = index - 1;


	DEBUG.logger("Mainlobe_positive is at " + mainlobe_positive);

        // Search from the peak in the negative direction.
        boolean found_neg = false;
        index = peak;

        while (index > 0 && (!found_neg))
        {
            if (cut[index] >= cutoff)
                index = index - 1;
            else
                found_neg = true;
        }

        int mainlobe_negitive = index + 1;
	

	DEBUG.logger("Mainlobe_negitive is at " + mainlobe_negitive);


        if ( !found_neg || !found_pos )
        {
            DEBUG.error("ERROR:\tCould not find mainlobe.");
            if (!found_neg)
                DEBUG.error("ERROR:\tCould not find neg lobe.");
            if (!found_neg)
                DEBUG.error("ERROR:\tCould not find pos lobe.");
            DEBUG.error("ERROR:\tcutoff is " + cutoff);
	    return new lobe (false);
        }

        return new lobe(mainlobe_positive, mainlobe_negitive);
    }

    /*
    pslr_1d ->  Calculate the one-dimensional PSLR and an uncertainty estimate for the
     PSLR.
    c
    c The PSLR is defined as
    c
    c     PSLR = 10 * log10 ( Peak sidelobe power / Peak mainlobe power )
    c
    c where the peak sidelobe power is the larger of the two sidelobes nearest
    c the mainlobe in the one-dimensional cut.
    c
    c One change has been made from S. Madsen's algorithm:  the clutter power
    c is no longer subtracted from the peak mainlobe and sidelobe powers,
    c according to A. Freeman's instructions.  (See reference below.)
    c
    c External functions
    c     uncert_pslr            Calculate an uncertainty estimate for the PSLR
    c
    c Inputs
    c     cut                    One-dimensional impulse response
    c     cut_maximum            Location of mainlobe peak within one-dimensional
    c                                impulse response
    c     sidelobe_1             Location of one first sidelobe within one-
    c                                dimensional impulse response
    c     sidelobe_2             Location of other first sidelobe within one-
    c                                dimensional impulse response
    c     clutter_power          Clutter power
    c     N                      Number of samples in impulse response
    c     id                     Identifier ("azimuth" or "range") for debugging
    c
    c Outputs
    c     pslr_db                PSLR (dB)
    c     err_pslr_db            Uncertainty estimate for PSLR (dB)
    c
    c Variables
    c     amplitude              Amplitude of sidelobe peak used in PSLR
    c     error_estimate         Uncertainty estimate for PSLR (%)
    c     peak_power             Mainlobe peak power
    c     side_power             Power of sidelobe peak used in PSLR
    c     larger_lobe            Location of the larger first sidelobe within
    c                                one-dimensional impulse response
    c
    c Reference
    c     S. N. Madsen, "Specification of the Single Point Target Analysis
    c         Module, SIR-C Calibration Processor," JPL IOM 3348-90-40,
    c         March 16, 1990.
    c
    c       History:
    c               03/18/98- Mods for SCANSAR DN**2 values
    c               05/18/98- Removed Mods for SCANSAR DN**2 values
    */

    pslr pslr_1d(	double cut[],
                  int cut_maximum,
                  side_lobe neg,
                  side_lobe pos ,
                  double clutter_power
                )
    {
        double amplitude;
        double error_estimate;
        double peak_power, side_power;
        double err_pslr_db = -1.0;
        double pslr_db = -1.0;


	if ( neg.valid == false || pos.valid == false )
	  	return new pslr (false);
	
        /*
        c Select the larger of the two sidelobes.
        */

        int sidelobe_1 = neg.pos;
        int sidelobe_2 = pos.pos;
        int larger_lobe = sidelobe_2;
        if (cut[sidelobe_1] > cut[sidelobe_2])
            larger_lobe = sidelobe_1;

        /*
        c Calculate the peak power and the amplitude and power of the larger
        c sidelobe.
        */

        amplitude = cut[larger_lobe];

        peak_power = cut[cut_maximum] * cut[cut_maximum];
        side_power = amplitude*amplitude;

        /*
        c Calculate the PSLR and the uncertainty estimate.  The clutter power is
        c no longer subtracted from the mainlobe and sidelobe signal powers,
        c according to A. Freeman's instructions of 12/3/93.
        */

        boolean lobe_good = true;

        if (side_power != 0.0)
            pslr_db = 10.0* Math.log(side_power / peak_power )/ Math.log(10);
        else
            lobe_good = false;

        error_estimate = uncert_pslr(side_power, clutter_power) * 100.0;

        if (error_estimate > 0.0)
            err_pslr_db = 10*Math.log(1.0 + error_estimate / 100.0)/Math.log(10);
        else
            lobe_good = false;

        return new pslr(pslr_db, err_pslr_db, lobe_good);
    }


    /*
    uncert_pslr -> Returns the uncertainty in the measurement of the PSLR.
    c
    c Inputs
    c     sidelobe_power    (Total) sidelobe peak power
    c     clutter_power     Average clutter power
    c
    c     n_looks           Number of looks (assumed equal to 1)
    c
    c Reference
    c     S. N. Madsen, "Specification of the Single Point Target Analysis
    c         Module, SIR-C Calibration Processor," JPL IOM 3348-90-40,
    c         March 16, 1990.
    c*/

    double uncert_pslr ( double sidelobe_power, double clutter_power  )
    {
        double n_looks = 1;

        if (clutter_power > 0.0 && sidelobe_power > clutter_power)
            return   (Math.sqrt(2.0 * clutter_power /
                                (n_looks * (sidelobe_power - clutter_power))) );
        else
            return -1.0;
    }

    /*
     uncert_islr ->  Returns the uncertainty in the measurement of the ISLR.
    c
    c Inputs
    c     clutter_power     Average clutter power
    c     n_cells           Number of resolution cells in sidelobe region
    c     n_clutter         Number of resolution cells used to calculate
    c                           clutter power
    c     mainlobe_energy   Integral of power in mainlobe region
    c     sidelobe_energy   Integral of power in sidelobe region
    c     n_corr            Resolution width in pixels
    c
    c Variables
    c     tmp1 - tmp6       Temporary variables used to simplify expressions
    c
    c Reference
    c     S. N. Madsen, "Specification of the Single Point Target Analysis
    c         Module, SIR-C Calibration Processor," JPL IOM 3348-90-40,
    c         March 16, 1990.
    c
    c History:
    c       03/18/98 - Mods for Scansar DN**2 values
    */
    double uncert_islr(
        double clutter_power,
        int n_cells,
        int n_clutter,
        double mainlobe_energy,
        double sidelobe_energy,
        double n_corr)
    {

        double tmp1, tmp2, tmp3, tmp4, tmp5, tmp6;

        /*
        c Check that the calculations will not result in divide-by-zero or domain
        c errors.  If the input values are OK, calculate the uncertainty; otherwise,
        c set the uncertainty to an illegal (negative) value.
        */

        if (n_clutter > 0 && clutter_power > 0.0)
        {
            tmp1 = n_cells*(n_corr*clutter_power)*(n_corr*clutter_power);
            tmp2 = ( (n_cells*clutter_power*n_corr)*(n_cells*clutter_power*n_corr) )
                   / n_clutter;
            tmp3 = 2*n_corr*clutter_power*sidelobe_energy;
            tmp4 = sidelobe_energy*sidelobe_energy;
            tmp5 = ( 2*clutter_power*n_corr ) / mainlobe_energy;
            tmp6 = (tmp1 + tmp2 + tmp3)/tmp4 + tmp5;
            if (tmp6 > 0.0)
                return Math.sqrt( (tmp1 + tmp2 + tmp3)/tmp4 + tmp5 );
            else
                return -1.0;
        }

        return  -1.0;
    }

  double db( double foo )
	{
		return ( 10.0 * Math.log(foo) / Math.log(10.0));
	}

}



class int_results {
    public double sum;
    public double maxval;

    int_results ( double s, double m )
    {
        sum = s;
        maxval = m;
        //DEBUG.logger("int_results -> sum = " + s + " maxval => " + m );
    }

}

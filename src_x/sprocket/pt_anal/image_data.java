//*
//*  image_data.java
//*	Contains the class image_data, which repersents image data from a SAR image
//*   	and some operations.
//*
//* Public Member Functions
//*
//* 	Oversample -> preforms the FFT oversampling
//*
//*	image_data_write -> write the image data out to the file "file_name"
//*
//*	oversampled_data_write -> write the oversampled date out to the file "file_name"
//*
//* Private Member Functions
//*
//*	compress -> converts the image data from a matrix format to a linear format where
//*	   the first item starts at 1.  This is done just so the fourn routine will work
//*	   without extensive modifications.
//*
//* 	uncompress_i/q -> converts from the linear format to the matrix format.
//*
//* 	fourn->  1- or 2-dimensional FFT/IFFT routine
//*
//*	xpose1d->  Transpose a complex square matrix.
//*
//*	findmax_cmpx2d_win -> finds the largest value in the subseted data.
//*
//*	rotate -> Rotate the elements in an array along the first
//*              dimension so that the specified bin is moved to bin 1.
//*
//*	copy_corners -> Copy each corner of a square array into the corresponding corner
//*		of a larger (square) array.  Both arrays are assumed to be complex.
//*
//*	baseband -> This subroutine shifts a complex 2-D spectrum to baseband along its
//*		first dimension.  The array is assumed to be square.

import java.util.* ;
import java.awt.* ;
import java.io.* ;
import java.lang.*;

public class image_data
{
    double I[][];
    double Q[][];
    public double OI[][];
    public double OQ[][];
    public int size;
    public int oversampled_size;
    int x_size;
    int y_size;
    int peak_x;
    int peak_y;
    Metadata M;
    double sample;

    image_data ( double i[][], double q[][], Metadata m, double smp)
    {
        I = i;
        Q = q;
        x_size = I.length;
        y_size = I[0].length;
        size = x_size;
	M = m;
	sample = smp;
    }

    /*
      compress -> converts a i/q matrix format to a linear format
       	where i follows q and the first element starts at 1 .

        ( This is just used to feed data into the fourn function
          below ) 

            */
    private double [] compress ( double ii[][], double qq[][], int size)
    {
        double ret[] = new double[size*size*2+1];

        for ( int i = 0; i < size; i++)
            for ( int j = 0; j < size; j++)
            {
                ret[(i*size+j)*2+1] = qq[i][j];
                ret[(i*size+j)*2+2] = ii[i][j];
            }
        return ret;
    }

    /*
         uncompress_q -> undo's a compress (see above) and 
    	returns the Q values.

    */

    private double [][] uncompress_q (double s[], int size)
    {
        double ret [][] = new double [size][size];
        for ( int i = 0; i < size; i++)
            for ( int j = 0; j < size; j++)
                ret [i][j]  = s[(i*size+j)*2+1];
        return ret;
    }

    /*
         uncompress_i -> undo's a compress (see above) and 
            returns the I values.

    */

    private double [][] uncompress_i (double s[], int size)
    {
        double ret [][] = new double [size][size];
        for ( int i = 0; i < size; i++)
            for ( int j = 0; j < size; j++)
                ret [i][j]  = s[(i*size+j)*2+2];
        return ret;
    }

    /*
    	Oversample -> preforms the FFT oversampling
    		1:   FFT
    		2:   Baseband
    		3:   copy the data into the corners of a larger array
    		4:   IFFT the larger array
    */

    void oversample( int azimuth_winsize, int range_winsize, int factor)
    {

        OI = new double[x_size*factor][y_size*factor];
        OQ = new double[x_size*factor][y_size*factor];
        double oi[][] ;
        double oq[][] ;

        //Moved into blocks so small array could go out of scope
        {

	    DEBUG.pre_progress("FFT");
            double small[] = compress ( I, Q, x_size);
            fourn (small, x_size, 2, -1, x_size );
            oi = uncompress_i( small, x_size);
            oq = uncompress_q( small, x_size);
	    DEBUG.post_progress();
        }


        baseband(oi, oq );

        copy_corners ( oi,oq,OI,OQ, x_size, x_size*factor);


        //Moved into blocks to large array could go out of scope quicker
        {
	    DEBUG.pre_progress("IFFT");
            double large[] = compress (OI, OQ,  x_size*factor);
            fourn (large, x_size*factor, 2, 1, x_size);
            OI = uncompress_i(large, x_size*factor);
            OQ = uncompress_q(large, x_size*factor);
	    DEBUG.post_progress();
        }

        oversampled_size = factor * size;
        find_peak();
    }


    double cabs( double i, double q)
	{
		return ( i*i + q*q);
	}

   	/*************************************
	*  baseband --
	*	"Basebands" spectra.
	*	This is done by moving the cols with the largest values and the 
	*	rotating them to [0] and then rotating the largest rows to [0].
	*	This stratigy is not the best - should instead use the "windowing" techique.
	*
	**************************************/

    void baseband ( double i[][], double q[][])
      	{
	  
	   double msum = -1; 
  	   double sum;
	   int bin = 0;	
	   int len = i.length;
         double tq[][] = new double[len][len];
         double ti[][] = new double[len][len];

	 boolean doppler_basebanding = true;
 	 boolean range_basebanding = false; 



        if (range_basebanding)
         {
		//Find biggest bin in X
	   for ( int a = 0; a < len; a++)
	      {
		sum = 0;		
		for ( int b = 0; b < len; b++)
			sum += cabs(i[a][b], q[a][b] );
		if (sum > msum)
		   {
			msum = sum;
			bin = a;
		   }
	      }

	  DEBUG.logger("Baseband -> max at " + bin);

		      //Find biggest bin in X
           	for ( int a = 0; a < bin; a++)
		   {
                  	for ( int b = 0; b < len; b++)
				{
				   tq[len-bin+a][b] = q[a][b];
				   ti[len-bin+a][b] = i[a][b];
				}
		   }

		for ( int a = 0; a < len-bin; a++)
		   {
			for ( int b = 0; b < len; b++)
                                {
                                   tq[a][b] = q[a+bin][b];
                                   ti[a][b] = i[a+bin][b];
                                }
		   }

	  }
	if ( !doppler_basebanding) 
	  {
	   msum = -1;
	   bin = 0;
	                //Find biggest bin in Y 
           for ( int b = 0; b < len; b++)
              {
                sum = 0;
                for ( int a = 0; a < len; a++)
                        sum += cabs(i[a][b], q[a][b] );
                if (sum > msum)
                   {
                        msum = sum;
                        bin = b;
                   }
              }
	   }
	 else
	  {
	    double prf = M.obtain_double("PRF");
	    double ref_first_d = M.obtain_double("range_reference"); 
	    double a0 = M.obtain_double("doppler_poly_a0");
	    double a1 = M.obtain_double("doppler_poly_a1");
	    double a2 = M.obtain_double("doppler_poly_a2");
	    int pixel = (int)(sample);
	
	    if ( M.asending_or_desending.equals("DESCENDING") )
		{
		  pixel = (int)(M.number_of_pixels) - pixel;
		}

	    double doppler_value = a0 + a1*pixel + a2*pixel*pixel; 
	    double fract_part =  doppler_value/prf - (double)((int)(doppler_value/prf));
	    bin = round(((double)len) * fract_part );

	    DEBUG.logger("Fract part = " + fract_part);
	    DEBUG.logger("Len = " + len);
	    DEBUG.logger("Sample="+pixel);
	    DEBUG.logger("doppler_value = " + doppler_value);
	    DEBUG.logger("doppler_value/prf = " + doppler_value/prf );
	    DEBUG.logger("Int part = " + round(doppler_value/prf) );
	    

	    if (bin < 0 )
		bin = 0 - bin;
	  }

          DEBUG.logger("Baseband Y -> max at " + bin);

                //Shift to bin 0
                for ( int b = 0; b < bin; b++)
                        for ( int a = 0; a < len; a++)
                                {
                                   //q[a][len-bin+b] = tq[a][b];
                                   //i[a][len-bin+b] = ti[a][b];
				   tq[a][len-bin+b] = q[a][b];
				   ti[a][len-bin+b] = i[a][b];
                                }

                for ( int b = 0; b < len-bin; b++)
                        for ( int a = 0; a < len; a++)
                                {
                                   //q[a][b] = tq[a][b+bin];
                                   //i[a][b] = ti[a][b+bin];
				   tq[a][b] = q[a][b+bin];
                                   ti[a][b] = i[a][b+bin];	
                                }

	for ( int b = 0; b < len; b++)
	  for ( int a = 0; a < len; a++)
		{
			q[b][a] = tq[b][a];
			i[b][a] = ti[b][a];
		}

	}

    /*
    	image_data_write -> write the image data out to the file "file_name".
    */

    void image_data_write ( String file_name )
    {
        data_write ( I, Q, size, file_name, false );
    }

    /*
    db -> convert in to db.
    */

    private double db ( double in )
    {
        return 10.0*Math.log(in) / Math.log(10);
    }


    /*
    	write out the image data one/two to the file "file_name".
    */ 
    private void data_write ( double [][]one, double [][]two, int sz, String file_name, boolean power )
    {

        double delta = 0;
        double max, min;
        double max_value = 0;
        int fiddle = 512;

        if ( power )
        {
            min = max  = one[0][0] * one[0][0] + two[0][0] * two[0][0];
            for ( int x= 0; x<sz; x++)
                for ( int y = 0; y <sz; y++)
                {
                    double dn = ( one[x][y] * one[x][y] + two[x][y] * two[x][y]);
                    if ( dn > max )
                        max = dn;
                    else
                        if ( dn < min )
                            min = dn;

                }

            max = db(max);
            min = db(min) - max;

            /* Chop off at -40 ---> used to make plot
               more understandable. */
            if ( min < -40)
                min = -40;
            delta = 256.0 /(min) ;

            fiddle = 256;

        }
        else
        {
            double sum = 0, sum2 = 0;
            for ( int x= 0; x<sz; x++)
                for ( int y = 0; y <sz; y++)
                {
                    double dn = ( one[x][y] * one[x][y] + two[x][y] * two[x][y]);
                    sum += dn;
                    sum2 += dn*dn;
                }
            double ave = sum / (sz*sz);
            double sddev = Math.sqrt ((sum2 - (sum * sum / (sz*sz))) / (sz*sz - 1.0));
            max =ave+ 2 * sddev;
            min = ave- 2  * sddev;
            if ( min < 0)
                min = 0;
            delta = -256.0 / (max - min);
            fiddle = 0;

        }
        try {

            DataOutputStream out = new DataOutputStream( new FileOutputStream ( file_name ) );
            out.writeBytes("P5\n" +sz+" " + sz +"\n 255\n");

            for ( int x= 0; x<sz; x++)
                for ( int y = 0; y<sz; y++)
                {
                    double dn = (one[x][y] * one[x][y] + two[x][y] * two[x][y] - max_value);
                    if (power)
                        dn = db(dn) - max;
                    int value = fiddle - (int)(delta*((dn)) + .5);

                    if ( value >255 )
                        value = 255;
                    else
                        if ( value < 0 )
                            value = 0;
                    out.writeByte ( value);
                }
        }
        catch (java.io.IOException e)
        {
            System.err.println("ERROR:\t -> \""+e+"\"");
            System.exit(-1);
        }
    }


    /*
    	oversampled_data_write -> write the oversampled data out to "file_name".
    */

    void oversampled_data_write ( String file_name )
    {
        data_write(OI, OQ, oversampled_size, file_name, true);
    }

    /*
    c fourn:  1- or 2-dimensional FFT/IFFT routine
    c
    c Arguments
    c
    c     data          - input and output array, assumed to be complex
    c     nfft          - length of one side of the array
    c     ndim          - dimension of FFT
    c     isign         - flag indicating FFT or IFFT:
    c                     isign = -1   <==>  FFT
    c                     isign =  1   <==>  IFFT
    c     scale         - factor by which to scale data, if needed
    c
    c Variables
    c     Many.
    c
    c
    c Note that the input array is assumed to be two-dimensional.
    c
    c
    c Modifications
    c
    c 1.  Restored the (I)FFT dimension as an argument to the routine.
    c
    c 2.  For the inverse FFT, the routine now divides the result by
    c     nfft**ndim immediately before returning.
    c
    c
    c Source
    c
    c C. Werner, who obtained this algorithm from _Numerical Recipes_,
    c by Press, Flannery, Teukolsky, and Vetterling.
    c
    c
    c J. Shimada
    c 12/8/89
    c
    	Java conversion notes:
    		Converted with no modifications to program structure - JC
     */
    private void fourn( double data[], int nfft, int ndim, int isign, int scale)
    {
        double wr, wi, wpr, wpi, wtemp, theta;
        double tempr, tempi, temp;
        int idim, n, nrem, ntot, nprev, ip1, ip2, ip3, i2rev;
        int i1, i2, i3, i3rev, ibit, ifp1, k1, k2, ifp2;

	int count = 0;


        ntot = nfft * nfft;
        nprev = 1;
        for( idim = 1; idim <= ndim; idim ++)
        {

            n = nfft;
            nrem = ntot / (n * nprev);
            ip1 = 2 * nprev;
            ip2 = ip1 * n;
            ip3 = ip2 * nrem;
            i2rev = 1;
            for (i2 = 1; i2 <= ip2; i2+=ip1)
            {
		DEBUG.progress(count++);
                if (i2 < i2rev)
                {
                    for ( i1 = i2;  i1 <= ( (i2 + ip1) - 2) ; i1+= 2)
                    {
                        for (i3 = i1; i3 <= ip3; i3 += ip2 )
                        {
                            i3rev = (i2rev + i3) - i2;
                            tempr = data[i3];
                            tempi = data[i3 + 1];
                            data[i3] = data[i3rev];
                            data[i3 + 1] = data[i3rev + 1];
                            data[i3rev] = tempr;
                            data[i3rev + 1] = tempi;
                        } //12
                    }  //13
                }
                ibit = ip2 / 2;
                while ( (ibit >= ip1) && (i2rev > ibit))
                {
                    i2rev = i2rev - ibit;
                    ibit = ibit / 2;
                }
                i2rev = i2rev + ibit;
            }	//14
            ifp1 = ip1;


            while (ifp1 < ip2)
            {
                ifp2 = 2 * ifp1;
                theta = (isign * 6.283185307179590) / (ifp2 / ip1);
                wpr = - (2.0 * Math.sin(0.50 * theta)*Math.sin(0.50 * theta));
                wpi = Math.sin(theta);
                wr = 1.0;
                wi = 0.0;

		DEBUG.progress(count++);
                for (i3 = 1; i3 <= ifp1; i3+=ip1)
                {
                    for (i1 = i3; i1 <= ((i3 + ip1) - 2); i1 += 2 )
                        for (i2 = i1; i2 <= ip3; i2 += ifp2 )
                        {
                            k1 = i2;
                            k2 = k1 + ifp1;
                            tempr = (wr*data[k2]) -
                                    (wi*data[k2 + 1]);
                            tempi =  ((wr*data[k2 + 1]) +
                                      (wi * data[k2]));
                            data[k2] = data[k1] - tempr;
                            data[k2 + 1] = data[k1 + 1] - tempi;
                            data[k1] = data[k1] + tempr;
                            data[k1 + 1] = data[k1 + 1] + tempi;
                        } //15
                    wtemp = wr;
                    wr = ((wr * wpr) - (wi * wpi)) + wr;
                    wi = ((wi * wpr) + (wtemp * wpi)) + wi;
			
                } //17
                ifp1 = ifp2;
            } //2
            nprev = n * nprev;

        }

        if (isign == 1)
        {
            temp = Math.pow(scale,ndim);
            ip3 = 2*nfft*nfft;
            for (i1 = 1; i1 <= ip3; i1++)
                data[i1] = data[i1]/temp;
        }

    }

    /*
     	xpose1d->  Transpose a complex square matrix.
    */
    void xpose1d(double im[][], double qm[][], int size)
    {
        double tmpi, tmpq;
        int i, j;

        for ( i = 0; i < size; i ++ )
            for ( j = 0; j < size; j++)
            {
                tmpi = im[j][i];
                tmpq = qm[j][i];
                im[j][i] = im[i][j];
                qm[j][i] = qm[i][j];

                im[i][j] = tmpi;
                qm[i][j] = tmpq;
            }
    }

    /*
    c This subroutine shifts a complex 2-D spectrum to baseband along its
    c first dimension.  The array is assumed to be square.
    c
    c External calls
    c     dump_dataset, get_avg_spectra, get_centroid, rotate
    c
    c Inputs
    c     subset            Array to be basebanded
    c     size              Dimension of array
    c     window_size       Window size used to determine centroid of spectrum,
    c                           as a function of array dimension
    c     id                Identifier used in output messages
    c
    c Output
    c     subset            Basebanded array
    c
    c Variables
    c     bin               Bin containing centroid
    c     error             Flag; nonzero if centroid could not be determined
    c     i, j              Indexing variables
    c     scratch           Scratch array used to store output for debugging
    c     spectra           Average of all spectra in the direction to be
    c                           basebanded
    c     window_len        Window length to use in calculating centroid
    c     window_sums       Sum of elements contained in window starting at a
    c                           given bin; used for debugging only
    c     filename          Debug output filename
    c
    c --------------------------------------------------------
     */
    private void baseband(double [][]im, double [][]qm, int size, double window_size)
    {
        double average[] = new double[size];

        // Zero out
        for ( int i = 0; i < size; i++)
            average[i] = 0;

        // Compute sum
        for (int i = 0; i < size; i++)
            for ( int j = 0; j < size; j++)
                average[i] = average[i] + im[i][j] * qm[i][j];

        for (int i = 0; i < size; i++)
            average[i] = average[i] / size;

        int peak = get_centroid(average, size, (int)(window_size*size));
        if ( peak != 0 )
            rotate(im,qm, size, peak);
    }

    /*
    findmax_cmpx2d_win -> finds the largest value in the subseted data.
    */    

    location findmax_cmpx2d_win(double array_i[][], double array_q[][])
    {
        int max_samp = -1, max_line =-1;
        double max_power = -1;
        double value;
        int i, j;

        for ( i = 0; i < array_i.length; i ++ )
            for ( j = 0; j < array_i.length; j++)
            {
                value = (array_i[i][j] * array_i[i][j] + array_q[i][j]*array_q[i][j] );
                if (value > max_power )
                {
                    max_power = value;
                    max_samp = i;
                    max_line = j;
                }

            }
        return new location ( max_samp, max_line);
    }

    /*
    	findmax_cmpx2d_win -> finds the largest value in the oversampled data.
    */

    location findmax_cmpx2d_win()
    {
        return findmax_cmpx2d_win(OI,OQ);
    }


    /*
    find_peak -> find the largest value in the oversampled data and
    update peak_x, peak_y.

    */

    private void find_peak ( )
    {

        /*
        c
        c Find the location of the maximum within the oversampled image.
        c Calculate the estimated sample and line numbers of the peak by
        c comparing the sample and line numbers read from the point target
        c input file with the sample and line numbers of the upper left
        c corner of the box actually extracted from the file.  For most
        c targets, the differences between the corresponding numbers should
        c equal half the length of the box (in each dimension), as the region
        c is supposed to be a square with the input sample and line at its
        c center.  If the target is near the edge of the image, however, the
        c box will have been defined so that the input location is as close
        c close to the box center as possible while keeping the box completely
        c within the image bounds.
        c
         */

        location peak = findmax_cmpx2d_win();
        peak_x = peak.x;
        peak_y = peak.y;

    }


    /*
      rotate -> Rotate the elements in an array along the first 
    	dimension so that the specified bin is moved to bin 1.

      Note:
    	Used in basebanding.
    	*/

    private void rotate  ( double im[][], double qm[][], int size, int peak )
    {
        double scratch_i[][] = new double [size][size];
        double scratch_q[][] = new double [size][size];

        int i = 0, j = 0;
        for ( i = peak; i < size; i++)
            for ( j = 0; j < size; j++);
        {
            scratch_i[i-peak][j] = im[i][j];
            scratch_q[i-peak][j] = qm[i][j];
        }

        for ( i = 0; i < peak; i++)
            for ( j = 0; j < size; j++);
        {
            scratch_i[i+size-peak][j] = im[i][j];
            scratch_q[i+size-peak][j] = qm[i][j];
        }

        for ( i = 0; i < peak; i++)
            for ( j = 0; j < size; j++);
        {
            im[i][j] = scratch_i[i][j];
            qm[i][j] = scratch_q[i][j];
        }
    }

    /*
     	copy_corners ->
    c Copy each corner of a square array into the corresponding corner
    c of a larger (square) array.  Both arrays are assumed to be complex.
    c
    c External calls
    c     None.
    c
    c Inputs
    c     small_set         Data to be copied
    c     small_dim         Dimension of small array
    c
    c Outputs
    c     large_set         Array with input data copied into corners
    c     large_dim         Dimension of large array
    c
    c Variables
    c     i, j              Indexing variables
    c
     */

    private void copy_corners ( double ii[][],
                                double qq[][],
                                double II[][],
                                double QQ[][],
                                int s,
                                int S)
    {
        int i, j;

        // Upper left corner
        for ( i = 0; i < s/2; i++)
            for ( j = 0; j < s/2; j++)
            {
                QQ[i][j] = qq[i][j];
                II[i][j] = ii[i][j];
            }

        for ( i = s/2; i < s; i++)
            for ( j = 0; j < s/2; j++)
            {
                QQ[S-s+i][j] = qq[i][j];
                II[S-s+i][j] = ii[i][j];
            }

        // Lower left corner
        for ( i = 0; i < s/2; i++)
            for ( j = s/2; j < s; j++)
            {
                QQ[i][S-s+j] = qq[i][j];
                II[i][S-s+j] = ii[i][j];
            }

        // Lower right corner
        for ( i = s/2; i < s; i++)
            for ( j = s/2; j < s; j++)
            {
                QQ[S-s+i][S-s+j] = qq[i][j];
                II[S-s+i][S-s+j] = ii[i][j];
            }
    }


    /* get_centroid -> returns "centroid"  of data */

    private int get_centroid(double []dataset, int size, int window_len)
    {

        int start_bin;
        double peak_sum, sum;
        double previous_sum;
        int peak_bin = 0;
        double window_sums[] = new double[size];


        peak_sum = 0.0;
        for( start_bin = 0; start_bin < window_len; start_bin ++)
            peak_sum = peak_sum + dataset[start_bin];

        window_sums[0] = peak_sum;
        previous_sum = peak_sum;
        for (start_bin=1; start_bin<size; start_bin ++ )
        {
            int right_edge = (start_bin + window_len - 1) % size -1;
            sum = previous_sum - dataset[start_bin-1] +
                  dataset[right_edge];
            if (sum > peak_sum)
            {
                peak_sum = sum;
                peak_bin = start_bin;
            }
            window_sums[start_bin] = sum;
            previous_sum = sum;
        }

        peak_bin =(peak_bin + window_len/2 - 1)%size + 1;
        return peak_bin;
    }

   int round ( double v )
    {
	if ( v < 0 )
		return (int)( v - .5);
	else
		return (int)( v + .5 );
    }
}

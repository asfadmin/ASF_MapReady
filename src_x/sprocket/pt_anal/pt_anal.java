//*  pt_anal
//*	Preforms the point target analysis for single look complex images.
//*
//*
//*  Public Member Functions
//*
//*	none.
//*
//*  Private Member Functions
//*	extract_data -> Extracts data from a complex image.
//*

import java.util.* ;
import java.awt.* ;
import java.io.* ;
import java.lang.*;

public class pt_anal {
    Metadata m;
    Vector targets;
    Vector data;
    Vector cuts;
    String base;

    /*
    	pt_anal -- Starts the analysis.	
    		trgs:		A vector of Targets
    		M:		A populated metadata object	
    		base_name:	root name of the image to analysis
    		bsz:		size of data to extract
		oversample_factor: size of fft oversample

    */

    public pt_anal ( Vector trgs, Metadata M, String base_name, int bsz, int oversample_factor)
    {
        m = M;
        targets = trgs;
        base = base_name;
        data = new Vector();
        cuts = new Vector();

        for ( int i = 0; i<trgs.size(); i++)
            data.add(extract_data((Target)(trgs.elementAt(i)),bsz, bsz ) );

        // Oversample
        for ( int i = 0; i<trgs.size(); i++)
	  {
	    DEBUG.status("Oversampling " + ((Target)trgs.elementAt(i)).cs_dev_id);
            ((image_data)data.elementAt(i)).oversample(0,0,oversample_factor);
	  }

        // Write out the image data
        for ( int i = 0; i<data.size(); i++)
            ((image_data)data.elementAt(i) ).image_data_write (
		base_name + ((Target)trgs.elementAt(i)).cs_dev_id  + "_image_data.pgm");

        // Write out the oversampled data
        for ( int i = 0; i<data.size(); i++)
            ((image_data)data.elementAt(i) ).oversampled_data_write (
	 	base_name + ((Target)trgs.elementAt(i)).cs_dev_id  + "_oversampled_data.pgm");

	try {
		PrintStream out = new PrintStream ( new FileOutputStream(base_name+".iq"));
		point_target_analysis_results.print_header(bsz, oversample_factor);
		point_target_analysis_results.print_header(out, bsz, oversample_factor);

        		// Perform the analysis
        	for ( int i = 0; i<data.size(); i++)
        	  {
            	     double THETA = Double.parseDouble(((Target)trgs.elementAt(i)).cs_post_tilt);
            	     data_cuts dc = new data_cuts ( (image_data)data.elementAt(i), m , THETA ) ;
		     dc.cuts_write(((Target)trgs.elementAt(i)).cs_dev_id );
	    	     try {
            			point_target_analysis_results pt = dc.get_1d_meas(3.0);
		
		 		 	//Print results
				pt.print(((Target)trgs.elementAt(i)).cs_dev_id);
				pt.print(out, ((Target)trgs.elementAt(i)).cs_dev_id);

	      		  }
	   	     catch ( java.lang.ArrayIndexOutOfBoundsException e)
	     		{
				DEBUG.error(" 	--- Could not perform analysis of target " + ((Target)trgs.elementAt(i)).cs_dev_id + "---");	
	     		}
	   	  }
	  }
	catch ( IOException e)
	  {

	  }
    }

    /* extract_data -> Extracts data from a complex image.  */
    private image_data extract_data ( Target t, int x, int y )
    {
        double I[][];
        double Q[][];

        int start_x = (int) (t.actual_image_x - x/2 );
        int start_y = (int) (t.actual_image_y - y/2 );

        DEBUG.logger("Working on \"" + t.cs_dev_id + "\"");

        // Read Data

        I = new double [x][y];
        Q = new double [x][y];

        // Read I
        try {
            //RandomAccessFile in = new RandomAccessFile (
            DataInputStream in = new DataInputStream ( new FileInputStream(
                                     base + Constants.I_data_file));

            in.skipBytes((int)(m.number_of_pixels)*4*start_y+start_x*4);

            for ( int i = 0; i < y; i++)
            {

                for ( int ii = 0; ii < x; ii++)
                    I[ii][i] =in.readInt();
                in.skipBytes(((int)(m.number_of_pixels) - x)*4);
            }
        }
        catch  ( java.io.FileNotFoundException e)
        {
            System.err.println("ERROR:\textract_data -> could not open \""
                               +base + Constants.I_data_file + ":" + e+"\"");
            System.exit(-1);
        }
        catch (java.io.IOException e)
        {
            System.err.println("ERROR:\textract_data -> \""+e+"\"");
            System.exit(-1);
        }

        // Read Q
        try {
            DataInputStream in = new DataInputStream ( new FileInputStream(
                                     base + Constants.Q_data_file));

            in.skipBytes((int)(m.number_of_pixels)*4*start_y+start_x*4);
            for ( int i = 0; i < y; i++)
            {
                for ( int ii = 0; ii < x; ii++)
                    Q[ii][i] =in.readInt();
                in.skipBytes(((int)(m.number_of_pixels) - x)*4);
            }
        }
        catch  ( java.io.FileNotFoundException e)
        {
            System.err.println("ERROR:\textract_data -> could not open \""
                               +base + Constants.I_data_file + ":" + e+"\"");
            System.exit(-1);
        }
        catch (java.io.IOException e)
        {
            System.err.println("ERROR:\textract_data -> \""+e+"\"");
            System.exit(-1);
        }

	//find peak
	double max = -1;
	int max_x = 0;
	int max_y = 0;

	for ( int i = 0; i < x; i ++ )
	  for ( int ii = 0; ii<x; ii++)
	 	{	
		  double val = (I[ii][i] * I[ii][i] + Q[ii][i]*Q[ii][i] );
		  if ( max < val)
		     {
			max = val;
			max_x = ii;
			max_y = i;
		     }
		}

	if ( max >= (256.0*256.0*256.0*256.0 -3.0 ) )
	  	DEBUG.error("Target \'"+t.cs_dev_id+"\'  is saturated at \"" + max + "\".");


	DEBUG.logger("Max I is " + I[max_x][max_y]);
	DEBUG.logger("Max Q is " + Q[max_x][max_y]);

	start_x -= x/2 - max_x;
	start_y -= y/2 - max_y;


	// Re-read the data to ensure we are centered.

		 // Read I
        try {
            //RandomAccessFile in = new RandomAccessFile (
            DataInputStream in = new DataInputStream ( new FileInputStream(
                                     base + Constants.I_data_file));

            in.skipBytes((int)(m.number_of_pixels)*4*start_y+start_x*4);

            for ( int i = 0; i < y; i++)
            {

                for ( int ii = 0; ii < x; ii++)
                    I[ii][i] =in.readInt();
                in.skipBytes(((int)(m.number_of_pixels) - x)*4);
            }
        }
        catch  ( java.io.FileNotFoundException e)
        {
            System.err.println("ERROR:\textract_data -> could not open \""
                               +base + Constants.I_data_file + ":" + e+"\"");
            System.exit(-1);
        }
        catch (java.io.IOException e)
        {
            System.err.println("ERROR:\textract_data -> \""+e+"\"");
            System.exit(-1);
        }

	        // Read Q
        try {
            DataInputStream in = new DataInputStream ( new FileInputStream(
                                     base + Constants.Q_data_file));

            in.skipBytes((int)(m.number_of_pixels)*4*start_y+start_x*4);
            for ( int i = 0; i < y; i++)
            {
                for ( int ii = 0; ii < x; ii++)
                    Q[ii][i] =in.readInt();
                in.skipBytes(((int)(m.number_of_pixels) - x)*4);
            }
        }
        catch  ( java.io.FileNotFoundException e)
        {
            System.err.println("ERROR:\textract_data -> could not open \""
                               +base + Constants.I_data_file + ":" + e+"\"");
            System.exit(-1);
        }
        catch (java.io.IOException e)
        {
            System.err.println("ERROR:\textract_data -> \""+e+"\"");
            System.exit(-1);
        }




        return new image_data (I, Q, m, t.actual_image_x);
    }

    public static void main ( String [] args)
    {

        if (args.length != 3 )
        {
            System.err.println("Ussage Locator <block_size> <oversample_size> <basename>\n");
            return;         //Whats the return type of main in java anyway?
        }

	int sz = Integer.parseInt(args[0]);
	int ov = Integer.parseInt(args[1]);

        InputStream in;

        try {
            // Read the metadata
            Metadata m = new Metadata(args[2]);

            // Open the targets
            in = new FileInputStream(
                     new File (args[2] + Constants.ground_target_file_located));

            //Read targets
            TargetIngester t = new TargetIngester( in);
            Vector v = t.getElements();
            pt_anal analysis = new pt_anal ( v, m, args[2], sz, ov);

        }
        catch (  Exception e )
        {
            System.err.println( "An Error occured while attempting to locate targets:" + e );
        }
    }
}

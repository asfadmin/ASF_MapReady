/******************************************************************
CLASS: Area

DESCRIPTION:
	Reads & holds basic info about a calibration site

HISTORY:
    DATE:    AUTHOR:    DESCRIPTION:
    -------------------------------------------------------------
    ??/????  ?. ?????   Initial development
    03/2004  P. Denny   Added commenting

*******************************************************************/

import java.util.* ;
import java.awt.* ;
import java.io.* ;

public class Area {
	String cs_name;   // Calibration Site name
	String cs_id;     // Calibration Site identification code
	String cs_desc;   // Calibration Site description

	/********************************************************
	 * Constructor:
	 * Grabs info for one calibration site from info stream
	 * coming from a sqsh shell script                     */
	public Area ( StreamTokenizer st ) throws IOException
	{
	    /* Skip the first batch of white space and other junk... */
	    eat_garbage(st);

	    /* Read cs_name */
	    if (st.sval != null)
	        cs_name = new String ( st.sval );	
	    else
	        cs_name = " ";
	
	    st.nextToken();

	    /* skip whitespace and other junk, then read cs_id */
	    eat_garbage(st);
	    if ( st.sval != null)
	        cs_id = new String ( st.sval );
	    else
	        cs_id = new String (" ");

	    /* Move on.. */
	    st.nextToken();

	    /* skip whitespace and other junk, then read cs_desc */
	    eat_garbage(st);
	    if ( st.sval != null) 
	        cs_desc =  new String ( st.sval );
	    else
	        cs_desc = new String (" ");

	    /* Move on.. */
	    st.nextToken();
	}

	/********************************************************************
	 * eat_garbage:
	 * While the read character is anything but "end of file" or "'" skip
	 * over it.  */
	private void eat_garbage ( StreamTokenizer st  ) throws IOException
	{ 
	    while ( (st.ttype != st.TT_EOF) && (st.ttype != 39) )
	        st.nextToken();
	}

        /**************************************************************
         * print:
         * Display the calibration site info available in this class */
	public void print ( )
	{
	    System.err.println( "'" + cs_name+"' '"+cs_id+"' '"+cs_desc+"'" );
	    System.err.println();
	}
}

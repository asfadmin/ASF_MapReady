import java.util.* ;
import java.awt.* ;
import java.io.* ;

public class Area {
	String cs_name;
	String cs_id;
	String cs_desc;
	
	private void eat_white_space ( StreamTokenizer st  ) throws IOException
	 { 
	    while ( (st.ttype != st.TT_EOF) && (st.ttype != 39) )
                st.nextToken();
  	 }
	
	public Area ( StreamTokenizer st ) throws IOException
    	  {
		/* Skip the first batch of white space and other junk... */
	    eat_white_space(st);
	     /* Read cs_name */
	    if ( st.sval != null)
	   	cs_name = new String ( st.sval );	
	    else
		cs_name = " ";
	
	   st.nextToken();

	     /* skip ws and other junk and then read cs_id */
	   eat_white_space(st);
	   if ( st.sval != null)
	   	cs_id = new String ( st.sval );
	   else
		cs_id = new String (" ");
	
	     /* Move on.. */
	   st.nextToken();
		
	     /* skip ws and other junk and then read cs_desc */
	   eat_white_space(st);
	   if ( st.sval != null) 
	   	cs_desc =  new String ( st.sval );
	   else
		cs_desc = new String (" ");

	     /* Move on.. */
	   st.nextToken();

	  }

	public void print ( )
          {
		System.err.println( "'" + cs_name+"' '"+cs_id+"' '"+cs_desc+"'" );
		System.err.println();
	  }
  }

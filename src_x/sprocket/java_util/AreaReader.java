import java.util.* ;
import java.awt.* ;
import java.io.* ;

public class AreaReader {
     private Vector v;
     public AreaReader ( InputStream in)
       {
	
	 try { 
 	   Reader r = new BufferedReader(new InputStreamReader(in));
           StreamTokenizer st = new StreamTokenizer(r);
	   Area a;

 	   v = new Vector();

	   st.resetSyntax(); 
	   st.quoteChar('\'');
	   st.whitespaceChars('-','-');
	   st.whitespaceChars(' ',' ');

	   while ( st.ttype != st.TT_EOF )
		v.add ( new Area (st) );
	  }
	 catch ( IOException e)
	     {
		System.err.println( "An Error occured:" + e );
	     }
       }

     public Vector getElements()
        {
	  return ( new Vector ( v ));
	}
         
  }

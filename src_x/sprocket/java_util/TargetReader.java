import java.util.* ;
import java.awt.* ;
import java.io.* ;

public class TargetReader {
     private Vector v;
     public TargetReader ( InputStream in)
       {
	
	 try { 
	   Reader r = new BufferedReader(new InputStreamReader(in));
           StreamTokenizer st = new StreamTokenizer(r);
 	   v = new Vector();
	   st.resetSyntax(); 
	   st.quoteChar('\'');
	   st.whitespaceChars('-','-');
	   st.whitespaceChars(' ',' ');
	   while ( st.ttype != st.TT_EOF )
		v.add ( new Target (st) );
	   v.remove(v.size()-1);
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

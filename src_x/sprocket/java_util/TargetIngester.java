import java.util.* ;
import java.awt.* ;
import java.io.* ;

public class TargetIngester {
     private Vector v;
     public TargetIngester ( InputStream in)
       {

         try {
           Reader r = new BufferedReader(new InputStreamReader(in));
           StreamTokenizer st = new StreamTokenizer(r);
           v = new Vector();

           st.resetSyntax();
	   st.wordChars(' '+1, '\"' -1);
	   st.wordChars('\"'+1, '~');
           st.whitespaceChars(' ',' ');
           st.whitespaceChars('\t','\t');
	   st.quoteChar('\"');

           while ( (st.ttype != st.TT_EOF) /* && (in.available() != 0)*/ )
	     {
		Target t = new Target();
		t.injest_target(st);
		v.add (t);
	     }
           //v.remove(v.size()-1);
          }
         catch ( IOException e)
             {
		System.err.println("ERROR:\tCould not read target (" + e +")");
             }
       }

     public Vector getElements()
        {
          return ( new Vector ( v ));
        }


	/*
      public static void main ( String [] args)
	{
	 InputStream in;
	 try {
	   	in = new FileInputStream(
			new File (
			"/scratch/tmp/E105791160G1U096.converted.ground_target"));
		TargetIngester t = new TargetIngester( in);	
		System.err.println("Read " + t.v.size());
	   }
	 catch (  Exception e )
	   {
		 System.err.println( "An Error occured:" + e );
	   }

	}
	*/
  }


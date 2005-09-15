import java.io.*;
import java.util.*;
/******************************************************
*  MetaReader
*	Reads value = "keyword" constructs and returns
*	the results as two arrays, one of keywords (token)
*	and the other of values (values).
*
*******************************************************/
public class MetaReader {
		/* Token is the array of keywords */
	  	/* Value is the array of values */
	public ArrayList tokens, values;

	public MetaReader ( InputStream in ) throws java.io.IOException
	  {
              Reader r = new BufferedReader(new InputStreamReader(in));
              StreamTokenizer st = new StreamTokenizer(r);

		/* Allocate space for tokens and values */
	      tokens = new ArrayList();
	      values = new ArrayList();

		/* Setup Parser */
	      st.resetSyntax();
		
		/* Set " as the quote char */
	      st.quoteChar('\"');

		/* Set ' ', \t and = to whitespace */	
	      st.whitespaceChars(' ',' ');
	      st.whitespaceChars('\t','\t');
	
		/* Set what chars compose words */
	        //st.wordChars('#', 'z');
	      	st.wordChars(' '+1, '\"' -1);
           	st.wordChars('\"'+1, '~');

	      while ( st.nextToken() != st.TT_EOF )
                {
		   switch (st.ttype )
		    {

                                /* = -> do nothing */
			case StreamTokenizer.TT_WORD : 
				if (st.sval.equals("="))
				    break;
			     	tokens.add(st.sval);
				/* System.err.println ("TT_WORD = " 
					+ st.sval );
				*/
			     	break;

				/* EOL -> do nothing */
			case StreamTokenizer.TT_EOL: 
				break;

				/* " -> implies value */
			case '\"' :
				values.add(st.sval);
				/*System.err.println (" String = "
                                        + st.sval );
				*/
				break;

			default:
				System.err.println(
				   "MetaReader:\tUnknown char read \"" +
				   st.ttype + "/" + st.toString());
		    }

                }

	  }
   }

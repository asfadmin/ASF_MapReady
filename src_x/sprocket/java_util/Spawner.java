import java.util.* ;
import java.awt.* ;
import java.io.* ;

public class Spawner {
   OutputStream Out;
   InputStream In;
   InputStream Err;
   public Spawner  ( String command )
     {
       try {
	 System.err.println("Running \"" + command + "\"...");
	 Process P = Runtime.getRuntime().exec(command);
	 Out = P.getOutputStream();
	 In = P.getInputStream();
	 Err = P.getErrorStream();
	 //P.waitFor();
	 
	}
       catch ( Exception e)
	{
	  System.err.println( "A error occurred ("+ e + ")");
	}
     }
  }



public class DEBUG {


    static void logger ( String s )
    {
      // System.out.println(s);
    }

   static void error ( String s )
    {
	System.err.println("ERROR:\t" +s );
    }


  static void status ( String foo )
    {
      System.out.println(foo);
    }
  static void pre_progress ( String foo)
    {
	System.out.print(foo + ": " );
    }

  static void post_progress ()
    {
        System.out.println( " Finished." );
    }
  static void progress ( int a )
    {
	int foo = a % 8;
	switch ( foo )
	 {
   	 case 0:  
		System.err.print("\b|");
           	return;
	 case 1:
		System.err.print("\b/");
                return;
	 case 2:
                System.err.print("\b-");
                return;
	 case 3:
                System.err.print("\b|");
                return;
	 case 4:
		System.err.print("\b\\");
                return;
	 case 5:
                System.err.print("\b-");
                return;
	 case 6:
                System.err.print("\b/");
                return;
	 case 7:
                System.err.print("\b-");
                return;
        }
    }

}

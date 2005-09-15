import java.util.*;

public class ASF_Time extends GregorianCalendar {
	public ASF_Time ( String str )
	  {
		super();	
		int year, jday, hour, minute, second, index;

	        /* Decode something like */
		/*1992-210T20:46:53.378*/

		//System.err.println("Recieved:" + str);

		/* Extract year */
		year = Integer.parseInt(str.substring(0,4));
		
		/* Extract jul date */
		index = str.indexOf('T');
		jday = Integer.parseInt(str.substring(5,index));

		/* Extract hour */
                hour = Integer.parseInt(str.substring(index+1,index+3));

		/* Extract minute */
                minute = Integer.parseInt(str.substring(index+4,index+6));

		/* Extract second */
		second =Integer.parseInt(str.substring(index+7,index+9));

		set(YEAR, year);
		set(DAY_OF_YEAR, jday);
		set(HOUR, hour);
		set(MINUTE, minute);
		set(SECOND, second);
		//computeFields();

	/*
		System.err.println("YEAR: " + year);
		System.err.println("DAY OF YEAR: " + jday);
                System.err.println("HOUR: " + hour);
                System.err.println("MINUTE: " + minute);
		System.err.println("SECOND: " + second);
		System.err.println("Generated: "+toASFDate());
	*/
	  }

	private String crush( int value, int dig)
     	  {
	     String Value = Integer.toString(value);
	     switch ( dig )
		{
		 case 0:
		 case 1: return new String (Integer.toString(value));
		 case 2:
			if ( value < 10 )
				return  new String ( "0" + Value);
			else
			   return new String (Value);
		 case 3:
			if ( value < 10 )
                                return  new String ( "00" + Value);
                        else
				if ( value < 100 )
                                	return  new String ( "0" + Value);
				else
					return (Value);
		}

	     return("0");
	  }

	public String toASFDate()
	  {
	     	String foo = new String();
		int year, jday, hour, minute, second;

		year = get(YEAR);
	   	jday = get(DAY_OF_YEAR);
		hour = get(HOUR);
                minute = get(MINUTE);
	        second = get(SECOND);

		foo =Integer.toString(year)+"-"+crush(jday,3)+"T"+crush(hour,2)+":"+
			crush(minute,2)+":"+crush(second,2)+".000";
	     	return foo;
	  }

	public void adjust_by_seconds(int seconds)
	  {
		add ( SECOND, seconds);
		computeFields();
	  }
  }

public class resolution {
    public double res;
    public double error;
    boolean valid;

    resolution ( double r, double e)
    {
        res = r;
        error = e;
	valid = true;
    }
   resolution ( boolean t )
	{
		valid = t;
	}
}


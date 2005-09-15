public class lobe{
    public int pos;
    public int neg;
    public boolean valid;

    lobe( int p, int n)
    {
        pos = p;
        neg = n;
	valid = true;
        DEBUG.logger("Lobe ( pos -> " + p + " neg -> " + neg + ")");
    }
    lobe ( boolean t )
      {
	valid = t;
      }

}

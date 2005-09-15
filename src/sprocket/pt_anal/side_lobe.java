public class side_lobe{
    public int pos;
    public boolean found;
    boolean valid;

    side_lobe( int lb, boolean err )
    {
        pos = lb;
        found = err;
	valid = true;
    }

   side_lobe ( boolean t )
    {
	valid = t;
    }

}

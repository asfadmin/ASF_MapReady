
public class location {
    public int x;
    public int y;
    public boolean valid;
    public location (int X, int Y )
    {
        x = X;
        y = Y;
        DEBUG.logger("Location ( X => " + X + " Y => " + Y );
    }
    public location ( boolean t )
     {
	valid = t;
	DEBUG.logger("Location ( " + t + " )" );
     }

}

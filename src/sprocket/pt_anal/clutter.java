public class clutter {
    public double clutter_power;
    public int n_clutter;
    public boolean valid;

    clutter ( double cp, int nc)
    {
        clutter_power = cp;
        n_clutter = nc;
        DEBUG.logger("Clutter (cp->" + cp + " nc -> " + nc + ")");
	valid = true;
    }
    public clutter ( boolean t )
     {
        valid = t;
        DEBUG.logger("Clutter ( " + t + " )" );
     }


}

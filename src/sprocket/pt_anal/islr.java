public class islr{
    public double value;
    public double err;
    public boolean valid;

    islr( double i_value, double err_value)
    {
        value = i_value;
        err = err_value;
        valid = true;
    }

    islr()
    {
        valid = false;
    }
    public islr ( boolean t )
     {
        valid = t;
        DEBUG.logger("Islr ( " + t + " )" );
     }

}

public class pslr{
    public double value;
    public double err;
    public boolean valid;

    pslr( double p_value, double err_value)
    {
        value = p_value;
        err = err_value;
        valid = true;
    }
    pslr( double p_value, double err_value, boolean tr)
    {
        value = p_value;
        err = err_value;
        valid = tr;
    }
    public pslr ( boolean t )
     {
        valid = t;
        DEBUG.logger("Pslr ( " + t + " )" );
     }



}

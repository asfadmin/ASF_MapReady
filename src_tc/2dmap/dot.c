



double dot(a,b)

double a[],
       b[];

   {
 
   int i;

   double adotb;
   adotb = 0.0;

   for (i = 1; i <= 3; i++)
      adotb = (a[i] * b[i]) + adotb;
      
    return (adotb);
   }


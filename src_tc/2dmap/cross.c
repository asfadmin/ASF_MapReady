


   /* calculate a X b return new vector in c */




void cross(a,b,c)

double a[],
       b[],
       c[];

   {
  
    c[1] = a[2] * b[3] - a[3] * b[2]; 
    c[2] = a[3] * b[1] - a[1] * b[3];
    c[3] = a[1] * b[2] - a[2] * b[1];
      
    return;
   }


BEGIN { first = "mace" }
{
   if ($1 == first)
     printf(" %s", $3);
   else
   {
     printf("\n");
     printf("perl $BIN/order.pl %s %s %s", $1,$2, $3);
     first = sprintf("%s", $1);
   }
}
END {printf("\n");}

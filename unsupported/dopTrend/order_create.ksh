#!/usr/bin/ksh
#

awk -f $BIN/order.awk < order_2.data >| order_1.ksh
/bin/rm -f *.odl
chmod 700 order_1.ksh
./order_1.ksh  >| order_2.ksh
chmod 700 order_2.ksh

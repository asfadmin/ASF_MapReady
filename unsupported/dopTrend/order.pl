#!/usr/local/bin/perl

   &usage if ($#ARGV < 1);
   $argcount = @ARGV;
   $frameno = 2;

   $orbit = $ARGV[0];
   $mode = $ARGV[1];
   $frame = $ARGV[2];

   $fcheck = sprintf ("R1_%05d_%s*",$orbit,$mode);
   $fname = sprintf ("R1_%05d_%s_%03d",$orbit,$mode,$frame);
   $fname .= ".odl";

   $list = `grep PACKAGE_ID $fcheck 2> /dev/null`;
   print $list;
   if ( -e $fname )
   {
     print "File $fname exists - overwrite ? ";
     $ans = (<STDIN>);
     die "Exiting\n" unless $ans =~ /^y/i;
   }

   open (OF,">$fname") || die "Can't open output file $fname";
   print "echo \" \"\necho $fname\nims_orderClnt -F $fname\n";

#   print "PID is $pid\n";
#   exit;
# MESSAGE_ID           = "B873405097"
# REQUEST_ID           = "873404733-12584-P1"
print OF '
GROUP                  = PRODUCT_REQUEST
  MESSAGE_ID           = "123"
  REQUEST_ID           = "123"
  DATA_CENTER_ID       = "ASF"
  AUTHENTICATOR        = "ramm"

  GROUP                  = USER_AFFILIATION
    CATEGORY             = "USA"
    TYPE                 = "UNIVERSITY"
  END_GROUP              = USER_AFFILIATION

  GROUP                  = CONTACT_ADDRESS
    FIRST_NAME           = "Joe"
    LAST_NAME            = "Ramm"
    ADDRESS              = ("The Ohio State University", "108 Scott Hall", "1090 Carmack Road")
    CITY                 = "Columbus"
    PHONE                = "614-292-1107"
    EMAIL                = "kfn@iceberg.mps.ohio-state.edu"
    STATE                = "OH"
    COUNTRY              = "USA"
    ZIP                  = "43210"
    FAX                  = "614-292-4697"
    ORGANIZATION         = "Byrd Polar Research Center"
  END_GROUP              = CONTACT_ADDRESS

  GROUP                  = SHIPPING_ADDRESS
    FIRST_NAME           = "Kenneth"
    LAST_NAME            = "Jezek"
    MIDDLE_INITIAL       = "C"
    ADDRESS              = ("The Ohio State University", "108 Scott Hall", "1090 Carmack Road")
    CITY                 = "Columbus"
    PHONE                = "614-292-1107"
    EMAIL                = "kfn@iceberg.mps.ohio-state.edu"
    STATE                = "OH"
    COUNTRY              = "USA"
    ZIP                  = "43210"
    FAX                  = "614-292-4697"
    TITLE                = "Dr."
    ORGANIZATION         = "Byrd Polar Research Center"
  END_GROUP              = SHIPPING_ADDRESS

  GROUP                  = BILLING_ADDRESS
    FIRST_NAME           = "Kenneth"
    LAST_NAME            = "Jezek"
    MIDDLE_INITIAL       = "C"
    ADDRESS              = ("The Ohio State University", "108 Scott Hall", "1090 Carmack Road")
    CITY                 = "Columbus"
    PHONE                = "614-292-1107"
    EMAIL                = "kfn@iceberg.mps.ohio-state.edu"
    STATE                = "Ohio"
    COUNTRY              = "USA"
    ZIP                  = "43210"
    FAX                  = "614-292-4697"
    TITLE                = "Dr."
    ORGANIZATION         = "Byrd Polar Research Center"
  END_GROUP              = BILLING_ADDRESS

';


while ($frameno < $argcount)
{
   $pid = sprintf ("R1_%05d_%s_%03d",$orbit,$mode,$ARGV[$frameno]);
#   print "DBG: pid is $pid\n";
   
print OF '
  GROUP                  = LINE_ITEM';
if ($mode =~ /^E/) {
print OF '
    DATASET_ID           = "RADARSAT-1 HIGH INCIDENCE IMAGES LEFT LOOKING - FULL RES"
';
} else {
print OF '
    DATASET_ID           = "RADARSAT-1 STANDARD BEAM IMAGES LEFT LOOKING - FULL RES"
';
}
print OF "    PACKAGE_ID           = \"$pid\"";
print OF '
    PROCESSING_OPTIONS   = "RAMP"
    MEDIA_TYPE           = "DISK"
    MEDIA_FORMAT         = "CEOS SAR FILE"
    ADDITIONAL_INFO      = "DLT"
    EST_COST             = 15.50
    BILLING_ID           = "RAMM-REORDER"
  END_GROUP              = LINE_ITEM
';
  $frameno++;
}
print OF '
  GROUP                  = MONITOR
    SESSION_ID           = "ims1.asf.alaska.edu:12659:19970904:123135"
    TX_CLIENT            = ("873405099", "380177")
    RX_SERVER            = ("873405099", "312136")
  END_GROUP              = MONITOR

  GROUP                  = VERSION
    SENDER_VERSION       = "asf-ims_orderClnt"
    PROTOCOL_VERSION     = 3.2
  END_GROUP              = VERSION

END_GROUP              = PRODUCT_REQUEST

END
';

   close OF;

#  print "Submit order ?";
#  $ans = (<STDIN>);
#  if ($ans =~ /^n/i)
#  {
#     print "deleting $fname and exiting\n";
#	 unlink $fname;
#     exit;
#  }
#  system ("ims_orderClnt -F $fname");



sub usage
{
  print "Usage: genorder.pl <orbit> <mode> <frame> [<frame>....]\n";
  exit;
}

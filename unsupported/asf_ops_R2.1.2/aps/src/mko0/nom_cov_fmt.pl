#!/usr/local/bin/perl

die ("Error opening $ARGV[0]\n") if (!open (IF,$ARGV[0]));

$day = 0;
$hr = 0;
$min = 0;
$sec = 0.0;
$prev_lat=-90;
while (<IF>)
{
   @words = split;
   $cur_lat = ($words[5]+$words[7])/2;
   if ($cur_lat > $prev_lat)
   {
      $AD = 'A';
   } elsif ($cur_lat < $prev_lat) {
      $AD = 'D';
   }
   $prev_lat = $cur_lat;
   print "$day/$hr:$min:$sec  1 $AD $words[5] $words[6] $words[7] $words[8]\n";
   $sec += 1;
   if ($sec >= 60) {$sec -= 60; $min++};
   if ($min >= 60) {$min -= 60; $hr++};
   if ($hr >= 24) {$hr -= 60; $day++};
}


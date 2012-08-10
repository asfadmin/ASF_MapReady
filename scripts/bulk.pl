#!/usr/bin/perl

use Getopt::Long qw(:config pass_through);

use strict;
use warnings;

my $usage = q~Usage:
  bulk.pl [--odr=[<odr_index>|auto] <script> <source dir> <dest dir> <dem>
~;

my $odrf;
GetOptions("odr=s" => \$odrf);

#optional args have already been pulled out above
if(scalar(@ARGV) != 4) { print $usage; exit; }

my $script = $ARGV[0];
my $idir = $ARGV[1];
my $odir = $ARGV[2];
my $dem = $ARGV[3];

$idir =~ s/\/$//;
$odir =~ s/\/$//;

open(LOG, ">>$odir/log.txt") or die "Could not open log file";

LOG("ASF Tools info:");
LOG(`which asf_mapready`);
LOG(`asf_mapready --version`);

LOG("Input dir: $idir");
LOG("Output dir: $odir");
LOG("ODR: " . ($odrf or "No ODR option specified"));

# scan the ODR index if one was supplied
my %odr;
if($odrf and $odrf !~ /auto/i) {
  open(ODR, $odrf) or die "Could not open ODR index: $!";
  while(<ODR>) {
    my ($key, $val) = split(/\s+/, $_);
    $key = get_basename($key);
    $odr{$key} = $val;
  }
  close(ODR);
}

LOG("Setup complete, processing data", "");

# done with prep, get the list of files to process
my @infiles;
foreach(`ls $idir`) {
  push @infiles, "$idir/$_" if /(^LED-)|(\.L$)/;
}

# process the files
foreach (@infiles) {
  chomp;
  /([^\/]+)$/;
  my $meta = $1;
  my $base = get_basename($meta);
  LOG("Processing $base");
  my $odr;
  if($odrf) {
    $odr = get_odr($base); #automatically chooses between a supplied index or auto determination of odr
    LOG(defined($odr)?("Using ODR $odr"):("No ODR found!"));
  }
  my $cmd = "$script $meta.L $base $dem" . (defined($odr)?" $odr":"");
  LOG("Command: $cmd");
  
  open my $cmd_fh, "$cmd |";
  while (<$cmd_fh>) {
    LOG("  $_");
  }
  close($cmd_fh);

  LOG("$base complete", "");
}

close(LOG);
exit;


sub LOG {
  my $now = `date`;
  chomp($now);
  my @msg = @_;
  foreach(@msg) {
    chomp;
    print "[$now] $_\n";
    print LOG "[$now] $_\n";
  }
}

sub get_odr {
  my $g = get_basename(shift);
  if($odrf) {
    if($odrf =~ /auto/i) { #automatically deterine odr
      my $arclist;
      if($g =~ /^E1/) {
        $arclist = "/asf/insar/terrain_correction/prc/ers1/dgm-e04"; #need to do something about these hard-coded paths
      } elsif($g =~ /^E2/) {
        $arclist = "/asf/insar/terrain_correction/prc/ers2/dgm-e04";
      }
      my $d = `metadata -dssr $idir/$g.L | grep INPT`;
      $d =~ /\d{2}(\d{6})\d+/;
      my $center = $1;
      open(ARCLIST, "$arclist/arclist") or die "Could not open arclist: $!";
      while(<ARCLIST>) { #scan the arclist file for the odr we want
        if(/^(\d+)\s+(\d+)\s+[\d\:]+\s\-\s+(\d+)\s+[\d\:]+/) {
          my $odr = $1;
          my $start = $2;
          my $stop = $3;
          if($start <= $center and $stop >= $center) {
            return "$arclist/$odr.ODR";
          }
        }
      }
      close(ARCLIST);
      return undef; #no appropriate odr found in arclist
    } else { #determine odr from supplied index
      return $odr{$g} or undef;
    }
  } else { #no odr use requested
    return undef;
  }
}

sub get_basename {
  my $g = shift;
  $g =~ s/\.[LD]$//;
  $g =~ s/^(LED|IMG)\-//;
  $g =~ s/\-.+?$//;
  return $g;
}

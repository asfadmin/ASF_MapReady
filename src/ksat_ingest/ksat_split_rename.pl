#!/usr/bin/perl
use FileHandle;
use File::Basename;
use Getopt::Long;
use File::Copy;

if (($#ARGV + 1) < 2){die <<EOS ;}
*** $0

usage: $0 [options] <SARFILE> <SUBDIR>
    SARFILE   (input)  Input SAR file to process 
    SUBDIR    (input)  Directory to create files in (must exist)

Process a KSAT format combined CEOS file into individual CEOS files, 
renaming output to include orbit and time of scene center.

EOS

# GetOptions ('l' => \$lo_flg,'d' => \$dead_flg,'n' => \$no_match_flg, 'o=f' => \$post, 'g' => \$gamma0_flg );

$sarfile = $ARGV[0];
$procdir = $ARGV[1];

my ($s1, $dir1, $ext1) = fileparse($sarfile, qr/\.[^.]*/); #extension is the last bit after the last period
$log = "$s1".".log";

print "\n";
print "$0: splitting and renaming\n";
print "Input File: $sarfile\n";

$cmd = "split_ceos $sarfile $procdir";
execute($cmd,$log);

chdir("$procdir/$sarfile");

@orb = get_orbit("$sarfile","logfile");
$orbit = $orb[0];
$time = $orb[1];

$rawname = "RS1_F1_".$orbit."_".$time.".raw";
$ldrname = "RS1_F1_".$orbit."_".$time.".ldr";

print "Output File: $rawname, $ldrname\n";

if (-e "../$rawname") {
  print "*******************************************************************\n";
  print "WARNING NAME COLLISION:: Input file: $sarfile; Output file $rawname\n";
  print "*******************************************************************\n";
}

copy("dat_01.001","../$rawname");
copy("lea_01.001","../$ldrname");

# clean up after ourselves
unlink glob "*" or warn "Could not unlink all: $!";
chdir("..");
rmdir $sarfile or warn "Could not unlink $sarfile: $1";

print "Done: $sarfile\n";
exit(0);
    
sub execute{
  my ($command, $log) = @_;
  print "$command\n";

  my $out = `$command`;
  my $exit = $? >> 8;

  if (-e $log){open(LOG,">>$log") or die "ERROR $0: cannot open log file: $log  command: $command\n";}
  else {open(LOG,">$log") or die "ERROR $0 : cannot open log file: $log  command: $command\n";}
  LOG->autoflush;
  print LOG ("\nRunning: ${command}\nOutput:\n${out}\n----------\n");
  close LOG;
  if ($exit != 0) {
    # Moving the "ERROR" into the regular print, so that they appear in the right order
    print "\nnon-zero exit status: ${command}\nOutput:\n${out}\nERROR: non-zero exit status: $command\n";
    die "$0 ERROR";
  }
}

sub get_orbit {
  my ($sarfile, $log) = @_;

  execute("metadata -all -save lea_01.001",$log);
  $output = `cat lea_01.001.dssr`;
  my @meta = split /\n/, $output;   #split into lines

  foreach my $line (@meta) {
    if ($line =~ m/ORBIT NUMBER/) {
      @a = split (/[\s+,]/, $line);	#parse line into tokens, splitting on whitespace or ,
      $orbit_num = $a[-1];
      print "found orbit number $orbit_num\n";
    }
   
    if ($line =~ m/INPT SCN CTR TIME/) {
      @a = split (/[\s+,]/, $line);	#parse line into tokens, splitting on whitespace or ,
      $center_time = $a[-1];
      print "found center time $center_time\n";
    }
  }

  @parms=("$orbit_num","$center_time");
  return @parms;
}

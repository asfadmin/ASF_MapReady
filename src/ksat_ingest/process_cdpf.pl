#!/usr/bin/perl
use FileHandle;
use File::Basename;
use Getopt::Long;
use File::Copy;


my @files = glob("RS1_*.raw");
foreach my $file (@files) {
  my ($s1, $dir1, $ext1) = fileparse($file, qr/\.[^.]*/); #extension is last bit including last period
  if (-e ".$s1") { print "Found file .$s1\n";}
  if (! -e ".$s1") {
    print "processing $s1\n";
    mkdir($s1);
    chdir($s1);
    execute("ln -s ../$s1.raw .","../process.log");
    execute("ln -s ../$s1.ldr .","../process.log");
    execute("cdpf_ingest $s1","../process.log");

    print "moving files to ../FRAMES directory\n";
    for my $file (glob "*.D") { 
      if (! -e "../FRAMES/$file") {
        move ($file, "../FRAMES/$file") or die $!; 
        $file =~ s/D/L/;
        move ($file, "../FRAMES/$file") or die $!; 
      } else {
        $thissize = -s $file;
        $thatsize = -s "../FRAMES/$file";
        if ($thissize > $thatsize) {
          move ($file, "../FRAMES/$file") or die $!; 
          $file =~ s/D/L/;
          move ($file, "../FRAMES/$file") or die $!; 
        }
      }
    }

    unlink glob "*" or warn "Could not unlink all: $!";
    chdir("..");
    rmdir $s1 or warn "Could not remove directory $s1: $!";
    `touch .$s1`;
  }
}
  print "Done!!!\n";

exit 0;

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


#!/usr/local/bin/perl -w

use strict;

use File::Basename qw(basename);
use File::Temp qw(tempfile);
use Getopt::Long qw(GetOptions);
use Pod::Usage qw(pod2usage);
use Storable qw(nstore retrieve);
use Text::Wrap qw($columns wrap);

=head1 NAME

find_dependencies.perl

=head1 SYNOPSIS

cd /wherever/asf_tools_root_dir ; find_dependencies.perl [option]...

=head1 DESCRIPTION

This program examines the contents of the ASF source directories (src,
src_akdem, src_lib, etc. -- the list is hard wired in the code) and
attempts to divine the interdepencencies of the individual tools and
libraries. Various strange heuristics are used to do this; see the
source for details.  A cache file of the package dependency graph
called 'package_dependency_graph.cache' is generated if it does not
already exist (unless B<-p> or B<--package> is given) and used to
speed up future runs of the script; delete it to force dependency
graph regeneration.

This script tends to err on the side of caution and therefore
generates a lot of false positives, which need to be eliminated by
manual inspection when a tool is being worked on.  Sometimes these
false positives are even useful: they can show up places where a well
named tool or library is failing to be used by some other package.

Note that conventional divination (employing a seer, oracle, or gypsy
fortune teller) might do a better job than this script.

Note also that 'dependence' in the context of this script means only
that the dependent package directly calls or uses the depended-on
package.  In practice this is a shallow measure, since ASF tools form
a chain and pass data along in files.  Automatic program or library
tests in particular will likely need to use programs or libraries that
the programs or libraries they test do not directly or indirectly
depend on to set up the test environments.

And now one final caveat: the practice of having big piles of header
files which are not properly identified as part of any particular
package makes it extremely hard to tell what really depends on what.
This script could be made to report on these "unowned" headers, but
its probably not worth it; the only real fix is to get rid of the
seperate include directory and move the headers to the libraries
responsible for them, as is done everywhere else.

=head1 OPTIONS AND ARGUMENTS

If no options specifying certain types of output are used, the direct
dependencies for all packages are computed and printed.

The interesting options available are:

=over 4

=item B<-c> I<package_one>,I<package_two>, 
B<--check>=I<package_one>,I<package_two>

Check if I<package_one> depends directly or indirectly on
I<package_two>.  If there is dependency, print some of the relevant
unique dependency path(s) in the dependency graph, if not, print a
message saying there is no dependency.  Note that not all unique
dependency paths are necessarily shown, because the dependency graph
of ASF software packages is cyclic in places and I do not have an
algorithm for finding unique paths in cyclic graphs.  A space after
the comma will foul things up.

For example,

=over 4

find_dependences.perl -c create_dem,asf_meta

=back

will show some of the ways in which the create_dem package depends on
the asf_meta package.

=item B<-i> I<package_one>,I<package_two>, 
B<--independency>=I<package_one>,I<package_two>

Explicity declare that I<package_one> does not directly depend on
I<package_two>.  Note that one or more indirect dependencies may still
exist.  This option is useful if you know that the heuristics are
generating a false positive dependency, and you want to specify the
true situation.  A space after the comma will foul things up.  This
option may be used multiple times to declare multiple independencies.
This option will not permanently affect the package dependency graph
cache.  To save information about independencies, use the
B<--independency_file> option.

=item B<--independency_file>=I<file_name>

Parse file I<file_name> as a list of indepency rules.  Each line of
the file must consist of exactly two package names seperated by a
single comma without white space, and has exactly the same effects as
a corresponding B<-i> option would.  Perl-style comments may be used
in the file.

=item B<-p> I<package>, B<--package>=I<package>

Compute only the direct dependencies for package I<package>.  This is
much faster than generating a full dependency graph for all packages.
However, if a package dependency graph cache file exists, the savings
will be small.  If a package dependency graph does not exist, a run
using this option will not produce one.

This option may not be used together with either of the B<--check> or
B<--reverse_dependencies> options.

=item B<-r> I<package>, B<--reverse_dependencies>=I<package>

List all packages which directly or indirectly depend on package
I<package>.  This option may not be used together with the
B<--package> option.

=item B<-s>, B<--scan_comments>

Do not ignore comment text when looking for package names in program
text.  Note that this option is only relevant when a new package
dependency graph cache file is being created.  Use of this option will
probably result in a lot of false positives.

=back

The standard verbose, version, and help options are also available:

=over 4

=item B<-v>, B<--verbose>

Enable verbose operation and progress reporting.

=item B<--version>

Print version information.

=item B<-?>, B<--help>

Print usage information.

=back

=cut

my $progname = basename($0);
my $version = "0.4.0";

# Set up output wrapping.
if ( $ENV{COLUMNS} ) {
    $columns = $ENV{COLUMNS} - $ENV{COLUMNS} / 10; # 9/10ths full lines.
} else {
    $columns = 72;		# Assume 80 column terminal.
}

# Default values for command line parameters.
my %p = (
	 'check' => [],		# Ref to list to contain two package names.
	 'independency' => [],	# Each successive pair an independency.	
	 'independency_file' => undef, # File containing independency pairs.
	 'package' => undef,	# Package name to compute direct deps for.
	 # Package name to compute reverse deps for. 
	 'reverse_dependencies' => undef,
	 'scan_comments' => 0,	# Flag true if comment text to be scanned.

	 # Standard options. 
	 'verbose' => 0,	# Verbose mode on.
	 'version' => 0,	# If given print version and exit.
	 'help'	   => 0		# If given print help and exit.
	 );

sub babble { if ( $p{verbose} ) { print @_; } }

GetOptions(\%p, 'check|c=s', 'package|p=s', 'independency|i=s', 
	   'independency_file=s', 'reverse_dependencies|r=s',
	   'scan_comments|s', 'verbose|v', 'version', 'help|?')
    or pod2usage("$progname: option parse failed.\n");

if ( $p{version} ) {
    print <<END_VERSION_TEXT;
$progname version $version
END_VERSION_TEXT
}

if ( $p{help} ) {
    pod2usage('-exitval' => 0);
}

if ( $p{'package'} and (@{$p{'check'}} or $p{'reverse_dependencies'}) ) {
    pod2usage("$progname: --package (-p) option may not be used together with check (-c) or --reverse_dependencies (-r) options.\n");
}

if ( @ARGV ) {
    pod2usage("$progname: error: no (non-option) arguments allowed.\n");
}

# Parse check option argument.
if ( @{$p{'check'}} ) {
    if ( @{$p{'check'}} > 1 ) {
	pod2usage("$progname: only one check (-c) option is allowed.\n");
    }
    @{$p{'check'}} = split(/,/, join(',', @{$p{'check'}}));
    if ( @{$p{'check'}} != 2 ) {
	pod2usage("$progname: malformed check (-c) option argument.\n");
    }
}

# Parse independency option arguments.
my %pkg_independencies;		# Explicit independency graph (hash of lists).
if ( @{$p{'independency'}} ) {
    if ( @{$p{'independency'}} ) { 
	@{$p{'independency'}} = split(/,/, join(',', @{$p{'independency'}}));
	if ( @{$p{'independency'}} % 2 != 0 ) {
	    pod2usage("$progname: malformed independency (-i) option argument.\n");
        }
    }
    for ( my $i = 0 ; $i < @{$p{'independency'}} ; $i += 2 ) {
	$pkg_independencies{${$p{'independency'}}[$i]} ||= [];
        push(@{$pkg_independencies{${$p{'independency'}}[$i]}},
             ${$p{'independency'}}[$i + 1]);
    }
}

# Parse independency_file option argument.
if ( $p{'independency_file'} ) {
    open(INDEPENDENCY_FILE, "<$p{'independency_file'}")
	or die "$progname: couldn't open $p{'independency_file'} for reading: $!\n";
    my $line_number = 1;	# Line number being parsed.
    while ( <INDEPENDENCY_FILE> ) {
	chomp;			# Strip newline.
	s/#.*$//;		# Strip comments.
	s/^\s*//;		# Strip leading white space.
	s/\s*$//;		# Strip trailing white space.
	# Skip blank lines.
	unless ( $_ ) {
	    next;
	}
        unless ( (my @new_independency = split(/,/)) == 2 ) {
            die "$progname: malformed independency at line $line_number of file $p{'independency_file'}\n";
        } elsif ( $new_independency[0] =~ m/^(\s.*)|(.*\s)$/
                   or $new_independency[1] =~ m/^(\s.*)|(.*\s)$/ ) {
            die "$progname: malformed independency at line $line_number of file $p{'independency_file'}, maybe illegal whitespace around comma?\n";
        } else {
            $pkg_independencies{$new_independency[0]} ||= [];
            push(@{$pkg_independencies{$new_independency[0]}}, 
                 $new_independency[1]);
	    $line_number++;
        }
    }
    close(INDEPENDENCY_FILE) 
        or die "couldn't close $p{'independency_file'}: $!";
}

# Hash of package dependency list refs.
my %pkg_deps;

# File in which to cache the dependency graph.
my $graph_cache_file = 'package_dependency_graph.cache';

# Try to use dependency graph cache file.
if ( -e $graph_cache_file ) {
    my $pkg_deps_ref = retrieve($graph_cache_file);
    unless ( $pkg_deps_ref ) {
	die "$progname: failed to correctly retrieve $graph_cache_file, maybe delete it to force regeneration?\n";
    }
    print "\nNOTE: using package dependency graph cache file\n'$graph_cache_file'. Delete it and rerun if its stale.\n";
    if ( $p{'scan_comments'} ) {
	print wrap('', '', "\nWARNING: scan comments option (-s or "
		   ."--scan_comments) is irrelevant when using an existing "
		   ."package dependency graph cache file.\n");
    }
    sleep 4;
    %pkg_deps = %{$pkg_deps_ref};

} else {
    # Regenerate package graph cache.

    # Unless doing just a single package, warn user about long delay.
    unless ( $p{'package'} ) {
	print "\nNo package dependency graph cache found, building dependendy graph (takes\na loooonng time, like hours and hours)...\n";
	sleep 4;
    }

    # Directories for different types of source.
    my @src_dirs = qw(src src_akdem src_geo src_ifm src_jpl src_lib src_mask 
		      src_mpi src_tc src_x);

    # Directories of individual packages, relative to top level (where
    # script is).
    my @pkg_dirs;
    &babble("\nBuilding list of package directories...\n");
    foreach my $dir ( @src_dirs ) {
        unless ( -d $dir ) {
	    &babble("\nSkipping nonexistent source directory $dir...\n");
	    next;
        }
        &babble("\nLooking for package directories in $dir...\n");
        foreach my $entry ( `ls -1 $dir` ) {
	    chomp($entry);
	    if ( -d "$dir/$entry" and "$dir/$entry" !~ /(CVS|\/\.)/ ) { 
    	        push(@pkg_dirs, "$dir/$entry");
	        &babble("  Added $dir/$entry to list of package directories.\n");
	    }
        }
    }

    my $top_level_dir = ${my $tmp = `pwd`; chomp($tmp); \$tmp; };

    foreach my $dir ( @pkg_dirs ) {

	chdir "$top_level_dir/$dir";

	# Get package name without directory part.
	$dir =~ m{\/([^/]+)$};
	my $pkg_name = $1;

	&babble("\nComputing dependencies for package $pkg_name...\n");

        # If we are computing direct dependencies for a single package
        # skip ahead unless we are looking at that package.
        if ( $p{'package'} ) {
            unless ( $pkg_name eq $p{'package'} ) {
	        next;
  	    }
        }

        # Initialize reference to empty list.
        $pkg_deps{$pkg_name} = [];

        # Check for dependencies on every other package...
        foreach my $other_dir ( @pkg_dirs ) {

	    # No need to worry about packages depending on themselves.
  	    if ( $dir eq $other_dir ) {
	        next;
	    }
	
	    my $dep_flag = 0;	# Flag true if other package is depended on.
	
	    # Get package name without directory part.
	    $other_dir =~ m{\/([^/]+)$};
	    my $other_pkg_name = $1;

	    &babble("  Checking for direct dependence on package $other_pkg_name... ");

	    # Heuristics go here.
	  
	    # Inclusion of a header file with a name corresponding to a
	    # package name is taken to mean dependence.
	    if ( !system("grep -q -I '#include[^a-zA-Z0-9]*$other_pkg_name"
		         ."[^a-zA-Z0-9]*' *.[ch] 2>/dev/null") ) {
	        $dep_flag = 1;
	    }

	    # Mere mention of another package in a file containing
	    # roughly the string "system(" or beginning with something
	    # like #!/bin/sh is taken to mean dependence.
	    foreach my $entry (`ls -1`) { 
	        chomp($entry); 

		# Skip directories.
	        if ( -d $entry ) {
		    next;
	        }

		# Make a temp file and strip comments as appropriate.
		open(ENTRY, "<$entry") or die "open failed: $!";
		my $file_contents = join('', <ENTRY>);
		close(ENTRY) or die "close failed";
		unless ( $p{'scan_comments'} ) {
		    if ( $entry =~ m/\.[ch]$/ ) {
			$file_contents =~ s!/\*.*?\*/!!gs;
		    } elsif ( $file_contents =~ m/^.!\/.*\/(ba)?sh/ ) {
			$file_contents =~ s/#[^!][^"']*?\n/\n/gs;
		    }
		}
		# Get temp file handle and name.
		my ($tfh, $tfn) = tempfile(DIR => "/tmp", UNLINK => 1);
		print $tfh $file_contents;
		close($tfh) or die "close failed on $tfn";

		# Look for references to other packages in temp file.
	        if ( !system("grep -q -I '[^_[:alpha:]]$other_pkg_name"
			     ."[^_[:alpha:]]' $tfn 2>/dev/null") 
		      and ( !system("grep -q -I 'system *(' $tfn "
				    ."2>/dev/null") 
		            or `head -n 1 $tfn` =~ /^.!.*\/sh/ ) ) {
		    $dep_flag = 1;
	        }

		# Remove temporary file.
		unlink($tfn) or die "unlink failed";
	    }

	    # If a library file with a base name corresponding to the
	    # package name is mentioned in the Makefile, it is taken
	    # as a dependency.  This protects against some libraries
	    # with inconsistently named headers.
            if ( !system("grep -q '[^_[:alpha:]]$other_pkg_name\.a' "
			 ."[Mm]akefile 2>/dev/null") ) {
	        $dep_flag = 1;
	    }
	
	    if ( $dep_flag ) {
	        &babble("yes.\n");
	        push(@{$pkg_deps{$pkg_name}}, $other_pkg_name);
	    } else {
	        &babble("no.\n");
	    }
	}

        # If in verbose mode, print dependency summary for this package.
        if ( $p{verbose} ) {
	    print "\n$pkg_name\n";
    	    if ( @{$pkg_deps{$pkg_name}} ) {
	        print "   |\n";	# Print cute little downward ASCII art line.
	    } else {
	        print "   No dependencies.\n";
	    }
	    foreach my $other_pkg ( @{$pkg_deps{$pkg_name}} ) {
	        print "   +--> $other_pkg\n";
	    }
        }
    }

    chdir "$top_level_dir";

    # Unless we have an incomplete graph due to use of the -p option,
    # store package dependency graph in file for later use.
    unless ( $p{'package'} ) {  
        unless ( nstore(\%pkg_deps, $graph_cache_file) ) {
	    die "$progname: failed to correctly store dependency graph in $graph_cache_file\n";
	} else {
	    print wrap('', '', "\nGenerated package dependency graph cache file\n'$graph_cache_file'.\n");
	}
    }
}

# Validate and apply independencies.
if ( %pkg_independencies ) {
    &babble("\nValidating and applying independencies...\n");
    foreach my $pkg ( keys %pkg_independencies ) {
        # If in single package  mode...
        if ( $p{'package'} ) {
	    # Don't try to apply independencies except to the package
	    # being dealt with
	    unless ( $pkg eq $p{'package'} ) {
		next;
	    }
	}
	unless ( exists($pkg_deps{$pkg}) ) {
	    die "$progname: unknown package name '$pkg' in independency\n";
	}
        foreach my $other_pkg ( @{$pkg_independencies{$pkg}} ) {
	    unless ( $p{'package'} or exists($pkg_deps{$other_pkg}) ) {
                die "$progname: unknown package name '$other_pkg' in independency\n";
            }
            my @new_deps = (); # Post-independency dependency list.
            foreach my $dependency ( @{$pkg_deps{$pkg}} ) {
                unless ( $dependency eq $other_pkg ) { 
                    push(@new_deps, $dependency);
                } else {
		    print wrap('', '', "\nRemoving detected direct dependency of $pkg on $dependency from dependency graph due to declared independency...\n");
		}
            }
            @{$pkg_deps{$pkg}} = @new_deps;
        }
    }
}


unless ( @{$p{'check'}} or $p{'reverse_dependencies'} ) {
    # Generate official (not verbose) output.

    foreach my $pkg ( keys %pkg_deps ) {
	# If we are computing direct dependencies for a single package
	# skip ahead unless we are looking at that package.
	if ( $p{'package'} ) {
	    unless ( $pkg eq $p{'package'} ) {
		next;
	    }
	}
	print "\n$pkg\n";
	if ( @{$pkg_deps{$pkg}} ) {
	    print "   |\n";	# Print cute little downward ASCII art line.
	} else {
	    print "   No dependencies.\n";
	}
	foreach my $other_pkg ( @{$pkg_deps{$pkg}} ) {
	    print "   +--> $other_pkg\n";
	}
    }

}

if ( @{$p{'check'}} ) {			
# Check output requested, examine dependency graph for dependency paths.

    # Verify that check arguments are known package names.
    unless ( exists($pkg_deps{${$p{'check'}}[0]}) ) {
        die wrap('', '', "$progname: check argument ${$p{'check'}}[0] is not a valid package name\n");
    }
    unless ( exists($pkg_deps{${$p{'check'}}[1]}) ) {
        die wrap('', '', "$progname: check argument ${$p{'check'}}[1] is not a valid package name\n");
    }

    &babble("\nLooking for all dependencies (direct and indirect) of \n"
            ."${$p{'check'}}[0] on ${$p{'check'}}[1]...\n");

    my $paths_buf = "";		# Accumulates dependency paths.
    my %visited;		# Has entries for package nodes visited.

# Subroutine to do recursive depth first search for depended on package.
sub path_search
{
    # Arguments are name of current package, and path from starting
    # package to current package node.
    my ($pkg_node, $path) = @_;

    $visited{$pkg_node} = 1;	# Set visited flag for not to true.

    foreach my $adjacent_pkg_node ( @{$pkg_deps{$pkg_node}} ) {
	if ( $visited{$adjacent_pkg_node} ) {
	    next;
	}
	if ( $adjacent_pkg_node eq ${$p{'check'}}[1] ) {
	    $paths_buf .= "$path --> ${$p{'check'}}[1]\n";
	    next;
	}
	&path_search($adjacent_pkg_node,
		     $path." --> ".$adjacent_pkg_node);
    }
}

    # Perform path search.
    &path_search( (${$p{'check'}}[0]) x 2);

    print "\n";

    if ( $paths_buf ) {		# If paths buffer not empty...
	print wrap('', '', "${$p{'check'}}[0] depends on ${$p{'check'}}[1] in *AT LEAST* the following way(s):\n");
        my @paths = split(/\n/, $paths_buf);
        foreach my $path ( @paths ) {
            print wrap(' ' x 2, ' ' x 4, "$path\n");
        }
        print "Remember this is *NOT* a complete list of dependency paths.\n";
    } else {
	print "${$p{'check'}}[0] does not depend on ${$p{'check'}}[1].\n";
    }
}

if ( $p{'reverse_dependencies'} ) {
# Reverse dependency output requested.

    # Verify that reverse_dependencies argument is known package name.
    unless ( exists($pkg_deps{$p{'reverse_dependencies'}}) ) {
        die wrap('', '', "$progname: --reverse_dependencies option argument ${$p{'check'}}[0] is not a valid package name\n");
    }


    &babble("\nLooking for all dependencies (direct and indirect) on "
	    ."$p{'reverse_dependencies'}...\n");

    # Construct reverse dependency graph from dependency graph.
    my %reverse_deps;
    foreach my $pkg ( keys %pkg_deps ) {
	foreach my $other_pkg ( @{$pkg_deps{$pkg}} ) {
            $reverse_deps{$other_pkg} ||= [];
            push(@{$reverse_deps{$other_pkg}}, $pkg);
        }
    }

    my %visited;		# Has entries for nodes visited.

# Routine to recursively traverse component connected to package.
sub depth_first_traversal
{    
    # Arguments are name of current package.
    my ($pkg_node) = @_;

    $visited{$pkg_node} = 1;	# Set visited flag for not to true.

    foreach my $adjacent_pkg_node ( @{$reverse_deps{$pkg_node}} ) {
	if ( $visited{$adjacent_pkg_node} ) {
	    next;
	}
	&depth_first_traversal($adjacent_pkg_node);
    }
}

    &depth_first_traversal($p{'reverse_dependencies'});
    
    print wrap('', '', "\nPackages with direct dependencies on $p{'reverse_dependencies'}:\n");

    foreach my $pkg ( @{$reverse_deps{$p{'reverse_dependencies'}}} ) {
	# So we don't hear about package again under indirect dependencies...
	delete($visited{$pkg});
	print "  $pkg\n";
    }

    print wrap('', '', "\nPackages with indirect dependencies on $p{'reverse_dependencies'}:\n");

    foreach my $pkg ( keys %visited ) {
	print "  $pkg\n";
    }
}

print "\n";

=head1 AUTHOR

Britton Kerin <bkerin@mail1.asf.alaska.edu>

=cut

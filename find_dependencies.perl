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

This program examines the contents of the top level ASF source
directories (src, src_akdem, src_lib, etc. -- the list is hard wired
in the code) and attempts to divine the interdepencencies of the
individual tools and libraries.  In general, one dependency graph node
is created for each directory immediately under the top level
directories.  The name of the node created is the same as the name of
the directory the tool or library source code lives in.

Various strange heuristics are used to actually determine which tools
depend on which, see the source for details.  A cache file of the
dependency graph called 'dependency_graph.cache' is generated if it
does not already exist (unless B<-p> or B<--package> is given) and
used to speed up future runs of the script; delete it to force
dependency graph regeneration.

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
dependencies for all nodes are computed and printed.

The interesting options available are:

=over 4

=item B<-c> I<node_one>,I<node_two>, 
B<--check>=I<node_one>,I<node_two>

Check if I<node_one> depends directly or indirectly on I<node_two>.
If there is dependency, print some of the relevant unique dependency
path(s) in the dependency graph, if not, print a message saying there
is no dependency.  Note that not all unique dependency paths are
necessarily shown, because the dependency graph of ASF software
packages is cyclic in places and I do not have an algorithm for
finding unique paths in cyclic graphs.  A space after the comma will
foul things up.

For example,

=over 4

find_dependences.perl -c create_dem,asf_meta

=back

will show some of the ways in which the create_dem package depends on
the asf_meta package.

=item B<-i> I<node_one>,I<node_two>, 
B<--independency>=I<node_one>,I<node_two>

Explicity declare that I<node_one> does not directly depend on
I<node_two>.  Note that one or more indirect dependencies may still
exist.  This option is useful if you know that the heuristics are
generating a false positive dependency, and you want to specify the
true situation.  A space after the comma will foul things up.  This
option may be used multiple times to declare multiple independencies.
This option will not permanently affect the dependency graph cache.
To save information about independencies, use the
B<--independency_file> option.

=item B<--independency_file>=I<file_name>

Parse file I<file_name> as a list of indepency rules.  Each line of
the file must consist of exactly two node names seperated by a single
comma without white space, and has exactly the same set of effects as
a corresponding B<-i> option would.  Perl-style comments may be used
in the file.

=item B<-l> I<node>, B<--loose_match>=I<node>

Use low criteria when scanning packages for depencence on node
I<node>.  Pretty much any occurence of the name I<node> in a package
will be taken as a dependency.  This option may well generate false
positives, but is extremely unlikely to miss anything, and works well
for looking for depenence on explicitly declared non-package nodes
like types (see the --with_node option).  This option is only relevant
when a new dependency graph cache file is being created.

=item B<--loose_match_file>=I<file_name>

Parse file I<file_name> as a list of nodes to be matched loosely.
Each line of the file must consist of exactly one node name, and has
exactly the same effects as a corresponding B<-l> option would.
Perl-style comments may be used in the file.

=item B<-p> I<node>, B<--package>=I<node>

Compute only the direct dependencies for node I<node>.  This is much
faster than generating a full dependency graph for all nodes.
However, if a package dependency graph cache file exists, the savings
will be small.  If a package dependency graph does not exist, a run
using this option will not produce one.

This option may not be used together with either of the B<--check> or
B<--reverse_dependencies> options.

=item B<-r> I<node>, B<--reverse_dependencies>=I<node>

List all nodes which directly or indirectly depend on node I<node>.
This option may not be used together with the B<--package> option.

=item B<-s>, B<--scan_comments>

Do not ignore comment text when looking for node names in program
text.  This option is only relevant when a new dependency graph cache
file is being created.  Use of this option will probably result in a
lot of false positives.

=item B<-w> I<name>, B<--with_node>=I<name>

Force a dependency graph node of name I<name> to exist, even if there
is no individual tool directory I<name>.  This node is (probably
wrongly) assumed to have no dependencies of its own.  This option is
only relevant when a new dependency graph cache file is being created.
Note that this option may be used together with the --loose_match
option to add arbitrary textual dependency information to the
dependency graph.  For example, '--loose_match=the_magic_type
--with_node=the_magic_type' may be used to include in the dependency
graph information about which packages directly or indirectly depend
on the_magic_type (including, somewhat unfortunately, the package in
which the type is defined).  This is a potentially useful way to deal
with the problem of header files which do not properly belong to any
individual package.

=item B<--with_node_file>=I<file_name>

Parse file I<file_name> as a list of nodes to force to exist.  Each
line of the file must consist of exactly one node name, and has
exactly the same effects as a corresponding B<-w> option would.
Perl-style comments may be used in the file.

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
my $version = "0.7.0";

# Set up output wrapping.
if ( $ENV{COLUMNS} ) {
    $columns = $ENV{COLUMNS} - $ENV{COLUMNS} / 10; # 9/10ths full lines.
} else {
    $columns = 72;		# Assume 80 column terminal.
}

# Default values for command line parameters.
my %p = (
	 'check' => [],		# Ref to list to contain two node names.
	 'independency' => [],	# Each successive pair an independency.	
	 'independency_file' => undef, # File containing independency pairs.
	 # Ref to list of nodes to match for loosely.
	 'loose_match' => [],  
	 # File containing node names to match for loosely.
	 'loose_match_file' => undef,
	 'package' => undef,	# Package name to compute direct deps for.
	 # Node name to compute reverse deps for. 
	 'reverse_dependencies' => undef,
	 'scan_comments' => 0,	# Flag true if comment text to be scanned.
	 # Ref to list of node names explicitly declared to exist.
	 'with_node' => [],
	 # File containing node names explicitly declared to exist.
	 'with_node_file' => undef,

	 # Standard options. 
	 'verbose' => 0,	# Verbose mode on.
	 'version' => 0,	# If given print version and exit.
	 'help'	   => 0		# If given print help and exit.
	 );

sub babble { if ( $p{verbose} ) { print @_; } }

GetOptions(\%p, 'check|c=s', 'package|p=s', 'independency|i=s',
	   'independency_file=s', 'loose_match|l=s',
	   'loose_match_file=s', 'reverse_dependencies|r=s',
	   'scan_comments|s', 'with_node|w=s', 'with_node_file=s',
	   'verbose|v', 'version', 'help|?')
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
my %node_independencies;	# Explicit independency graph (hash of lists).
if ( @{$p{'independency'}} ) {
    if ( @{$p{'independency'}} ) { 
	@{$p{'independency'}} = split(/,/, join(',', @{$p{'independency'}}));
	if ( @{$p{'independency'}} % 2 != 0 ) {
	    pod2usage("$progname: malformed independency (-i) option argument.\n");
        }
    }
    for ( my $i = 0 ; $i < @{$p{'independency'}} ; $i += 2 ) {
	$node_independencies{${$p{'independency'}}[$i]} ||= [];
        push(@{$node_independencies{${$p{'independency'}}[$i]}},
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
            $line_number++;
	    next;
	}
        unless ( (my @new_independency = split(/,/)) == 2 ) {
            die "$progname: malformed independency at line $line_number of file $p{'independency_file'}\n";
        } elsif ( $new_independency[0] =~ m/^(\s.*)|(.*\s)$/
                   or $new_independency[1] =~ m/^(\s.*)|(.*\s)$/ ) {
            die "$progname: malformed independency at line $line_number of file $p{'independency_file'}, maybe illegal whitespace around comma?\n";
        } else {
            $node_independencies{$new_independency[0]} ||= [];
            push(@{$node_independencies{$new_independency[0]}}, 
                 $new_independency[1]);
	    $line_number++;
        }
    }
    close(INDEPENDENCY_FILE) 
        or die "couldn't close $p{'independency_file'}: $!";
}

# List of nodes to be forced to exist, even if there is no
# corresponding package directory with this name.
my @with_nodes = @{$p{'with_node'}};		
# Hash with elements for nodes to be explicitly included.  I use this
# trick as a resonably fast lazy way to search lists when I don't
# actualy want to see the list item, just know if it is there, and
# using twice the memory is not a problem.  Yes I am evil.
my %with_nodes;	
foreach ( @with_nodes ) {
    $with_nodes{$_} = 1;
}

# Parse with_node_file option argument.
if ( $p{'with_node_file'} ) {
    open(WITH_NODE_FILE, "<$p{'with_node_file'}")
	or die "$progname: couldn't open $p{'with_node_file'} for reading: $!\n";
    my $line_number = 1;	# Line number being parsed.
    while ( <WITH_NODE_FILE> ) {
	chomp;			# Strip newline.
	s/#.*$//;		# Strip comments.
	s/^\s*//;		# Strip leading white space.
	s/\s*$//;		# Strip trailing white space.
	# Skip blank lines.
	unless ( $_ ) {
            $line_number++;
	    next;
	}
        unless ( exists($with_nodes{$_}) ) {
            push (@with_nodes, $_);
            $with_nodes{$_} = 1;
        }
        $line_number++;
    }
    close(WITH_NODE_FILE) 
        or die "couldn't close $p{'with_node_file'}: $!";
}

# List of nodes which are to be matched for loosely.
my @loose_match_nodes = @{$p{'loose_match'}};		

# Parse loose_match_file option argument.
if ( $p{'loose_match_file'} ) {
    open(LOOSE_MATCH_FILE, "<$p{'loose_match_file'}")
	or die "$progname: couldn't open $p{'loose_match_file'} for reading: $!\n";
    my $line_number = 1;	# Line number being parsed.
    while ( <LOOSE_MATCH_FILE> ) {
	chomp;			# Strip newline.
	s/#.*$//;		# Strip comments.
	s/^\s*//;		# Strip leading white space.
	s/\s*$//;		# Strip trailing white space.
	# Skip blank lines.
	unless ( $_ ) {
            $line_number++;
	    next;
	}
        push(@loose_match_nodes, $_);
        $line_number++;
    }
    close(LOOSE_MATCH_FILE) 
        or die "couldn't close $p{'loose_match_file'}: $!";
}

# Hash with elements for nodes to be matched for loosely.  I use this
# trick as a resonably fast lazy way to search lists when I don't
# actualy want to see the list item, just know if it is there, and
# using twice the memory is not a problem.  Yes I am evil.
my %loose_match_nodes;	
foreach ( @loose_match_nodes ) {
    $loose_match_nodes{$_} = 1;
}

# Hash of package dependency list refs.
my %node_deps;

# File in which to cache the dependency graph.
my $graph_cache_file = 'dependency_graph.cache';

# Try to use dependency graph cache file.
if ( -e $graph_cache_file ) {
    my $node_deps_ref = retrieve($graph_cache_file);
    unless ( $node_deps_ref ) {
	die "$progname: failed to correctly retrieve $graph_cache_file, maybe delete it to force regeneration?\n";
    }
    print "\nNOTE: using dependency graph cache file\n'$graph_cache_file'. Delete it and rerun if its stale.\n";
    # Print warning if using the cache means we are ignoring options.
    if ( @loose_match_nodes ) {
	print wrap('', '', "\nWARNING: --loose_match type options (-l, -loose_match, or --loose_match_file) are irrelevant when using an existing dependency graph cache file.\n");
    }
    if ( $p{'scan_comments'} ) {
	print wrap('', '', "\nWARNING: --scan_comments option (-s or --scan_comments) is irrelevant when using an existing dependency graph cache file.\n");
    }
    if ( @with_nodes ) {
	print wrap('', '', "\nWARNING: --with_node type options (-w, --with_node, or --with_node_file) are irrelevant when using an existing dependency graph cache file.\n");
    }
    sleep 4;
    %node_deps = %{$node_deps_ref};

} else {
    # Regenerate dependency graph cache.

    # Unless doing just a single node, warn user about long delay.
    unless ( $p{'package'} ) {
	print "\nNo dependency graph cache found, building dependendy graph (takes\na loooonng time, like hours and hours)...\n";
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

    # Add 'directoryless' nodes explicitly declared to exist by
    # command line option to the dependency graph.
    foreach ( @with_nodes ) {
        $node_deps{$_} = [];
    }

    foreach my $dir ( @pkg_dirs ) {

	chdir "$top_level_dir/$dir";

	# Get package name without directory part.
	$dir =~ m{\/([^/]+)$};
	my $pkg_name = $1;

        # If we are computing direct dependencies for a single package
        # skip ahead unless we are looking at that package.
        if ( $p{'package'} ) {
            unless ( $pkg_name eq $p{'package'} ) {
	        next;
  	    }
        }

	&babble("\nComputing dependencies for package $pkg_name...\n");

        # Initialize reference to empty list, unless it has already
        # been initialized due to having been declared explicitly as a
        # node which exists (via one of the -w, --with_node, or
        # --with_node_file options).
        $node_deps{$pkg_name} ||= [];

        # Check for dependencies on every other node.
        my @other_nodes = (@pkg_dirs, @with_nodes);
        foreach my $other_node ( @other_nodes ) {

	    # No need to worry about nodes depending on themselves.
	    # If $other_node was explicitly declared (via -w,
	    # --with_node, or --with_node_file), it probably won't
	    # have a directory part, so we also check if $other_node
	    # matches the non-directory part of the current package
	    # path.
            if ( $dir eq $other_node or $dir =~ m/.*\/$other_node$/ ) {
	        next;
	    }
	
	    my $dep_flag = 0;	# Flag true if other node is depended on.
	
	    # Get package (node) name without directory part.
            my $other_node_name;
	    if ( $other_node =~ m{\/([^/]+)$} ) {
		$other_node_name = $1;
	    } else {
		# Node name does not contain a directory part.
		$other_node_name = $other_node;
	    }

	    &babble("  Checking for direct dependence on node $other_node_name... ");

	    # Heuristics go here.
	  
	    # Inclusion of a header file with a name corresponding to a
	    # node name is taken to mean dependence.
	    if ( !system("grep -q -I '#include[^a-zA-Z0-9]*$other_node_name"
		         ."[^a-zA-Z0-9]*' *.[ch] 2>/dev/null") ) {
	        $dep_flag = 1;
	    }

	    # Mere mention of another node in a file containing
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
		close(ENTRY) or die "close failed on $entry: $!";
		unless ( $p{'scan_comments'} ) {
		    # If we are looking at a C file...
		    if ( $entry =~ m/\.[ch]$/ ) {
			# then strip C style comments.
			$file_contents =~ s!/\*.*?\*/!!gs;
		    # If we are looking at a shell script...	
		    } elsif ( $file_contents =~ m/^.!\/.*\/(ba)?sh/ ) {
			# then strip shell style comments.
			$file_contents =~ s/#[^!][^"']*?\n/\n/gs;
		    }
		}
		# Get temp file handle and name.
		my ($tfh, $tfn) = tempfile(DIR => "/tmp", UNLINK => 1);
		print $tfh $file_contents;
		close($tfh) or die "close failed on $tfn: $!";

		# Look for references to other node in temp file.
		my $grep_for_other_node_token_command
		    = "grep -q -I '[^_[:alpha:]]$other_node_name"
       		      ."[^_[:alpha:]]' $tfn 2>/dev/null";
		my $grep_for_system_function_call_command
		    = "grep -q -I 'system *(' $tfn 2>/dev/null";
		if ( exists($loose_match_nodes{$other_node_name}) ) {
		    if ( !system($grep_for_other_node_token_command) ) {
			$dep_flag = 1;
		    }
		} else {
		    if ( !system($grep_for_other_node_token_command)
			 and ( !system($grep_for_system_function_call_command)
			       or `head -n 1 $tfn` =~ /^.!.*\/sh/ ) ) {
			$dep_flag = 1;
		    }
	        }

		# Remove temporary file.
		unlink($tfn) or die "unlink failed";
	    }

	    # If a library file with a base name corresponding to the
	    # node name is mentioned in the Makefile, it is taken as a
	    # dependency.  This protects against some libraries with
	    # inconsistently named headers.
            if ( !system("grep -q '[^_[:alpha:]]$other_node_name\.a' "
			 ."[Mm]akefile 2>/dev/null") ) {
	        $dep_flag = 1;
	    }
	
	    if ( $dep_flag ) {
	        &babble("yes.\n");
	        push(@{$node_deps{$pkg_name}}, $other_node_name);
	    } else {
	        &babble("no.\n");
	    }
	}

        # If in verbose mode, print dependency summary for this node.
        if ( $p{verbose} ) {
	    print "\n$pkg_name\n";
    	    if ( @{$node_deps{$pkg_name}} ) {
	        print "   |\n";	# Print cute little downward ASCII art line.
	    } else {
	        print "   No dependencies.\n";
	    }
	    foreach my $other_node ( @{$node_deps{$pkg_name}} ) {
	        print "   +--> $other_node\n";
	    }
        }
    }

    chdir "$top_level_dir";

    # Unless we have an incomplete graph due to use of the -p option,
    # store dependency graph in file for later use.
    unless ( $p{'package'} ) {  
        unless ( nstore(\%node_deps, $graph_cache_file) ) {
	    die "$progname: failed to correctly store dependency graph in $graph_cache_file\n";
	} else {
	    # Print warnings if we cache the effects of options.
	    if ( @loose_match_nodes ) {
		print wrap('', '', "\nWARNING: the effects of --loose_match type options (-l, -loose_match, or --loose_match_file) have been cached in the dependency graph cache file.\n");
	    }
	    if ( $p{'scan_comments'} ) {
		print wrap('', '', "\nWARNING: the effects of the scan comments option (-s or --scan_comments) have been cached in the dependency graph cache file.\n");
	    }
	    if ( @with_nodes ) {
		print wrap('', '', "\nWARNING: the effects of 'with_node' type options (-w, --with_node, or --with_node_file) have been cached in the dependency graph cache file.\n");
	    }
	    print wrap('', '', "\nGenerated dependency graph cache file\n'$graph_cache_file'.\n");
	}
    }
}

# Validate and apply independencies.
if ( %node_independencies ) {
    &babble("\nValidating and applying independencies...\n");
    foreach my $node ( keys %node_independencies ) {
        # If in single package  mode...
        if ( $p{'package'} ) {
	    # Don't try to apply independencies except to the package
	    # being dealt with.
	    unless ( $node eq $p{'package'} ) {
		next;
	    }
	}
	unless ( exists($node_deps{$node}) ) {
	    die "$progname: unknown node name '$node' in independency\n";
	}
        foreach my $other_node ( @{$node_independencies{$node}} ) {
	    unless ( $p{'package'} or exists($node_deps{$other_node}) ) {
                die "$progname: unknown node name '$other_node' in independency\n";
            }
            my @new_deps = (); # Post-independency dependency list.
            foreach my $dependency ( @{$node_deps{$node}} ) {
                unless ( $dependency eq $other_node ) { 
                    push(@new_deps, $dependency);
                } else {
		    print wrap('', '', "\nRemoving detected direct dependency of $node on $dependency from dependency graph due to declared independency...\n");
		}
            }
            @{$node_deps{$node}} = @new_deps;
        }
    }
}


unless ( @{$p{'check'}} or $p{'reverse_dependencies'} ) {
    # Generate official (not verbose) output.

    foreach my $node ( keys %node_deps ) {
	# If we are computing direct dependencies for a single package
	# skip ahead unless we are looking at that package.
	if ( $p{'package'} ) {
	    unless ( $node eq $p{'package'} ) {
		next;
	    }
	}
	print "\n$node\n";
	if ( @{$node_deps{$node}} ) {
	    print "   |\n";	# Print cute little downward ASCII art line.
	} else {
	    print "   No dependencies.\n";
	}
	foreach my $other_node ( @{$node_deps{$node}} ) {
	    print "   +--> $other_node\n";
	}
    }

}

if ( @{$p{'check'}} ) {			
# Check output requested, examine dependency graph for dependency paths.

    # Verify that check arguments are known node names.
    unless ( exists($node_deps{${$p{'check'}}[0]}) ) {
        die wrap('', '', "$progname: check argument ${$p{'check'}}[0] is not a valid node name\n");
    }
    unless ( exists($node_deps{${$p{'check'}}[1]}) ) {
        die wrap('', '', "$progname: check argument ${$p{'check'}}[1] is not a valid node name\n");
    }

    &babble("\nLooking for all dependencies (direct and indirect) of \n"
            ."${$p{'check'}}[0] on ${$p{'check'}}[1]...\n");

    my $paths_buf = "";		# Accumulates dependency paths.
    my %visited;		# Has entries for nodes visited.

# Subroutine to do recursive depth first search for depended on node.
sub path_search
{
    # Arguments are name of current node, and path from starting node
    # to current node.
    my ($node, $path) = @_;

    $visited{$node} = 1;	# Set visited flag for node to true.

    foreach my $adjacent_node ( @{$node_deps{$node}} ) {
	if ( $visited{$adjacent_node} ) {
	    next;
	}
	if ( $adjacent_node eq ${$p{'check'}}[1] ) {
	    $paths_buf .= "$path --> ${$p{'check'}}[1]\n";
	    next;
	}
	&path_search($adjacent_node,
		     $path." --> ".$adjacent_node);
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

    # Verify that reverse_dependencies argument is a known node name.
    unless ( exists($node_deps{$p{'reverse_dependencies'}}) ) {
        die wrap('', '', "$progname: --reverse_dependencies option argument ${$p{'check'}}[0] is not a valid node name\n");
    }


    &babble("\nLooking for all dependencies (direct and indirect) on "
	    ."$p{'reverse_dependencies'}...\n");

    # Construct reverse dependency graph from dependency graph.
    my %reverse_deps;
    foreach my $node ( keys %node_deps ) {
	foreach my $other_node ( @{$node_deps{$node}} ) {
            $reverse_deps{$other_node} ||= [];
            push(@{$reverse_deps{$other_node}}, $node);
        }
    }

    my %visited;		# Has entries for nodes visited.

# Routine to recursively traverse component connected to node.
sub depth_first_traversal
{    
    # Arguments are name of current node.
    my ($node) = @_;

    $visited{$node} = 1;	# Set visited flag for not to true.

    foreach my $adjacent_node ( @{$reverse_deps{$node}} ) {
	if ( $visited{$adjacent_node} ) {
	    next;
	}
	&depth_first_traversal($adjacent_node);
    }
}

    &depth_first_traversal($p{'reverse_dependencies'});
    
    print wrap('', '', "\nNodes with direct dependencies on $p{'reverse_dependencies'}:\n");

    # If we have no direct dependencies...
    unless ( @{$reverse_deps{$p{'reverse_dependencies'}}} ) {
	# print special message,
        print "  (none)\n";
    } else {
	# otherwise, print the indirect dependencies.
	foreach my $node ( @{$reverse_deps{$p{'reverse_dependencies'}}} ) {
	    # So we don't hear about node again under indirect
	    # dependencies...
	    delete($visited{$node});
	    print "  $node\n";
	}
    }

    # We don't generally think of nodes indirectly depending on
    # themselves.
    delete($visited{$p{'reverse_dependencies'}});

    print wrap('', '', "\nNodes with indirect dependencies on $p{'reverse_dependencies'}:\n");

    # If we have no indirect dependencies...
    unless ( keys %visited ) {
	# print special message,
        print "  (none)\n";
    } else {
	# otherwise, print the indirect dependencies.
	foreach my $node ( keys %visited ) {
	    print "  $node\n";
	}
    }
}

print "\n";

=head1 AUTHOR

Britton Kerin <bkerin@mail1.asf.alaska.edu>

=cut

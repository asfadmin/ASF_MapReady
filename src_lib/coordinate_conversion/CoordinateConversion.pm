package CoordinateConversion;

# EXTERNAL ASSOCIATES:
#    NAME:               USAGE:
#    ---------------------------------------------------------------
#

use Exporter ();
@ISA = qw(Exporter);

@EXPORT = qw(earth_radius_at_geodetic_lat geocentric2geodetic
             geodetic2geocentric llr2xyz xyz2llr xyz2sch sch2xyz
             line_sample2sc sc2line_sample);

use strict;

# Standard libraries (they ship with perl).
use POSIX qw(ceil floor);

# Add on libraries from CPAN
use Params::Validate;		# Better function argument checking.
# FIXME: I'd rather have Math::Trig::sec imported, but there doesn't
# seem to be a way to get PDL to play nicely and not try to import the
# sec symbol, so for now we get the PDL version, which does I don't
# know what.
use Math::Trig qw(!sec :radial);	# Trig routines.
use PDL;			# The perl data language.
use PDL::Slatec;		# SLATEC matrix routines for PDL.

=head1 NAME

ASF::CoordinateConversion;

=head1 DESCRIPTION

This is a library of routines useful for converting between different
radar-related coordinate systems.  Units are degrees or SI base units
unless otherwise noted.

=head1 FUNCTIONS

The functions exported by this module are:

=over 4

=item $radius = B<earth_radius_at_geodetic_lat>($geodetic_lat_degrees);

Compute earth radius at given geodetic latitude using ellipsoid model.
See 'Calculation of ASF CEOS Metadata Values', Tom Logan and Rudiger
Gens, Appendix C.

=cut

sub earth_radius_at_geodetic_lat {
    my ($geodetic_lat) = validate_pos(@_, 1);

    # Convert degrees to radians.
    my $geodetic_latr = deg2rad($geodetic_lat);

    # Equivalent geocentric latitude.
    my $geocentric_latr = &geodetic2geocentric($geodetic_latr);

    # Ellipsoidal earth facts.
    my $re = 6378100;   # Equatorial earth radius.
    my $rp = 6356800;   # Polar earth radius.

    return ($re * $rp) / (sqrt(($rp * cos($geocentric_latr))**2
			       + ($re * sin($geocentric_latr))**2));
}

# Shorthand function for division by pi over two.
sub divpo2 { return shift() / (pi() / 2); }

=item $geodetic_lat_radians = B<geocentric2geodetic>($geocentric_lat_radians);

Convert given geocentric latitude in radians to geodetic latitude in
radians using WGS84 ellipsoid earth model.  See I<Calculation of ASF
CEOS Metadata Values>, Tom Logan and Rudiger Gens, Appendix C.

=cut

sub geocentric2geodetic {
    my ($geocentric_lat) = validate_pos(@_, 1);

    my $e2 = 0.00669437999014;   # Ellipticity of the WGS84 earth squared.

    # Handle potential infinite returns.
    my $tmp = &divpo2($geocentric_lat) - floor(&divpo2($geocentric_lat));
    if ( $tmp < 0.00001 or $tmp > 0.99999 ) {
	return $geocentric_lat;
    }

    # FIXME: Bleck this is awful.  But I can't figure out the better
    # way to do it if there is one.  
    my $start_quadrant = ceil(&divpo2($geocentric_lat));
    my $result = atan(tan($geocentric_lat) / (1 - $e2));
    my $end_quadrant = ceil(&divpo2($result));
    # Eccentricity is small and shouldn't change the angle much, so we
    # compare against 1.95 to check if we have jumped a couple
    # quadrands (not 2.0 to avoid potential problems with floating
    # point accuracy).
    if ( abs($end_quadrant - $start_quadrant) > 1.95 ) {
	if ( $result >= 0 ) { $result -= pi(); }
	elsif ( $result < 0 ) { $result += pi(); }
    }
	
    return $result;
}

=item $geocentic_lat_radians = B<geodetic2geocentric>($geodetic_lat_radians);

Convert given geodetic latitude in radians to geocentric latitude in
radians using WGS84 ellipsoid earth model.  See I<Calculation of ASF
CEOS Metadata Values>, Tom Logan and Rudiger Gens, Appendix C.

=cut

sub geodetic2geocentric {
    my ($geodetic_lat) = validate_pos(@_, 1);

    my $e2 = 0.00669437999014;   # Ellipticity of the WGS84 earth squared.

    # Handle potential infinite returns.
    my $tmp = &divpo2($geodetic_lat) - floor(&divpo2($geodetic_lat));
    if ( $tmp < 0.00001 or $tmp > 0.99999 ) {
	return $geodetic_lat;
    }

    # FIXME: Bleck this is awful.  But I can't figure out the better
    # way to do it if there is one.  
    my $start_quadrant = ceil(&divpo2($geodetic_lat));
    my $result = atan(tan($geodetic_lat) * (1 - $e2));
    my $end_quadrant = ceil(&divpo2($result));
    # Eccentricity is small and shouldn't change the angle much, so we
    # compare against 1.95 to check if we have jumped a couple
    # quadrands (not 2.0 to avoid potential problems with floating
    # point accuracy).
    if ( abs($end_quadrant - $start_quadrant) > 1.95 ) {
	if ( $result >= 0 ) { $result -= pi(); }
	elsif ( $result < 0 ) { $result += pi(); }
    }
	
    return $result;
}

=item ($x, $y, $z) = B<llr2xyz>($lat, $lon, $radius)

Given geodetic latitude and longitude in degrees, and geocentric
radius, return global cartesian coordinates.

=cut

sub llr2xyz {
    my ($lat, $lon, $r) = validate_pos(@_, (1) x 3);

    my $latr = deg2rad($lat);
    my $lonr = deg2rad($lon);

    my $geocentric_latr = &geodetic2geocentric($latr);

    my $x = $r * cos($geocentric_latr) * cos($lonr);
    my $y = $r * cos($geocentric_latr) * sin($lonr);
    my $z = $r * sin($geocentric_latr);

    return ($x, $y, $z);
}

=item ($lat, $lon, $radius) = B<xyz2llr>($x, $y, $z)

Given global cartesian coordinates in meters, return geodetic
latitude and longitude in degrees, and geocentric radius in meters.

=cut

sub xyz2llr {
    my ($x, $y, $z) = validate_pos(@_, (1) x 3);

    # Return values in degrees, not normalized to 0 to 2*pi (that's
    # what the 1 arguments to rad2deg mean to not do).
    return (rad2deg(&geocentric2geodetic(atan($z/sqrt($x**2 + $y**2))), 1), 
	    rad2deg(atan2($y, $x), 1), 
	    sqrt($x**2 + $y**2 + $z**2));
}

# Convenience functions to compute values used to translate between
# sch and global cartesian coordinates.  These values are used in both
# the forward and reverse transform functions.  The function names are
# taken from the variable names in 'Geocoding of AIRSAR/TOPSAR SAR
# Data', Holecz et al., see that paper for the meaning of all this.
{
    # Earth model facts (in closure for convenience functions in this block). 
    my $a = 6378137;		# semi-major axis length of WGS84 ellipsoid
    my $e2 = 0.00669437999015;	# ellipticity of WGS84 ellipsoid

    # East radius of curvature at the peg point.
    sub re {
	my ($pp_latitude) = validate_pos(@_, 1);
	
	return $a / sqrt(1 - $e2 * (sin(deg2rad($pp_latitude)))**2);
    }

    # North radius of curvature at the peg point.
    sub rn {
	my ($pp_latitude) = validate_pos(@_, 1);

        return ($a * (1 - $e2)) 
	       / (1 - $e2 * (sin(deg2rad($pp_latitude)))**2)**(3/2);
    }

    # Along track radius of curvature at the peg point.
    sub ra {
	my ($pp_lat, $pp_head) = validate_pos(@_, (1) x 2);

	my $re = &re($pp_lat);
	my $rn = &rn($pp_lat);
	my $headr = deg2rad($pp_head);

	return  $re * $rn / ($re * (cos($headr))**2 + $rn * (sin($headr))**2);
    }

    # Transformation matrices.

    sub M1 {
	my ($pp_lat, $pp_lon) = validate_pos(@_, (1) x 2);
 
	my $latr = deg2rad($pp_lat);
	my $lonr = deg2rad($pp_lon);

        return pdl 
	    [[-sin($lonr) , -sin($latr)*cos($lonr) , cos($latr)*cos($lonr)],
	     [ cos($lonr) , -sin($latr)*sin($lonr) , cos($latr)*sin($lonr)],
	     [     0      ,       cos($latr)       ,      sin($latr)      ]];
    }

    sub M2 {
	my ($pp_head) = validate_pos(@_, 1);
	
	my $headr = deg2rad($pp_head);

	return pdl [[ 0 , sin($headr) , -cos($headr) ],
		    [ 0 , cos($headr) ,  sin($headr) ],
		    [ 1 ,     0       ,      0       ]]; 
    }
     

    # Translation vector.
    sub O {
	my ($pp_lat, $pp_lon, $pp_head) = validate_pos(@_, (1) x 3);

	my $re = &re($pp_lat);
	my $ra = &ra($pp_lat, $pp_head);
	my $latr = deg2rad($pp_lat);
	my $lonr = deg2rad($pp_lon);

	return pdl [[ $re*cos($latr)*cos($lonr) - $ra*cos($latr)*cos($lonr) ],
		    [ $re*cos($latr)*sin($lonr) - $ra*cos($latr)*sin($lonr) ],
		    [        $re*(1-$e2)*sin($latr) - $ra*sin($latr)        ]];
    }
}

=item ($x, $y, $z) = B<sch2xyz>($s, $c, $h, $pp_lat, $pp_lon, $pp_head);

Given a set of sch coordinates and a peg point and aircraft heading,
return the corresponding global cartesian coordinates.  See 'Geocoding
of AIRSAR/TOPSAR SAR Data', Holecz et al.

=cut

sub sch2xyz {
    my ($s, $c, $h_r, $pp_lat, $pp_lon, $pp_head) = validate_pos(@_, (1) x 6);

    # Along track earth radius of curvature at peg point in heading direction.
    my $ra = &ra($pp_lat, $pp_head);
    my $c_phi = $c/$ra;		# Normalized cross track arc length.
    my $s_lambda = $s/$ra;	# Normalized along track arc length.

    # Transformation matrices.
    my $M1 = &M1($pp_lat, $pp_lon);
    my $M2 = &M2($pp_head);

    # Holecz paper isn't exactly a full explanation of things...
    my $Unnamed_matrix = pdl [[ ($ra + $h_r)*cos($c_phi)*cos($s_lambda) ],
			      [ ($ra + $h_r)*cos($c_phi)*sin($s_lambda) ],
			      [        ($ra + $h_r)*sin($c_phi)         ]];

    # Translation vector.
    my $O = &O($pp_lat, $pp_lon, $pp_head);    
    
    my $XYZ = $M1 x $M2 x $Unnamed_matrix + $O;

    return ($XYZ->at(0,0), $XYZ->at(0,1), $XYZ->at(0,2));
}

=item ($s, $c, $h_r) = B<xyz2sch>($x, $y, $z, $pp_lat, $pp_lon, $pp_head);

Given a global cartesian coordinate and an sch peg point and aircraft
heading, return the sch coordinates corresponding to the given
cartesian coordinate.  See 'Geocoding of AIRSAR/TOPSAR SAR Data',
Holecz et al.

=cut

sub xyz2sch {
    my ($x, $y, $z, $pp_lat, $pp_lon, $pp_head) = validate_pos(@_, (1) x 6);
    
    # Cartesian coordinates as a column vector.
    my $XYZ = pdl [[ $x ],
		   [ $y ],
		   [ $z ]];

    # Along track earth radius of curvature at peg point in heading direction.
    my $ra = &ra($pp_lat, $pp_head);

    # Transformation matrices.
    my $M1 = &M1($pp_lat, $pp_lon);
    my $M2 = &M2($pp_head);

    # Translation vector.
    my $O = &O($pp_lat, $pp_lon, $pp_head);

    # Intermediate result, a constant vector of a system of equations
    # containing the sch coordinates as variables.
    my $sch_sys = ($M1 x $M2)->matinv x ($XYZ - $O);

    # Solve system to get back to sch coords.  This is from the column
    # vector given explicitly in the equation from the above mentioned
    # paper which contains the sch coordinates, solving the system
    # needs only algebra and the first step is getting an expression
    # for ra+hr from the third element of the column vector.
    my $s_lambda = atan($sch_sys->at(0,1) / $sch_sys->at(0,0));
    my $c_phi = atan($sch_sys->at(0,2)*cos($s_lambda) / $sch_sys->at(0,0));
    my $h_r = $sch_sys->at(0,2)/sin($c_phi) - $ra;

    my $s = $s_lambda * $ra;
    my $c = $c_phi * $ra;

    return ($s, $c, $h_r);
}

=item ($s, $c) = B<line_sample2sc>($line, $sample, $azimuth_pixel_size,
 $range_pixel_size, $azimuth_offset, $range_offset);

Given 1-based line and sample coordinates of a pixel, azimuth and
range pixel sizes, and azimuth and range offsets from the peg point to
the first image pixel, (which point in the first image pixel the
offset refers to is unclear), returns the s and c coordinates of the
pixel.  

=cut

sub line_sample2sc {
    my ($line, $sample, $azimuth_pixel_size, $range_pixel_size, 
	$azimuth_offset, $range_offset) 
	= validate_pos(@_, 
		       ({ callbacks 
			      => {'is (at least almost) whole number' 
				 => 
        => sub {    $_[0] - floor($_[0]) < 0.000001
		 or $_[0] - floor($_[0]) > 0.999999; 
	   }
			         }
		        }) x 2,
		       (1) x 4);
    
    return ($line * $azimuth_pixel_size + $azimuth_offset,
	    $sample * $range_pixel_size + $range_offset);
}

=item ($line, $sample) = B<sc2line_sample>($s, $c, $azimuth_pixel_size,
 $range_pixel_size, $azimuth_offset, $range_offset);

Given s and c from a set of sch coordinates, azimuth and range pixel
sizes, and azimuth and range offsets from the peg point to the first
image pixel in meters (which point in the first image pixel the offset
refers to is unclear), returns the 1-based line and sample coodinates
of the pixel at the given coordinates.

=cut

sub sc2line_sample {
    my ($s, $c, $azimuth_pixel_size, $range_pixel_size, $azimuth_offset,
	        $range_offset) = validate_pos(@_, (1) x 6);

    # Compute rounded line and sample.
    my $line = floor(($s - $azimuth_offset) / $azimuth_pixel_size + 0.5);
    my $sample = floor(($c - $range_offset) / $range_pixel_size + 0.5);

    return ($line, $sample);
}

# Perl needs to see a true return value from this file.
"module return true";









package ASF::CoordinateConversion;

use Exporter ();
@ISA = qw(Exporter);

@EXPORT = qw(earth_radius_at_geodetic_lat geodetic2geocentric llr2xyz 
	     sch2line_sample xyz2sch);

use strict;

# FIXME: somebody install Params::Validate into the standard repository.
use lib "/home/bkerin/local/lib/site_perl";
use Params::Validate;		# Better function argument checking.

# FIXME: I'd rather have Math::Trig::sec imported, but there doesn't
# seem to be a way to get PDL to play nicely and not try to import the
# sec symbol, so for now we get the PDL version, which does I don't
# know what.
use Math::Trig qw(!sec);	# Trig routines.
use PDL;			# The perl data language.
use PDL::Slatec;		# SLATEC matrix routines for PDL.

=head1 NAME

ASF::CoordinateConversion;

=head1 DESCRIPTION

This is a library of routines useful for converting between different
radar-related coordinate systems.  Units are radians or SI base units
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
    my $geodetic_latr = &deg2rad($geodetic_lat);

    # Equivalent geocentric latitude.
    my $geocentric_latr = &geodetic2geocentric($geodetic_latr);

    # Ellipsoidal earth facts.
    my $re = 6378100;   # Equatorial earth radius.
    my $rp = 6356800;   # Polar earth radius.

    return ($re * $rp) / (sqrt(($rp * cos($geocentric_latr))**2
			       + ($re * sin($geocentric_latr))**2));
}

=item $geodetic_lat_radians = B<geodetic2geocentric>($geocentric_lat_radians);

Convert given geodetic lattitude in radians to geocentric lattitude in
radians using ellipsoid model.  See I<Calculation of ASF CEOS Metadata
Values>, Tom Logan and Rudiger Gens, Appendix C.

=cut

sub geodetic2geocentric {
    my ($geodetic_lat) = validate_pos(@_, 1);

    # Ellipsoidal earth facts.
    my $re = 6378137;   # Equatorial earth radius.
    my $rp = 6356752.3;   # Polar earth radius.
    my $e2 = 0.00669437999014;   # Ellipticity of the earth squared.

    return atan(tan($geodetic_lat) * (1 - $e2));
}

=item ($x, $y, $z) = B<llr2xyz>($lat, $lon, $radius)

Given geodetic latitude and longitude in degrees, and geocentric
radius, return global cartesian xyz.

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

=item ($s, $c, $h_r) = B<xyz2sch>($x, $y, $z, $pp_lat, $pp_lon, $pp_head);

Given a global cartesian coordinate and an sch peg point and aircraft
heading, return the sch coordinates corresponding to the given
cartesian coordinate.  The pp_lat, pp_lon, and pp_head arguments
should be in degrees.  See 'Geocoding of AIRSAR/TOPSAR SAR Data',
Holecz et al.

=cut

sub xyz2sch {
    my ($x, $y, $z, $pp_lat, $pp_lon, $pp_head) = validate_pos(@_, (1) x 6);
    
    # Peg point lattitude, longitude, and heading in radians.
    my $latr = deg2rad($pp_lat);
    my $lonr = deg2rad($pp_lon);
    my $headr = deg2rad($pp_head);

    my $a = 6378137;   # semi-major axis length of WGS84 ellipsoid
    my $e2 = 0.00669437999015;   # ellipticity of WGS84 ellipsoid

    # east radius of curvature at peg point
    my $re = $a / sqrt(1 - $e2 * (sin($latr))**2);
    # north radius of curvature at peg point
    my $rn = ($a * (1 - $e2)) / (1 - $e2 * (sin($latr))**2)**(3/2);

    # radius of curvature of approximating sphere.
    my $ra = $re * $rn / ($re * (cos($headr))**2 + $rn * (sin($headr))**2);

    # Cartesian coordinates as a column vector.
    my $XYZ = pdl [[ $x ],
		   [ $y ],
		   [ $z ]];

    # Transformation matrices.  From 'Geocoding of AIRSAR/TOPSAR SAR
    # Data', Holecz et al.

    my 
    $M1 = pdl [[-sin($lonr) , -sin($latr)*cos($lonr) , cos($latr)*cos($lonr)],
	       [ cos($lonr) , -sin($latr)*sin($lonr) , cos($latr)*sin($lonr)],
	       [     0      ,       cos($latr)       ,      sin($latr)      ]];

    my $M2 = pdl [[ 0 , sin($headr) , -cos($headr) ],
	          [ 0 , cos($headr) ,  sin($headr) ],
	          [ 1 ,     0       ,      0       ]]; 
     
    # Translation vector.  From 'Geocoding of AIRSAR/TOPSAR SAR
    # Data', Holecz et al.
    my $O = pdl [[ $re*cos($latr)*cos($lonr) - $ra*cos($latr)*cos($lonr) ],
	         [ $re*cos($latr)*sin($lonr) - $ra*cos($latr)*sin($lonr) ],
	         [        $re*(1-$e2)*sin($latr) - $ra*sin($latr)        ]];

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

=item ($line, $sample) = B<sch2line_sample>($s, $c, $h_r, $azimuth_pixel_size,
 $range_pixel_size, $azimuth_offset, $range_offset);

Given s and c from a set of sch coordinates, azimuth and range pixel
sizes, and azimuth and range offsets to the first image pixel, returns
the 1-based line and sample coodinates of the pixel at the given
coordinates.

=cut

sub sch2line_sample {
    my ($s, $c, $azimuth_pixel_size, $range_pixel_size, $azimuth_offset,
	        $range_offset) = validate_pos(@_, (1) x 6);
    
    return (($s - $azimuth_offset) / $azimuth_pixel_size, 
	    ($c - $range_offset) / $range_pixel_size);
}

# Perl needs to see a true return value from this file.
"module return true";


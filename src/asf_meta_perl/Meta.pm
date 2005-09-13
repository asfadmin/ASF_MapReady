package ASF::Meta;

# EXTERNAL ASSOCIATES:
#    NAME:               USAGE:
#    ---------------------------------------------------------------
#    asf_meta            parts of this library are wrapped for use 
#                        by other perl programs

use Carp;
use Exporter ();
@ISA = qw(Exporter);

# Currently everything is OO, so we don't need to export anything.
@EXPORT = qw();

=head1 NAME

ASF::Meta - Perl interface to the asf_meta library

=head1 DESCRIPTION

This is an (extremely incomplete) perl interface to the asf_meta
library and some related functions.  It is useful for accessing the
more useful CEOS metadata values without much pain and suffering, see
asf_meta.h and jpl_proj.h.

=head1 FUNCTIONS

So far the following functions and methods are implemented:

=over 4

=item B<< $meta_obj = ASF::Meta->new($file_name); >>

Instantiate a new $meta_obj using the metadata in $file_name.

=item B<< ($line, $samp) = $meta_obj->get_lineSamp($lat, $long, $elev); >>

Get image line and sample corresponding to given lattitude, longitude,
and elevation.

=item B<< ($lines, $samps) = $meta_obj->get_orig_img_dimensions(); >>

Get original image dimensions in lines and samples.

=item B<< $rng_pix_sz = $meta_obj->get_orig_range_pixel_size(); >>

Get original range direction pixel size in meters.

=item B<< $azi_pix_sz = $meta_obj->get_orig_azimuth_pixel_size(); >>

Get original azimuth direction pixel size in meters.

=item B<< ($c1, $c2) = $meta_obj->latlon_to_proj($lat, $long); >>

Get map projection coordinates corresponding to given lattitude and
longitude.  Only works for map projected images.

=back

=head1 HOW TO IMPLEMENT ADDITIONAL INTERFACE FUNCTIONS

If you want to add additional functions from asf_meta to this perl
interface, start with L<Inline(3pm)> and L<Inline::C(3pm)>.  Basicly, you
will need to understand somewhat of L<perlguts(1)>, but are spared the
details of XS.

The major screwy undocumented thing I ran into is that Inline somehow
manages to write code that seg faults at run time rather than giving a
link error at compile time if you forget to mention a needed library.
Also, it stupidly doesn't recompile things when you change just the
'LIBS' configure option,you have to put a space in C source somewhere
so the checksum of the C code itself will change.  Hopefully the
Inline maintainer will fix this in the future.

If you think Inline is spiffy and want to play with this module for
the sake of playing with it, have a look at L<Inline::Files(3pm)/"The
FILE and BELOW keywords">, it would be nice to use that functionality
once its mature.

Remember to keep the documentation in sync.  You can learn pod in five
minutes from L<perlpod(1)>.

=cut
    
use diagnostics;
use strict; 

use Carp;
use Inline C => Config => INC => '-I/asf_tools/src_lib/asf_inc',
                          LIBS => '-L/asf_tools/lib/irix -lasf_meta \
                                   -lasf -lm',
                          # Always include stdio.h for debug instrumentation.
                          AUTO_INCLUDE => '#include <stdio.h>',
                          DIRECTORY => '/tmp';

use Inline C => 'DATA';

# Corresponds to meta_get_lineSamp from asf_meta.h.
sub get_lineSamp {
    $#_ == 3 or croak "get_lineSamp got wrong number of arguments";
 
    my $self = shift;

    defined(my $lat = shift) or croak "get_lineSamp got undefined lattitude";
    defined(my $lon = shift) or croak "get_lineSamp got undefined longitude";
    defined(my $elev = shift) or croak "get_lineSamp got undefined elevation";

    return &c_wrap_get_lineSamp($self, $lat, $lon, $elev);
}

# Return true value from file. Perl needs this disgusting junk.
1;

__DATA__

__C__

/* Sigh... IRIX defines these in /usr/include/sys/file.h.  So to keep
   the caplib fctns included from asf_meta.h from puking we have this.
   */
#undef FOPEN
#undef FREAD
#undef FWRITE
#include "asf_meta.h"
#include "jpl_proj.h"
#define FOPEN 0xFFFFFFFF
#define FREAD           0x01
#define FWRITE          0x02

/* Instantiate a new ASF::Meta object corresponding to a
   meta_parameters structure.  */
SV *new(SV *self, char *file_name)
{
  /* Make sure we have not somehow gotten called on a weird type.  */
  if ( !sv_derived_from(self, "ASF::Meta")) {
    croak("creation routine ASF::Meta::new called on object not of type ASF::Meta");
  }

  if ( sv_isobject(self) ) { 
    /* Support questionable perl concept of reinstantiatable objects.  */
    meta_free( (meta_parameters *) SvIV(SvRV(self)));
    sv_setiv(SvRV(self), (IV) meta_init(file_name));
    return self;
  } else {
    /* Create new object.  */
    char *class;
    meta_parameters *meta_parms;
    SV *obj_ref;
    SV *obj;

    class = SvPV_nolen(self);  
    meta_parms = meta_init(file_name);
    obj_ref = newSViv(0);
    obj = newSVrv(obj_ref, class);
    free(class);

    sv_setiv(obj, (IV) meta_parms);
    /* Was here: from Inline::C-Cookbook but I cant find docs:
       'SvREADONLY_on(obj);'.  Don't think I like it anyway cause it
       looks like it'll cause problems for reinstantiation.  */

    return obj_ref;
  }
}

/* C wrapper to meta_get_lineSamp from asf_meta.h.  There is a bit of
   perl code around this function to check the arguments somewhat.  */
void c_wrap_get_lineSamp(SV *obj_ref, double lat, double lon, double elev)
{
  double yLine, xSample;	/* Hold return values. */
  INLINE_STACK_VARS;		/* Inline macro for perl stack vars.  */

  meta_get_lineSamp( (meta_parameters *) SvIV(SvRV(obj_ref)), 
		     lat, lon, elev, &yLine, &xSample);

  /* Put return values on perl stack.  */
  INLINE_STACK_RESET;
  /* FIXME: why does Inline::C-Cookbook not use sv_2mortal or The like
  on these return items as is done in perlxs man page?  I do not see
  it happening under the hood in compiled code either.  */
  INLINE_STACK_PUSH(newSVnv(yLine));
  INLINE_STACK_PUSH(newSVnv(xSample));
  INLINE_STACK_DONE;

  INLINE_STACK_RETURN(2);
}

/* Corresponds to meta_get_orig_img_dimensions from asf_meta.h.  */
void get_orig_img_dimensions(SV *obj_ref)
{
  long lines, samples;   /* Hold return values. */
  INLINE_STACK_VARS;	 /* Inline macro for perl stack vars.  */

  meta_get_img_dimensions( (meta_parameters *) SvIV(SvRV(obj_ref)), 
			   &lines, &samples);

  /* Put return values on perl stack.  */
  INLINE_STACK_RESET;
  /* FIXME: why does Inline::C-Cookbook not use sv_2mortal or the like
  on these return items as is done in perlxs man page?  I do not see
  it happening in under the hood in compiled code either.  */
  INLINE_STACK_PUSH(newSViv(lines));
  INLINE_STACK_PUSH(newSViv(samples));
  INLINE_STACK_DONE;

  INLINE_STACK_RETURN(2); 
} 

/* There appears to be no interface fctn for returning the pixel sizes
   in asf_meta.h.  These fctns return the original pixel sizes in
   meters.  */
double get_orig_range_pixel_size(SV *obj_ref)
{
  return ((meta_parameters *) SvIV(SvRV(obj_ref)))->geo->xPix;
}
double get_orig_azimuth_pixel_size(SV *obj_ref)
{
  return ((meta_parameters *) SvIV(SvRV(obj_ref)))->geo->yPix;
}

void latlon_to_proj(SV *obj_ref, double lat, double lon)
{
  double p1, p2;		/* Hold return values.  */
  INLINE_STACK_VARS;

  ll_to_proj(( (meta_parameters *) SvIV(SvRV(obj_ref)))->geo, lat, lon,
	     &p1, &p2);

  INLINE_STACK_RESET;
  INLINE_STACK_PUSH(newSVnv(p1));
  INLINE_STACK_PUSH(newSVnv(p2));
  INLINE_STACK_RETURN(2);
}

/* Called automagicly when an ASF::Meta object runs out of references
   (which usually happens when the object goes out of scope).  */
void DESTROY(SV *obj_ref)
{
  meta_free( (meta_parameters *) SvIV(SvRV(obj_ref)));
}

=head1 AUTHOR

Britton Kerin (bkerin@mail1.asf.alaska.edu)

=cut

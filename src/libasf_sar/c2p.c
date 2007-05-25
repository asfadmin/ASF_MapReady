#include "asf_sar.h"
#include "asf_meta.h"
#include "asf.h"

void c2p(const char *infile, const char *outfile, int multilook, int banded)
{
    meta_parameters *in_meta = meta_read(infile);
    int data_type = in_meta->general->data_type;

    // some sanity checks
    switch (data_type) {
      case COMPLEX_BYTE:
      case COMPLEX_INTEGER16:
      case COMPLEX_INTEGER32:
      case COMPLEX_REAL32:
      case COMPLEX_REAL64:
          break;
      default:
          asfPrintError("c2p: %s is not a complex image.\n", infile);
    }

    if (in_meta->general->band_count != 1)
        asfPrintError("c2p: %s is not a single-band image.\n", infile);

    if (!in_meta->sar)
        asfPrintError("c2p: %s is missing a SAR block.\n", infile);

    asfPrintStatus("Converting complex image to amplitude/phase...\n");

    int nl = in_meta->general->line_count;
    int ns = in_meta->general->sample_count;
    // process 1 line at a time when not multilooking, otherwise grab nlooks
    int looks = multilook ? in_meta->sar->look_count : 1;

    meta_parameters *out_meta = meta_read(infile);
    out_meta->general->data_type = meta_complex2polar(data_type);
    out_meta->general->image_data_type = AMPLITUDE_IMAGE;
    out_meta->general->band_count = 2;
    strcpy(out_meta->general->bands, "AMP,PHASE");

    FILE *fin = fopenImage(infile, "rb");

    FILE *fout_banded=NULL, *fout_amp=NULL, *fout_phase=NULL;
    if (banded) {
        asfPrintStatus("Output is 2-band image: %s\n", outfile);
        fout_banded = fopenImage(outfile, "wb");
    } else {
        char *amp_name = appendToBasename(outfile, "_amp");
        char *phase_name = appendToBasename(outfile, "_phase");
        asfPrintStatus("Output amplitude file: %s\n", amp_name);
        asfPrintStatus("Output phase file: %s\n", phase_name);
        fout_amp = fopenImage(amp_name, "wb");
        fout_phase = fopenImage(phase_name, "wb");
        free(amp_name);
        free(phase_name);
    }

    complexFloat *cpx = MALLOC(sizeof(complexFloat)*ns*looks);
    float *amp = MALLOC(sizeof(float)*ns*looks);
    float *phase = MALLOC(sizeof(float)*ns*looks);

    int l,lb,s,ol=0; // lb=line in a block,  ol=output line #
    for (l=0; l<nl; l+=looks) {
        int blockSize = get_complexFloat_lines(fin,in_meta,l,looks,cpx);
        if (blockSize != looks*ns)
            asfPrintError("What the.. !!?? %d %d %d\n", blockSize, looks, ns);

        // first, compute the power
        for (lb=0; lb<looks; ++lb) {
            for (s=0; s<ns; ++s) {
                int k = lb*ns + s;
                float r = cpx[k].real;
                float i = cpx[k].imag;
                if (r != 0.0 || i != 0.0) {
                    amp[k] = r*r + i*i;
                    phase[k] = atan2(i,r);
                } else {
                    amp[k] = phase[k] = 0.0;
                }
            }
        }

        // now multilook, if requested
        if (multilook) {
            // put the multilooked data in the first "row" of amp,phase
            for (s=0; s<ns; ++s) {
                float value = 0.0;
                for (lb=0; lb<looks; ++lb)
                    value += amp[lb*ns + s];
                amp[s] = value/(float)looks;
                value = 0.0;
                for (lb=0; lb<looks; ++lb)
                    value += phase[lb*ns + s];
                phase[s] = value/(float)looks;
            }
        }

        // now compute amplitude
        for (s=0; s<ns; ++s)
            amp[s] = sqrt(amp[s]);

        // write out a line (multilooked) or a bunch of lines (not multi)
        if (multilook) {
            if (banded) {
                put_band_float_line(fout_banded, out_meta, 0, ol, amp);
                put_band_float_line(fout_banded, out_meta, 1, ol, phase);
            } else {
                put_float_line(fout_amp, out_meta, ol, amp);
                put_float_line(fout_phase, out_meta, ol, phase);
            }
            ++ol;
        } else {
            for (lb=0; lb<looks; ++lb) {
                if (banded) {
                    put_band_float_line(fout_banded, out_meta, 0, l+lb, amp);
                    put_band_float_line(fout_banded, out_meta, 1, l+lb, phase);
                } else {
                    put_float_line(fout_amp, out_meta, l+lb, amp);
                    put_float_line(fout_phase, out_meta, l+lb, phase);
                }
            }
        }

        asfLineMeter(l,nl);
    }

    fclose(fin);
    if (fout_banded) fclose(fout_banded);
    if (fout_amp) fclose(fout_amp);
    if (fout_phase) fclose(fout_phase);

    if (multilook) {
        out_meta->general->line_count = ol;
        out_meta->general->y_pixel_size *= looks;
    }

    meta_write(out_meta, outfile);

    meta_free(in_meta);
    meta_free(out_meta);

    FREE(amp);
    FREE(phase);
    FREE(cpx);
}

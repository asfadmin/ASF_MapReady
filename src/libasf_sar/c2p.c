#include "asf_sar.h"
#include "asf_meta.h"
#include "asf.h"
#include <assert.h>

void c2p(const char *infile, const char *outfile, int multilook, int banded)
{
    meta_parameters *in_meta = meta_read(infile);
    int data_type = in_meta->general->data_type;
    // the old code did this, but why??
    in_meta->general->data_type = meta_polar2complex(data_type);

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
    int nlooks = multilook ? in_meta->sar->look_count : 1;

    if (nlooks == 1 && multilook) {
        asfPrintStatus("Not multilooking, look_count is 1.\n");
        multilook = FALSE;
    }

    if (multilook)
        asfPrintStatus("Multilooking with %d looks.\n", nlooks);

    meta_parameters *out_meta = meta_read(infile);
    out_meta->general->data_type = meta_complex2polar(data_type);

    // set up input/output files
    char *infile_img = appendExt(infile, ".cpx");
    if (!fileExists(infile_img))
        asfPrintError("The input file %s was not found.\n");
    FILE *fin = fopenImage(infile_img, "rb");

    // we either have 1 or 2 output files, per the "banded" flag.
    char *outfile_img = appendExt(outfile, ".img");
    char *amp_name=NULL, *phase_name=NULL;
    FILE *fout_banded=NULL, *fout_amp=NULL, *fout_phase=NULL;
    if (banded) {
        asfPrintStatus("Output is 2-band image: %s\n", outfile_img);
        fout_banded = fopenImage(outfile_img, "wb");
    } else {
        amp_name = appendToBasename(outfile_img, "_amp");
        phase_name = appendToBasename(outfile_img, "_phase");
        asfPrintStatus("Output amplitude file: %s\n", amp_name);
        asfPrintStatus("Output phase file: %s\n", phase_name);
        fout_amp = fopenImage(amp_name, "wb");
        fout_phase = fopenImage(phase_name, "wb");
    }
    if (banded)
        assert(fout_banded && !fout_amp && !fout_phase);
    else
        assert(!fout_banded && fout_amp && fout_phase);

    // get the metadata band_count correct, needed in the put_* calls
    if (banded) {
        out_meta->general->band_count = 2;
        strcpy(out_meta->general->bands, "AMP,PHASE");
    }

    // input buffer
    complexFloat *cpx = MALLOC(sizeof(complexFloat)*ns*nlooks);

    // output buffers
    float *amp = MALLOC(sizeof(float)*ns*nlooks);
    float *phase = MALLOC(sizeof(float)*ns*nlooks);

    int line_in;    // line in the input image
    int line_out=0; // line in the output image
    int samp;       // sample #, loop index
    int l;          // line loop index, iterates over the lines in the block

    for (line_in=0; line_in<nl; line_in+=nlooks)
    {
        // lc = "line count" -- how many lines to read. normally we will read
        // nlooks lines, but near eof we might have to read fewer
        int lc = nlooks; 
        if (line_in + lc > nl)
            lc = nl - line_in;

        // read "nlooks" (or possibly fewer, if near eof) lines of data
        int blockSize = get_complexFloat_lines(fin,in_meta,line_in,lc,cpx);
        if (blockSize != lc*ns)
            asfPrintError("bad blockSize: bs=%d nlooks=%d ns=%d\n", blockSize, nlooks, ns);

        // first, compute the power/phase
        for (l=0; l<lc; ++l) {
            for (samp=0; samp<ns; ++samp) {
                int k = l*ns + samp; // index into the block
                float re = cpx[k].real;
                float im = cpx[k].imag;
                if (re != 0.0 || im != 0.0) {
                    amp[k] = re*re + im*im;
                    phase[k] = atan2(im, re);
                } else {
                    amp[k] = phase[k] = 0.0;
                }
            }
        }

        // now multilook, if requested
        if (multilook) {
            // put the multilooked data in the first "row" of amp,phase
            for (samp=0; samp<ns; ++samp) {
                float value = 0.0;
                for (l=0; l<lc; ++l)
                    value += amp[l*ns + samp];
                amp[samp] = value/(float)lc;

                value = 0.0;
                for (l=0; l<lc; ++l)
                    value += phase[l*ns + samp];
                phase[samp] = value/(float)lc;
            }
        }

        // now compute amplitude from the (multilooked) power
        for (samp=0; samp<ns; ++samp)
            amp[samp] = sqrt(amp[samp]);

        // write out a line (multilooked) or a bunch of lines (not multi)
        if (multilook) {
            if (banded) {
                put_band_float_line(fout_banded, out_meta, 0, line_out, amp);
                put_band_float_line(fout_banded, out_meta, 1, line_out, phase);
            } else {
                put_float_line(fout_amp, out_meta, line_out, amp);
                put_float_line(fout_phase, out_meta, line_out, phase);
            }
            ++line_out;
        } else {
            for (l=0; l<lc; ++l) {
                if (banded) {
                    put_band_float_line(fout_banded, out_meta, 0, line_in+l, amp);
                    put_band_float_line(fout_banded, out_meta, 1, line_in+l, phase);
                } else {
                    put_float_line(fout_amp, out_meta, line_in+l, amp);
                    put_float_line(fout_phase, out_meta, line_in+l, phase);
                }
                ++line_out;
            }
        }

        asfPercentMeter((float)line_in/(float)(nl));
    }
    asfPercentMeter(1.0);

    fclose(fin);
    if (fout_banded) fclose(fout_banded);
    if (fout_amp) fclose(fout_amp);
    if (fout_phase) fclose(fout_phase);

    if (multilook) {
        out_meta->general->line_count = line_out;
        out_meta->general->y_pixel_size *= nlooks;
        out_meta->sar->azimuth_time_per_pixel *= nlooks;
    } else
        assert(line_out == nl);

    // write out the metadata, different whether multi-banded or not
    if (banded) {
        assert(!amp_name && !phase_name);
        meta_write(out_meta, outfile);
    } else {
        assert(amp_name && phase_name);
        
        out_meta->general->image_data_type = AMPLITUDE_IMAGE;
        meta_write(out_meta, amp_name);
        
        out_meta->general->image_data_type = PHASE_IMAGE;
        meta_write(out_meta, phase_name);
    }

    if (multilook)
        asfPrintStatus("Original line count: %d, after multilooking: %d "
            "(%d looks)\n", nl, line_out, nlooks);

    meta_free(in_meta);
    meta_free(out_meta);

    FREE(amp);
    FREE(phase);
    FREE(cpx);

    FREE(infile_img);
    FREE(outfile_img);

    FREE(amp_name);
    FREE(phase_name);
}

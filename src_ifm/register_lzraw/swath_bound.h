#ifndef SWATH_BOUND_H
#define SWATH_BOUND_H

  /*
   * Swath Bounds structure, this structure is intended for use with
   * swath_offset, water_mask and register_lzraw, so that information
   * about offsets, and the water mask can be more easily shared.
   * Water masking no longer used.
   */ 
struct swathbounds
{
	int start_offset1, end_offset1;			/* line offsets for image1  */
	int start_offset2, end_offset2;			/* line offsets for image2  */
	int masked_start_offset1, masked_end_offset1;	/* line offsets for image1, */
							/*   after water masking    */
	int masked_start_offset2, masked_end_offset2;   /* line offsets for image2, */
							/*   after water masking    */

	int total_loc1, total_loc2;		/* total location blocks for both images */
	int aisp_patches, masked_patches;	/* aisp patches for clipped images and   */
						/*    the masked images			 */
	int par_start_loc1, par_end_loc1;	/* parameter file start & end location   */
						/*    blocks in image 1.		 */
	int par_start_loc2, par_end_loc2;	/* parameter file start & end location   */
						/*    blocks in image 2.		 */
	char *parFile1, *parFile2;		/* parameter file names for both images  */
	char *metaFile1, *metaFile2;		/* meta file names for both images	 */
};
  /*
   * Definitions for read and write strucutre file functions.
   */

void make_bound_file(char *fname, struct swathbounds boundOut);
void read_bound_file(char *fname, struct swathbounds *boundOut);

#endif


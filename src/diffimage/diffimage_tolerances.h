#ifndef _DIFFIMAGE_TOLERANCES_
#define _DIFFIMAGE_TOLERANCES_

#define MIN_DIFF_TOL 0.5 // Max allowed difference, expressed as percent of 6-sigma range of baseline data
#define MAX_DIFF_TOL 0.5
#define MEAN_DIFF_TOL 0.5
#define SDEV_DIFF_TOL 0.5
// #define RMSE_DIFF_TOL 0.5 // Max allowed difference, in percent of baseline rmse, between internal data rmse's
#define PSNR_TOL 7.0 // Max absolute difference allowed for peak signal to noise ratio (range typically 0 to around 400)

#endif // _DIFFIMAGE_TOLERANCES_


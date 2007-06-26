#ifndef _DIFFIMAGE_TOLERANCES_
#define _DIFFIMAGE_TOLERANCES_

#define MIN_DIFF_TOL 0.5 // Max allowed difference, expressed as percent of 6-sigma range of baseline data
#define MAX_DIFF_TOL 0.5
#define MEAN_DIFF_TOL 0.5
#define SDEV_DIFF_TOL 0.5
// #define RMSE_DIFF_TOL 0.5 // Max allowed difference, in percent of baseline rmse, between internal data rmse's
#define BYTE_PSNR_TOL -16.0       // Minimum PSNRs v. data type
#define INTEGER16_PSNR_TOL -5.46
#define INTEGER32_PSNR_TOL 18.62
#define REAL32_PSNR_TOL 161.96

#endif // _DIFFIMAGE_TOLERANCES_


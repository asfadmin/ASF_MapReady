static char sccsid_dataset_c[] =  "@(#)dataset.c	1.9 97/04/03 09:59:18";

/* #define HI_LL_GRF /* uncomment if R1_HI_LL_GRF is valid */

#include <stdio.h>
#include "product.h"

char getFrameModeChar(char *frameModeStr)
{
  char frameMode = FRAME_MODE_DEF;

  if (frameModeStr == NULL)
    return(FRAME_MODE_DEF); /* just so we don't crash */

  frameMode = (strcmp(frameModeStr,"ARCTIC") == 0) ? FRAME_MODE_N :
              (strcmp(frameModeStr,"ANTARCTIC") == 0) ? FRAME_MODE_S :
               FRAME_MODE_DEF;
  return(frameMode);
}

/*----------------------------------------------------------
 * NAME:
 *  get_dataset
 *
 * DESCRIPTION:
 *  generate dataset name given product id
 *
 * NOTES:
 *
 *---------------------------------------------------------*/

const char* get_dataset(char* product_id, char *product_type, 
              char* mode, char *frame_mode, char *msg_type, char *compensated)
{
    char frameMode = getFrameModeChar(frame_mode);
    int isLL = (frameMode == FRAME_MODE_LL) ? 1 : 0;
    int isComp = (compensated == NULL) ? 0 : 
             (strcmp(compensated, "NO") == 0) ? 0 : 1;
    int isCal = (product_type == NULL) ? 0 : 
             (strcmp(product_type, "CAL_SET") == 0) ? 1 : 0;
    int isStd = (product_type == NULL) ? 0 : 
             (strcmp(product_type, "STANDARD") == 0) ? 1 : 0;

    static const char
    E1_STD_CCSD[]	="ERS-1 STANDARD BEAM CCSD PRODUCT",
    E1_STD_COMPLEX[]	="ERS-1 STANDARD BEAM COMPLEX PRODUCT",
    E1_STD_GRF[]	="ERS-1 STANDARD BEAM STANDARD GRF PRODUCT",
    E1_STD_UNCOMP[]	="ERS-1 STANDARD BEAM UNCOMPENSATED PRODUCT",
   *E1_STD_UNKNOWN	= NULL,

    E2_STD_CCSD[]	="ERS-2 STANDARD BEAM CCSD PRODUCT",
    E2_STD_COMPLEX[]	="ERS-2 STANDARD BEAM COMPLEX PRODUCT",
    E2_STD_GRF[]	="ERS-2 STANDARD BEAM STANDARD GRF PRODUCT",
    E2_STD_UNCOMP[]	="ERS-2 STANDARD BEAM UNCOMPENSATED PRODUCT",
   *E2_STD_UNKNOWN	= NULL,

    J1_STD_CCSD[]	="JERS-1 STANDARD BEAM CCSD PRODUCT",
    J1_STD_COMPLEX[]	="JERS-1 STANDARD BEAM COMPLEX PRODUCT",
    J1_STD_GRF[]	="JERS-1 STANDARD BEAM STANDARD GRF PRODUCT",
    J1_STD_UNCOMP[]	="JERS-1 STANDARD BEAM UNCOMPENSATED PRODUCT",
   *J1_STD_UNKNOWN	= NULL,

    R1_STD_CCSD[]	="RADARSAT-1 STANDARD BEAM CCSD PRODUCT",
    R1_STD_COMPLEX[]	="RADARSAT-1 STANDARD BEAM COMPLEX PRODUCT",
    R1_STD_GRF[]	="RADARSAT-1 STANDARD BEAM STANDARD GRF PRODUCT",
    R1_STD_UNCOMP[]	="RADARSAT-1 STANDARD BEAM UNCOMPENSATED PRODUCT",
    R1_STD_LL_GRF[]     =
                 "RADARSAT-1 STANDARD BEAM LEFT LOOKING STANDARD GRF PRODUCT",
    R1_STD_LL_RAMP[]    ="RADARSAT-1 STANDARD BEAM LEFT LOOKING RAMP PRODUCT",
   *R1_STD_UNKNOWN	= NULL,

    R1_SSR_MULTILOOK[]	="RADARSAT-1 SCANSAR MULTILOOK PRODUCT",
    R1_SSR_SINGLELOOK[]	="RADARSAT-1 SCANSAR SINGLELOOK PRODUCT",
    R1_SSR_GRF[]	="RADARSAT-1 SCANSAR STANDARD GRF PRODUCT",
    R1_SSR_GCD[]	="RADARSAT-1 SCANSAR STANDARD GCD PRODUCT",
    R1_SSR_GTC[]	="RADARSAT-1 SCANSAR STANDARD GTC PRODUCT",
   *R1_SSR_UNKNOWN	= NULL,

    R1_HI_COMPLEX[] ="RADARSAT-1 HIGH INCIDENCE BEAM COMPLEX PRODUCT",
    R1_HI_GRF[]     ="RADARSAT-1 HIGH INCIDENCE BEAM STANDARD GRF PRODUCT",
    R1_HI_UNCOMP[]  ="RADARSAT-1 HIGH INCIDENCE BEAM UNCOMPENSATED PRODUCT",
#ifdef HI_LL_GRF
    R1_HI_LL_GRF[]  ="RADARSAT-1 HIGH INCIDENCE BEAM LEFT LOOKING RAMP PRODUCT",
#endif
    R1_HI_LL_RAMP[] ="RADARSAT-1 HIGH INCIDENCE BEAM LEFT LOOKING RAMP PRODUCT",
   *R1_HI_UNKNOWN   = NULL,

    E1_SCAN_RESULTS[] = "ERS-1 SCAN RESULTS FILE",
    E2_SCAN_RESULTS[] = "ERS-2 SCAN RESULTS FILE",
    J1_SCAN_RESULTS[] = "JERS-1 SCAN RESULTS FILE",
    R1_SCAN_RESULTS[] = "RADARSAT SCAN RESULTS FILE",

   *PLATFORM_UNKNOWN	= NULL;

#ifdef BUILD_DEBUG
printf("get_dataset: productId %s mode %s comp %s frame_mode %s\n",
 product_id, mode, compensated, frame_mode);
printf("get_dataset: frameMode %c isLL %d isComp %d\n", frameMode, isLL, isComp);
#endif
   
    if (strcmp(msg_type, "SCAN_REQUEST") == 0) {
      return
      (strncmp(product_id, "E1", 2) == 0 ) ? E1_SCAN_RESULTS :
      (strncmp(product_id, "E2", 2) == 0 ) ? E2_SCAN_RESULTS :
      (strncmp(product_id, "J1", 2) == 0 ) ? J1_SCAN_RESULTS :
      (strncmp(product_id, "R1", 2) == 0 ) ? R1_SCAN_RESULTS :
      PLATFORM_UNKNOWN;
    }

/* for frame requests, both these parameters must be set */

    if (compensated == NULL || frame_mode == NULL)
      return(NULL);

    return
    (strncmp(product_id, "E1", 2) == 0 ?
	(product_id[PROC_MODE_INDEX] == PROC_MODE_CCSD ? E1_STD_CCSD :
        (product_id[PROC_MODE_INDEX] == PROC_MODE_CX ? E1_STD_COMPLEX :
        (product_id[PROC_MODE_INDEX] == PROC_MODE_UNCOMP ? E1_STD_UNCOMP :
        (product_id[PROJECTION_INDEX] == PROJECTION_GROUND ? E1_STD_GRF :
        (product_id[PROJECTION_INDEX] == PROJECTION_STD ? E1_STD_GRF : E1_STD_UNKNOWN))))) :
    (strncmp(product_id, "E2", 2) == 0 ?
	(product_id[PROC_MODE_INDEX] == PROC_MODE_CCSD ? E2_STD_CCSD :
        (product_id[PROC_MODE_INDEX] == PROC_MODE_CX ? E2_STD_COMPLEX :
        (product_id[PROC_MODE_INDEX] == PROC_MODE_UNCOMP ? E2_STD_UNCOMP :
        (product_id[PROJECTION_INDEX] == PROJECTION_GROUND ? E2_STD_GRF :
        (product_id[PROJECTION_INDEX] == PROJECTION_STD ? E2_STD_GRF : E2_STD_UNKNOWN))))) :
    (strncmp(product_id, "J1", 2) == 0 ?
	(product_id[PROC_MODE_INDEX] == PROC_MODE_CCSD ? J1_STD_CCSD :
        (product_id[PROC_MODE_INDEX] == PROC_MODE_CX ? J1_STD_COMPLEX :
        (product_id[PROC_MODE_INDEX] == PROC_MODE_UNCOMP ? J1_STD_UNCOMP :
        (product_id[PROJECTION_INDEX] == PROJECTION_GROUND ? J1_STD_GRF :
        (product_id[PROJECTION_INDEX] == PROJECTION_STD ? J1_STD_GRF : J1_STD_UNKNOWN))))) :

    (strncmp(product_id, "R1", 2) == 0 ?

        ((strncmp(mode, "ST", 2) == 0) ?
            ((product_id[PROC_MODE_INDEX] == PROC_MODE_CCSD ? R1_STD_CCSD :
             (product_id[PROC_MODE_INDEX] == PROC_MODE_CX ? R1_STD_COMPLEX :
             (product_id[PROC_MODE_INDEX] == PROC_MODE_UNCOMP ? R1_STD_UNCOMP :
             ((product_id[PROC_MODE_INDEX] == PROC_MODE_RAMP &&  isLL) ?
                            R1_STD_LL_RAMP :
             ((product_id[PROJECTION_INDEX] == PROJECTION_GROUND && isLL) ? 
                            R1_STD_LL_GRF : 
             ((product_id[PROJECTION_INDEX] == PROJECTION_STD && isLL) ?
                            R1_STD_LL_GRF : 
             ((product_id[PROJECTION_INDEX] == PROJECTION_STD ||
               product_id[PROJECTION_INDEX] == PROJECTION_GROUND) ? R1_STD_GRF :
                     R1_STD_UNKNOWN))))))))

        :   /* Non Radarsat Continuous Standard */

        ((strncmp(mode, "EH", 2) == 0) ?

            ((product_id[PROC_MODE_INDEX] == PROC_MODE_CX ? R1_HI_COMPLEX :
             (product_id[PROC_MODE_INDEX] == PROC_MODE_UNCOMP ? R1_HI_UNCOMP :
            ((product_id[PROC_MODE_INDEX] == PROC_MODE_RAMP && isLL) ?
                            R1_HI_LL_RAMP :
            ((product_id[PROJECTION_INDEX] == PROJECTION_STD ||
              product_id[PROJECTION_INDEX] == PROJECTION_GROUND) ? R1_HI_GRF :
            ((product_id[PROJECTION_INDEX] == PROJECTION_GROUND && isLL) ?
#ifdef HI_LL_GRF
                            R1_HI_LL_GRF :
#else
                            R1_HI_GRF :
#endif
            ((product_id[PROJECTION_INDEX] == PROJECTION_STD && isLL) ?
#ifdef HI_LL_GRF
                            R1_HI_LL_GRF :
#else
                            R1_HI_GRF :
#endif
                     R1_HI_UNKNOWN)))))))

        :   /* Non High Incidence : ScanSar*/

        (isCal ?
          ((product_id[PROC_MODE_INDEX] == CAL_DIGIT_LETTER ? R1_SSR_SINGLELOOK:
           (product_id[PROC_MODE_INDEX] == PROC_MODE_STD    ||
            product_id[PROC_MODE_INDEX] == PROC_MODE_CCSD   ||
            product_id[PROC_MODE_INDEX] == PROC_MODE_CX     ||
            product_id[PROC_MODE_INDEX] == PROC_MODE_QL     ||
            product_id[PROC_MODE_INDEX] == PROC_MODE_USER   ||
            product_id[PROC_MODE_INDEX] == PROC_MODE_UNCOMP ||
            product_id[PROC_MODE_INDEX] == PROC_MODE_RAMP ? R1_SSR_MULTILOOK :
                     R1_SSR_UNKNOWN))) 

        :
        (isStd ?
	  (isComp ?
            ((product_id[PROJECTION_INDEX] == PROJECTION_UTM ? R1_SSR_GCD :
             (product_id[PROJECTION_INDEX] == PROJECTION_PS ? R1_SSR_GCD :
             (product_id[PROJECTION_INDEX] == PROJECTION_LAMBERT ? R1_SSR_GCD :
             (product_id[PROJECTION_INDEX] == PROJECTION_GROUND ? R1_SSR_GRF :
             (product_id[PROJECTION_INDEX] == PROJECTION_TERR ? R1_SSR_GTC :
                     R1_SSR_UNKNOWN)))))) : PLATFORM_UNKNOWN )


    :   /* Unknown Platform */

        PLATFORM_UNKNOWN
         ) /* std */
         ) /* cal */
         ) /* EH */
         ) /* continuous */

    :   /* Unknown Platform */

         PLATFORM_UNKNOWN) /*R1*/
         ) /*J1*/
         ) /*E2*/
         ) /*E1*/
         ;
}


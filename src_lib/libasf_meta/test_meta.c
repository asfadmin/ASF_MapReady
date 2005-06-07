#include "libasf_meta.h"

#define METADATA_VERSION 1.4

int main(int argc, char *argv[])
{
  meta_parameters *meta=NULL;
  struct DDR *ddr=NULL;
  char file[255];

  printf("TESTS FOR BACKWARD COMPATIBILITY WITHIN 1.X\n\n");
  printf("Tests on 'image14.meta'\n");
  sprintf(file, "image14.meta");
  /* TEST 1: Read the lastet and greatest (1.4) in and straight out */
  printf("TEST 1: Read as 1.4, write as 1.4 ... ");
  asf_meta_read(file, "1.4", &meta, NULL);
  asf_meta_write(meta, "1.4", NULL, "test1.meta");
  printf("done\n");

  /* TEST 2: Read 1.4 and write as 1.3 */
  printf("TEST 2: Read as 1.4, write as 1.3 ... ");
  asf_meta_read(file, "1.4", &meta, NULL);
  asf_meta_write(meta, "1.3", NULL, "test2.meta");
  printf("done\n");

  /* TEST 3: Read 1.3 and write as 1.3 */
  printf("TEST 3: Read as 1.3, write as 1.3 ... ");
  asf_meta_read(file, "1.3", &meta, NULL);
  asf_meta_write(meta, "1.3", NULL, "test3.meta");
  printf("done\n");

  printf("\nTests on 'image_utm13.meta'\n");
  sprintf(file, "image_utm13.meta");
  /* TEST 4: Read 1.3 and write as 1.3 */
  printf("TEST 4: Read as 1.3, write as 1.3 ... ");
  asf_meta_read(file, "1.3", &meta, NULL);
  asf_meta_write(meta, "1.3", NULL, "test4.meta");
  printf("done\n");

  /* TEST 5: Read 1.3 and write as 1.2 */
  printf("TEST 5: Read as 1.3, write as 1.2 ... ");
  asf_meta_read(file, "1.3", &meta, NULL);
  asf_meta_write(meta, "1.2", NULL, "test5.meta");
  printf("done\n");

  /* TEST 6: Read 1.2 and write as 1.2 */
  printf("TEST 6: Read as 1.2, write as 1.2 ... ");
  asf_meta_read(file, "1.2", &meta, NULL);
  asf_meta_write(meta, "1.2", NULL, "test6.meta");
  printf("done\n");

  /* TEST 7: Read 1.3 and write as 1.1 */
  printf("TEST 7: Read as 1.3, write as 1.1 ... ");
  asf_meta_read(file, "1.3", &meta, NULL);
  asf_meta_write(meta, "1.1", NULL, "test7.meta");
  printf("done\n");

  /* TEST 8: Read 1.1 and write as 1.1 */
  printf("TEST 8: Read as 1.1, write as 1.1 ... ");
  asf_meta_read(file, "1.1", &meta, NULL);
  asf_meta_write(meta, "1.1", NULL, "test8.meta");
  printf("done\n");

  printf("\nTests on 'image_albers13.meta'\n");
  sprintf(file, "image_albers13.meta");
  /* TEST 9: Read 1.3 and write as 1.3 */
  printf("TEST 9: Read as 1.3, write as 1.3 ... ");
  asf_meta_read(file, "1.3", &meta, NULL);
  asf_meta_write(meta, "1.3", NULL, "test9.meta");
  printf("done\n");

  /* TEST 10: Read 1.3 and write as 1.2 */
  printf("TEST 10: Read as 1.3, write as 1.2 ... ");
  asf_meta_read(file, "1.3", &meta, NULL);
  asf_meta_write(meta, "1.2", NULL, "test10.meta");
  printf("done\n");

  /* TEST 11: Read 1.2 and write as 1.2 */
  printf("TEST 11: Read as 1.2, write as 1.2 ... ");
  asf_meta_read(file, "1.2", &meta, NULL);
  asf_meta_write(meta, "1.2", NULL, "test11.meta");
  printf("done\n");

  /* TEST 12: Read 1.3 and write as 1.1 */
  printf("TEST 12: Read as 1.3, write as 1.1 ... ");
  asf_meta_read(file, "1.3", &meta, NULL);
  asf_meta_write(meta, "1.1", NULL, "test12.meta");
  printf("done\n");

  /* TEST 13: Read 1.1 and write as 1.1 */
  printf("TEST 13: Read as 1.1, write as 1.1 ... ");
  asf_meta_read(file, "1.1", &meta, NULL);
  asf_meta_write(meta, "1.1", NULL, "test13.meta");
  printf("done\n");

  printf("\nTests on 'image_lamcc13.meta'\n");
  sprintf(file, "image_lamcc13.meta");
  /* TEST 14: Read 1.3 and write as 1.3 */
  printf("TEST 14: Read as 1.3, write as 1.3 ... ");
  asf_meta_read(file, "1.3", &meta, NULL);
  asf_meta_write(meta, "1.3", NULL, "test14.meta");
  printf("done\n");

  /* TEST 15: Read 1.3 and write as 1.2 */
  printf("TEST 15: Read as 1.3, write as 1.2 ... ");
  asf_meta_read(file, "1.3", &meta, NULL);
  asf_meta_write(meta, "1.2", NULL, "test15.meta");
  printf("done\n");

  /* TEST 16: Read 1.2 and write as 1.2 */
  printf("TEST 16: Read as 1.2, write as 1.2 ... ");
  asf_meta_read(file, "1.2", &meta, NULL);
  asf_meta_write(meta, "1.2", NULL, "test16.meta");
  printf("done\n");

  /* TEST 17: Read 1.3 and write as 1.1 */
  printf("TEST 17: Read as 1.3, write as 1.1 ... ");
  asf_meta_read(file, "1.3", &meta, NULL);
  asf_meta_write(meta, "1.1", NULL, "test17.meta");
  printf("done\n");

  /* TEST 18: Read 1.1 and write as 1.1 */
  printf("TEST 18: Read as 1.1, write as 1.1 ... ");
  asf_meta_read(file, "1.1", &meta, NULL);
  asf_meta_write(meta, "1.1", NULL, "test18.meta");
  printf("done\n");

  printf("\nTests on 'image_ps13.meta'\n");
  sprintf(file, "image_ps13.meta");
  /* TEST 19: Read 1.3 and write as 1.3 */
  printf("TEST 19: Read as 1.3, write as 1.3 ... ");
  asf_meta_read(file, "1.3", &meta, NULL);
  asf_meta_write(meta, "1.3", NULL, "test19.meta");
  printf("done\n");

  /* TEST 20: Read 1.3 and write as 1.2 */
  printf("TEST 20: Read as 1.3, write as 1.2 ... ");
  asf_meta_read(file, "1.3", &meta, NULL);
  asf_meta_write(meta, "1.2", NULL, "test20.meta");
  printf("done\n");

  /* TEST 21: Read 1.2 and write as 1.2 */
  printf("TEST 21: Read as 1.2, write as 1.2 ... ");
  asf_meta_read(file, "1.2", &meta, NULL);
  asf_meta_write(meta, "1.2", NULL, "test21.meta");
  printf("done\n");

  /* TEST 22: Read 1.3 and write as 1.1 */
  printf("TEST 22: Read as 1.3, write as 1.1 ... ");
  asf_meta_read(file, "1.3", &meta, NULL);
  asf_meta_write(meta, "1.1", NULL, "test22.meta");
  printf("done\n");

  /* TEST 23: Read 1.1 and write as 1.1 */
  printf("TEST 23: Read as 1.1, write as 1.1 ... ");
  asf_meta_read(file, "1.1", &meta, NULL);
  asf_meta_write(meta, "1.1", NULL, "test23.meta");
  printf("done\n");

  printf("\nTest on 'image12.meta' for stats block\n");
  sprintf(file, "image12.meta");
  /* TEST 24: Read 1.2 and write as 1.2 */
  printf("TEST 24: Read as 1.2, write as 1.2 ... ");
  asf_meta_read(file, "1.2", &meta, NULL);
  asf_meta_write(meta, "1.2", NULL, "test24.meta");
  printf("done\n");

  /* TEST 25: Read 1.2 and write as 1.1 */
  printf("TEST 25: Read as 1.2, write as 1.1 ... ");
  asf_meta_read(file, "1.2", &meta, NULL);
  asf_meta_write(meta, "1.1", NULL, "test25.meta");
  printf("done\n");

  printf("\n\nTESTS FOR FORWARD COMPATIBILITY WITHIN 1.X\n");
  printf("\nTests on 'image13.meta'\n");
  sprintf(file, "image13.meta");
  /* TEST 26: Read 1.3 and write as 1.3 */
  printf("TEST 26: Read as 1.3, write as 1.3 ... ");
  asf_meta_read(file, "1.3", &meta, NULL);
  asf_meta_write(meta, "1.3", NULL, "test26.meta");
  printf("done\n");

  /* TEST 27: Read 1.3 and write as 1.4 */
  printf("TEST 27: Read as 1.3, write as 1.4 ... ");
  asf_meta_read(file, "1.3", &meta, NULL);
  asf_meta_write(meta, "1.4", NULL, "test27.meta");
  printf("done\n");

  printf("\nTests on 'image_utm12.meta'\n");
  sprintf(file, "image_utm12.meta");
  /* TEST 28: Read 1.2 and write as 1.2 */
  printf("TEST 28: Read as 1.2, write as 1.2 ... ");
  asf_meta_read(file, "1.2", &meta, NULL);
  asf_meta_write(meta, "1.2", NULL, "test28.meta");
  printf("done\n");

  /* TEST 29: Read 1.2 and write as 1.3 */
  printf("TEST 29: Read as 1.2, write as 1.3 ... ");
  asf_meta_read(file, "1.2", &meta, NULL);
  asf_meta_write(meta, "1.3", NULL, "test29.meta");
  printf("done\n");

  /* TEST 30: Read 1.2 and write as 1.4 */
  printf("TEST 30: Read as 1.2, write as 1.4 ... ");
  asf_meta_read(file, "1.2", &meta, NULL);
  asf_meta_write(meta, "1.4", NULL, "test30.meta");
  printf("done\n");

  /* TEST 31: Read 1.3 and write as 1.3 */
  printf("TEST 31: Read as 1.3, write as 1.3 ... ");
  asf_meta_read(file, "1.3", &meta, NULL);
  asf_meta_write(meta, "1.3", NULL, "test31.meta");
  printf("done\n");

  /* TEST 32: Read 1.4 and write as 1.4 */
  printf("TEST 32: Read as 1.4, write as 1.4 ... ");
  asf_meta_read(file, "1.4", &meta, NULL);
  asf_meta_write(meta, "1.4", NULL, "test32.meta");
  printf("done\n");

  printf("\nTests on 'image_albers12.meta'\n");
  sprintf(file, "image_albers12.meta");
  /* TEST 33: Read 1.2 and write as 1.2 */
  printf("TEST 33: Read as 1.2, write as 1.2 ... ");
  asf_meta_read(file, "1.2", &meta, NULL);
  asf_meta_write(meta, "1.2", NULL, "test33.meta");
  printf("done\n");

  /* TEST 34: Read 1.2 and write as 1.3 */
  printf("TEST 34: Read as 1.2, write as 1.3 ... ");
  asf_meta_read(file, "1.2", &meta, NULL);
  asf_meta_write(meta, "1.3", NULL, "test34.meta");
  printf("done\n");

  /* TEST 35: Read 1.2 and write as 1.4 */
  printf("TEST 35: Read as 1.2, write as 1.4 ... ");
  asf_meta_read(file, "1.2", &meta, NULL);
  asf_meta_write(meta, "1.4", NULL, "test35.meta");
  printf("done\n");

  /* TEST 36: Read 1.3 and write as 1.3 */
  printf("TEST 36: Read as 1.3, write as 1.3 ... ");
  asf_meta_read(file, "1.3", &meta, NULL);
  asf_meta_write(meta, "1.3", NULL, "test36.meta");
  printf("done\n");

  /* TEST 37: Read 1.4 and write as 1.4 */
  printf("TEST 37: Read as 1.4, write as 1.4 ... ");
  asf_meta_read(file, "1.4", &meta, NULL);
  asf_meta_write(meta, "1.4", NULL, "test37.meta");
  printf("done\n");

  printf("\nTests on 'image_lamcc12.meta'\n");
  sprintf(file, "image_lamcc12.meta");
  /* TEST 38: Read 1.2 and write as 1.2 */
  printf("TEST 38: Read as 1.2, write as 1.2 ... ");
  asf_meta_read(file, "1.2", &meta, NULL);
  asf_meta_write(meta, "1.2", NULL, "test38.meta");
  printf("done\n");

  /* TEST 39: Read 1.2 and write as 1.3 */
  printf("TEST 39: Read as 1.2, write as 1.3 ... ");
  asf_meta_read(file, "1.2", &meta, NULL);
  asf_meta_write(meta, "1.3", NULL, "test39.meta");
  printf("done\n");

  /* TEST 40: Read 1.2 and write as 1.4 */
  printf("TEST 40: Read as 1.2, write as 1.4 ... ");
  asf_meta_read(file, "1.2", &meta, NULL);
  asf_meta_write(meta, "1.4", NULL, "test40.meta");
  printf("done\n");

  /* TEST 41: Read 1.3 and write as 1.3 */
  printf("TEST 41: Read as 1.3, write as 1.3 ... ");
  asf_meta_read(file, "1.3", &meta, NULL);
  asf_meta_write(meta, "1.3", NULL, "test41.meta");
  printf("done\n");

  /* TEST 42: Read 1.4 and write as 1.4 */
  printf("TEST 42: Read as 1.4, write as 1.4 ... ");
  asf_meta_read(file, "1.4", &meta, NULL);
  asf_meta_write(meta, "1.4", NULL, "test42.meta");
  printf("done\n");

  printf("\nTests on 'image_ps12.meta'\n");
  sprintf(file, "image_ps12.meta");
  /* TEST 43: Read 1.2 and write as 1.2 */
  printf("TEST 43: Read as 1.2, write as 1.2 ... ");
  asf_meta_read(file, "1.2", &meta, NULL);
  asf_meta_write(meta, "1.2", NULL, "test43.meta");
  printf("done\n");

  /* TEST 44: Read 1.2 and write as 1.3 */
  printf("TEST 44: Read as 1.2, write as 1.3 ... ");
  asf_meta_read(file, "1.2", &meta, NULL);
  asf_meta_write(meta, "1.3", NULL, "test44.meta");
  printf("done\n");

  /* TEST 45: Read 1.2 and write as 1.4 */
  printf("TEST 45: Read as 1.2, write as 1.4 ... ");
  asf_meta_read(file, "1.2", &meta, NULL);
  asf_meta_write(meta, "1.4", NULL, "test45.meta");
  printf("done\n");

  /* TEST 46: Read 1.3 and write as 1.3 */
  printf("TEST 46: Read as 1.3, write as 1.3 ... ");
  asf_meta_read(file, "1.3", &meta, NULL);
  asf_meta_write(meta, "1.3", NULL, "test46.meta");
  printf("done\n");

  /* TEST 47: Read 1.4 and write as 1.4 */
  printf("TEST 47: Read as 1.4, write as 1.4 ... ");
  asf_meta_read(file, "1.4", &meta, NULL);
  asf_meta_write(meta, "1.4", NULL, "test47.meta");
  printf("done\n");

  printf("\nTests on 'image11.meta' with stats block\n");
  sprintf(file, "image11.meta");
  /* TEST 48: Read 1.1 and write as 1.1 */
  printf("TEST 48: Read as 1.1, write as 1.1 ... ");
  asf_meta_read(file, "1.1", &meta, NULL);
  asf_meta_write(meta, "1.1", NULL, "test48.meta");
  printf("done\n");

  /* TEST 49: Read 1.1 and write as 1.2 */
  printf("TEST 49: Read as 1.1, write as 1.2 ... ");
  asf_meta_read(file, "1.1", &meta, NULL);
  asf_meta_write(meta, "1.2", NULL, "test49.meta");
  printf("done\n");

  /* TEST 50: Read 1.2 and write as 1.2 */
  printf("TEST 50: Read as 1.2, write as 1.2 ... ");
  asf_meta_read(file, "1.2", &meta, NULL);
  asf_meta_write(meta, "1.2", NULL, "test50.meta");
  printf("done\n");

  printf("\nTests on 'image_utm11.meta'\n");
  sprintf(file, "image_utm11.meta");
  /* TEST 51: Read 1.1 and write as 1.1 */
  printf("TEST 51: Read as 1.1, write as 1.1 ... ");
  asf_meta_read(file, "1.1", &meta, NULL);
  asf_meta_write(meta, "1.1", NULL, "test51.meta");
  printf("done\n");

  /* TEST 52: Read 1.1 and write as 1.2 */
  printf("TEST 52: Read as 1.1, write as 1.2 ... ");
  asf_meta_read(file, "1.1", &meta, NULL);
  asf_meta_write(meta, "1.2", NULL, "test52.meta");
  printf("done\n");

  /* TEST 53: Read 1.2 and write as 1.2 */
  printf("TEST 53: Read as 1.2, write as 1.2 ... ");
  asf_meta_read(file, "1.2", &meta, NULL);
  asf_meta_write(meta, "1.2", NULL, "test53.meta");
  printf("done\n");

  /* TEST 54: Read 1.1 and write as 1.3 */
  printf("TEST 54: Read as 1.1, write as 1.3 ... ");
  asf_meta_read(file, "1.1", &meta, NULL);
  asf_meta_write(meta, "1.3", NULL, "test54.meta");
  printf("done\n");

  /* TEST 55: Read 1.3 and write as 1.3 */
  printf("TEST 55: Read as 1.3, write as 1.3 ... ");
  asf_meta_read(file, "1.3", &meta, NULL);
  asf_meta_write(meta, "1.3", NULL, "test55.meta");
  printf("done\n");

  /* TEST 56: Read 1.1 and write as 1.4 */
  printf("TEST 56: Read as 1.1, write as 1.4 ... ");
  asf_meta_read(file, "1.1", &meta, NULL);
  asf_meta_write(meta, "1.4", NULL, "test56.meta");
  printf("done\n");

  /* TEST 57: Read 1.4 and write as 1.4 */
  printf("TEST 57: Read as 1.4, write as 1.4 ... ");
  asf_meta_read(file, "1.4", &meta, NULL);
  asf_meta_write(meta, "1.4", NULL, "test57.meta");
  printf("done\n");

  printf("\nTests on 'image_lamcc11.meta'\n");
  sprintf(file, "image_lamcc11.meta");
  /* TEST 58: Read 1.1 and write as 1.1 */
  printf("TEST 58: Read as 1.1, write as 1.1 ... ");
  asf_meta_read(file, "1.1", &meta, NULL);
  asf_meta_write(meta, "1.1", NULL, "test58.meta");
  printf("done\n");

  /* TEST 59: Read 1.1 and write as 1.2 */
  printf("TEST 59: Read as 1.1, write as 1.2 ... ");
  asf_meta_read(file, "1.1", &meta, NULL);
  asf_meta_write(meta, "1.2", NULL, "test59.meta");
  printf("done\n");

  /* TEST 60: Read 1.2 and write as 1.2 */
  printf("TEST 60: Read as 1.2, write as 1.2 ... ");
  asf_meta_read(file, "1.2", &meta, NULL);
  asf_meta_write(meta, "1.2", NULL, "test60.meta");
  printf("done\n");

  /* TEST 61: Read 1.1 and write as 1.3 */
  printf("TEST 61: Read as 1.1, write as 1.3 ... ");
  asf_meta_read(file, "1.1", &meta, NULL);
  asf_meta_write(meta, "1.3", NULL, "test61.meta");
  printf("done\n");

  /* TEST 62: Read 1.3 and write as 1.3 */
  printf("TEST 62: Read as 1.3, write as 1.3 ... ");
  asf_meta_read(file, "1.3", &meta, NULL);
  asf_meta_write(meta, "1.3", NULL, "test62.meta");
  printf("done\n");

  /* TEST 63: Read 1.1 and write as 1.4 */
  printf("TEST 63: Read as 1.1, write as 1.4 ... ");
  asf_meta_read(file, "1.1", &meta, NULL);
  asf_meta_write(meta, "1.4", NULL, "test63.meta");
  printf("done\n");

  /* TEST 64: Read 1.4 and write as 1.4 */
  printf("TEST 64: Read as 1.4, write as 1.4 ... ");
  asf_meta_read(file, "1.4", &meta, NULL);
  asf_meta_write(meta, "1.4", NULL, "test64.meta");
  printf("done\n");

  printf("\nTests on 'image_ps11.meta'\n");
  sprintf(file, "image_ps11.meta");
  /* TEST 65: Read 1.1 and write as 1.1 */
  printf("TEST 65: Read as 1.1, write as 1.1 ... ");
  asf_meta_read(file, "1.1", &meta, NULL);
  asf_meta_write(meta, "1.1", NULL, "test65.meta");
  printf("done\n");

  /* TEST 66: Read 1.1 and write as 1.2 */
  printf("TEST 66: Read as 1.1, write as 1.2 ... ");
  asf_meta_read(file, "1.1", &meta, NULL);
  asf_meta_write(meta, "1.2", NULL, "test66.meta");
  printf("done\n");

  /* TEST 67: Read 1.2 and write as 1.2 */
  printf("TEST 67: Read as 1.2, write as 1.2 ... ");
  asf_meta_read(file, "1.2", &meta, NULL);
  asf_meta_write(meta, "1.2", NULL, "test67.meta");
  printf("done\n");

  /* TEST 68: Read 1.1 and write as 1.3 */
  printf("TEST 68: Read as 1.1, write as 1.3 ... ");
  asf_meta_read(file, "1.1", &meta, NULL);
  asf_meta_write(meta, "1.3", NULL, "test68.meta");
  printf("done\n");

  /* TEST 69: Read 1.3 and write as 1.3 */
  printf("TEST 69: Read as 1.3, write as 1.3 ... ");
  asf_meta_read(file, "1.3", &meta, NULL);
  asf_meta_write(meta, "1.3", NULL, "test69.meta");
  printf("done\n");

  /* TEST 70: Read 1.1 and write as 1.4 */
  printf("TEST 70: Read as 1.1, write as 1.4 ... ");
  asf_meta_read(file, "1.1", &meta, NULL);
  asf_meta_write(meta, "1.4", NULL, "test70.meta");
  printf("done\n");

  /* TEST 71: Read 1.4 and write as 1.4 */
  printf("TEST 71: Read as 1.4, write as 1.4 ... ");
  asf_meta_read(file, "1.4", &meta, NULL);
  asf_meta_write(meta, "1.4", NULL, "test71.meta");
  printf("done\n");

  printf("\n\nTESTS CONVERTING BETWEEN 0.9 AND 1.X\n");
  printf("Tests on 'image11.meta'\n");
  sprintf(file, "image11.meta");
  /* TEST 72: Read 1.1 in and write as 0.9 */
  printf("TEST 72: Read as 1.1, write as 0.9 ... ");
  asf_meta_read(file, "1.1", &meta, NULL);
  asf_meta_write(meta, "0.9", NULL, "test72.meta");
  printf("done\n");

  /* TEST 73: Read 0.9 in and write as 0.9 */
  printf("TEST 73: Read as 0.9, write as 0.9 ... ");
  asf_meta_read(file, "0.9", &meta, &ddr);
  asf_meta_write(meta, "0.9", ddr, "test73.meta");
  printf("done\n");

  printf("\nTests on 'image_utm13.meta'\n");
  sprintf(file, "image_utm13.meta");
  /* TEST 74: Read 1.3 and write as 0.9 */
  printf("TEST 74: Read as 1.3, write as 0.9 ... ");
  asf_meta_read(file, "1.3", &meta, NULL);
  asf_meta_write(meta, "0.9", ddr, "test74.meta");
  printf("done\n");

  /* TEST 75: Read 0.9 and write as 0.9 */
  printf("TEST 75: Read as 0.9, write as 0.9 ... ");
  asf_meta_read(file, "0.9", &meta, &ddr);
  asf_meta_write(meta, "0.9", ddr, "test75.meta");
  printf("done\n");

  printf("\nTests on 'image_utm09.meta' and 'image_utm09.ddr'\n");
  sprintf(file, "image_utm09.meta");

  /* TEST 76: Read 0.9 and write as 1.1 */
  printf("TEST 76: Read as 0.9, write as 1.1 ... ");
  asf_meta_read(file, "0.9", &meta, &ddr);
  asf_meta_write(meta, "1.1", ddr, "test76.meta");
  printf("done\n");

  /* TEST 77: Read 1.1 and write as 1.1 */
  printf("TEST 77: Read as 1.1, write as 1.1 ... ");
  asf_meta_read(file, "1.1", &meta, &ddr);
  asf_meta_write(meta, "1.1", ddr, "test77.meta");
  printf("done\n");

  /* TEST 78: Read 0.9 and write as 1.2 */
  printf("TEST 78: Read as 0.9, write as 1.2 ... ");
  asf_meta_read(file, "0.9", &meta, &ddr);
  asf_meta_write(meta, "1.2", ddr, "test78.meta");
  printf("done\n");

  /* TEST 79: Read 1.2 and write as 1.2 */
  printf("TEST 79: Read as 1.2, write as 1.2 ... ");
  asf_meta_read(file, "1.2", &meta, &ddr);
  asf_meta_write(meta, "1.2", ddr, "test79.meta");
  printf("done\n");

  /* TEST 80: Read 0.9 and write as 1.3 */
  printf("TEST 80: Read as 0.9, write as 1.3 ... ");
  asf_meta_read(file, "0.9", &meta, &ddr);
  asf_meta_write(meta, "1.3", ddr, "test80.meta");
  printf("done\n");

  /* TEST 81: Read 1.3 and write as 1.3 */
  printf("TEST 81: Read as 1.3, write as 1.3 ... ");
  asf_meta_read(file, "1.3", &meta, &ddr);
  asf_meta_write(meta, "1.3", ddr, "test81.meta");
  printf("done\n");

  printf("\nTests on 'image_ps13.meta'\n");
  sprintf(file, "image_ps13.meta");

  /* TEST 82: Read 1.3 and write as 0.9 */
  printf("TEST 82: Read as 1.3, write as 0.9 ... ");
  asf_meta_read(file, "1.3", &meta, &ddr);
  asf_meta_write(meta, "0.9", ddr, "test82.meta");
  printf("done\n");

  /* TEST 83: Read 0.9 and write as 0.9 */
  printf("TEST 83: Read as 0.9, write as 0.9 ... ");
  asf_meta_read(file, "0.9", &meta, &ddr);
  asf_meta_write(meta, "0.9", ddr, "test83.meta");
  printf("done\n");

  printf("\nTests on 'image_ps09.meta' and 'image_ps09.ddr'\n");
  sprintf(file, "image_ps09.meta");

  /* TEST 84: Read 0.9 and write as 1.1 */
  printf("TEST 84: Read as 0.9, write as 1.1 ... ");
  asf_meta_read(file, "0.9", &meta, &ddr);
  asf_meta_write(meta, "1.1", ddr, "test84.meta");
  printf("done\n");

  /* TEST 85: Read 1.1 and write as 1.1 */
  printf("TEST 85: Read as 1.1, write as 1.1 ... ");
  asf_meta_read(file, "1.1", &meta, &ddr);
  asf_meta_write(meta, "1.1", ddr, "test85.meta");
  printf("done\n");

  /* TEST 86: Read 0.9 and write as 1.2 */
  printf("TEST 86: Read as 0.9, write as 1.2 ... ");
  asf_meta_read(file, "0.9", &meta, &ddr);
  asf_meta_write(meta, "1.2", ddr, "test86.meta");
  printf("done\n");

  /* TEST 87: Read 1.2 and write as 1.2 */
  printf("TEST 87: Read as 1.2, write as 1.2 ... ");
  asf_meta_read(file, "1.2", &meta, &ddr);
  asf_meta_write(meta, "1.2", ddr, "test87.meta");
  printf("done\n");

  /* TEST 88: Read 0.9 and write as 1.3 */
  printf("TEST 88: Read as 0.9, write as 1.3 ... ");
  asf_meta_read(file, "0.9", &meta, &ddr);
  asf_meta_write(meta, "1.3", ddr, "test88.meta");
  printf("done\n");

  /* TEST 89: Read 1.3 and write as 1.3 */
  printf("TEST 89: Read as 1.3, write as 1.3 ... ");
  asf_meta_read(file, "1.3", &meta, &ddr);
  asf_meta_write(meta, "1.3", ddr, "test89.meta");
  printf("done\n");

  printf("\nTests on 'image_albers13.meta'\n");
  sprintf(file, "image_albers13.meta");

  /* TEST 90: Read 1.3 and write as 0.9 */
  printf("TEST 90: Read as 1.3, write as 0.9 ... ");
  asf_meta_read(file, "1.3", &meta, &ddr);
  asf_meta_write(meta, "0.9", ddr, "test90.meta");
  printf("done\n");

  /* TEST 91: Read 0.9 and write as 0.9 */
  printf("TEST 91: Read as 0.9, write as 0.9 ... ");
  asf_meta_read(file, "0.9", &meta, &ddr);
  asf_meta_write(meta, "0.9", ddr, "test91.meta");
  printf("done\n");

  printf("\nTests on 'image_lamcc13.meta'\n");
  sprintf(file, "image_lamcc13.meta");

  /* TEST 92: Read 1.3 and write as 0.9 */
  printf("TEST 92: Read as 1.3, write as 0.9 ... ");
  asf_meta_read(file, "1.3", &meta, &ddr);
  asf_meta_write(meta, "0.9", ddr, "test92.meta");
  printf("done\n");

  /* TEST 93: Read 0.9 and write as 0.9 */
  printf("TEST 93: Read as 0.9, write as 0.9 ... ");
  asf_meta_read(file, "0.9", &meta, &ddr);
  asf_meta_write(meta, "0.9", ddr, "test93.meta");
  printf("done\n");

  printf("\nTests on 'image_lamcc09.meta' and 'image_lamcc09.ddr'\n");
  sprintf(file, "image_lamcc09.meta");

  /* TEST 94: Read 0.9 and write as 1.1 */
  printf("TEST 94: Read as 0.9, write as 1.1 ... ");
  asf_meta_read(file, "0.9", &meta, &ddr);
  asf_meta_write(meta, "1.1", ddr, "test94.meta");
  printf("done\n");

  /* TEST 95: Read 1.1 and write as 1.1 */
  printf("TEST 95: Read as 1.1, write as 1.1 ... ");
  asf_meta_read(file, "1.1", &meta, &ddr);
  asf_meta_write(meta, "1.1", ddr, "test95.meta");
  printf("done\n");

  /* TEST 96: Read 0.9 and write as 1.2 */
  printf("TEST 96: Read as 0.9, write as 1.2 ... ");
  asf_meta_read(file, "0.9", &meta, &ddr);
  asf_meta_write(meta, "1.2", ddr, "test96.meta");
  printf("done\n");

  /* TEST 97: Read 1.2 and write as 1.2 */
  printf("TEST 97: Read as 1.2, write as 1.2 ... ");
  asf_meta_read(file, "1.2", &meta, &ddr);
  asf_meta_write(meta, "1.2", ddr, "test97.meta");
  printf("done\n");

  /* TEST 98: Read 0.9 and write as 1.3 */
  printf("TEST 98: Read as 0.9, write as 1.3 ... ");
  asf_meta_read(file, "0.9", &meta, &ddr);
  asf_meta_write(meta, "1.3", ddr, "test98.meta");
  printf("done\n");

  /* TEST 99: Read 1.3 and write as 1.3 */
  printf("TEST 99: Read as 1.3, write as 1.3 ... ");
  asf_meta_read(file, "1.3", &meta, &ddr);
  asf_meta_write(meta, "1.3", ddr, "test99.meta");
  printf("done\n");

  return(0);
}

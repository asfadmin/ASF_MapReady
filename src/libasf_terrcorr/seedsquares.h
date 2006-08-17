
void seed_points(int *x, int *y, int ns, int nl);
void check_square(int size,int x_pos, int y_pos, float  *mask, int ns, int nl, long *masked_pixels, long *good_pixels);
void lay_seeds(int num_seeds, float *mask, long ns, long nl, int *x_pos_list, int *y_pos_list, int *size_list, long *clipped_pixels);


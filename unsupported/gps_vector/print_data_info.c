#include "asf.h"

#include "gps_vector.h"

void print_data_info(meta)
struct gps_meta *meta;
{
    char buf[100];

    strncpy (buf, meta->apid, 11); 
    buf[11] = '\0'; 
    printf ("Source Image ID = %s", buf); 
    strncpy (buf, meta->ayear, 5); 
    buf[5] = '\0';
    printf ("\tYear = %s", buf);
    strncpy (buf, meta->atime, 17);
    buf[17] = '\0';
    printf ("\tDate = %s\n", buf);
    strncpy (buf, meta->bpid, 11);
    buf[11] = '\0';
    printf ("Target image ID = %s", buf);
    strncpy (buf, meta->byear, 5);
    buf[5] = '\0';
    printf ("\tYear = %s", buf);
    strncpy (buf, meta->btime, 17);
    buf[17] = '\0';
    printf ("\tDate = %s\n", buf);
    
    strncpy(buf, meta->avgdispx, 10);
    buf[9] = '\0';
    printf("Avg X Displacement = %s\n", buf);
    
    strncpy(buf, meta->avgdispy, 10);
    buf[9] = '\0';
    printf("Avg Y Displacement = %s\n", buf);

    return;
}


/* SccsId[]= @(#)main.c	2.41 3/24/98 */
static char sccsid_main[]= "@(#)PPmain.c:2.41";

#include <stdio.h>
#include <stdlib.h>
#include <sys/file.h>
#include <string.h>
#include <syslog.h>

#include "main.h" 
#include "odl_file.h"
#include "aux_file.h"
#include "cali_file.h"
#include "input_file.h"
#include "error.h"

struct ODL_FILE *odl_file;
struct AUX_FILE *aux_file;
struct CALI_FILE *cali_file;
struct IMAGE image;
struct DATA_SET data_set;
struct FACILITY_RECORD facility_record;
struct RANGE_SPECTRA range_spectra;
struct PLATFORM_POSITION platform_position;

int num_rec;
int rec_length;

void read_info(struct IMAGE*,
               struct DATA_SET*,
               struct FACILITY_RECORD*,
               struct RANGE_SPECTRA*,
               struct PLATFORM_POSITION*, int);
int set_status(char *, int, float);
char * get_host_name(char *path);
		
int main(int argc, char *argv[]) 
{

	int in_fd, out_fd;

	int i, error_code, new_rec_length, counter;
	int n, num_recs_left, x, y;

	unsigned char *image_buf;
	unsigned char *buf;
	unsigned char *buf2, *buffer, *buffer2, *buffer3;

	char in_file[MAX_LENGTH];
	char out_file[MAX_LENGTH];
	char fileaux[MAX_LENGTH];
	char filecali[MAX_LENGTH];

	if (argc != 2) {
		printf("usage: %s config_file\n", argv[0]);
		exit(ierr_21);
	}
		
	printf("start the ceos program\n");
	fflush(stdout);

	load_config(argv[1]);

	odl_file = (struct ODL_FILE *) malloc(sizeof(struct ODL_FILE));
	aux_file = (struct AUX_FILE *) malloc(sizeof(struct AUX_FILE));
	cali_file = (struct CALI_FILE *) malloc(sizeof(struct CALI_FILE));


	if((error_code = get_frame_rqst(fileodl, odl_file)) != 0)
		info_handler(error_code, NULL, "%s", odl_file->error_msg);
/**
	get_frame_rqst(fileodl, odl_file);
**/

	sprintf(fileaux, "/home/tmpdisk/aux.dat.%d", odl_file->job_id);
	sprintf(filecali, "/home/tmpdisk/calib.dat.%d", odl_file->job_id);

	if((error_code = get_aux_file(fileaux, aux_file)) != 0)
		info_handler(error_code, NULL, "%s", aux_file->error_msg);
	if((error_code = get_cali_file(filecali, cali_file)) != 0)
		info_handler(error_code, NULL, "%s", cali_file->error_msg);
/**
	get_aux_file(fileaux, aux_file);
	get_cali_file(filecali, cali_file);
**/
	read_info(&image, &data_set, &facility_record, &range_spectra, &platform_position, odl_file->job_id);
	
	sprintf(in_file,"/home/tmpdisk/image_tmp.dat.%d", odl_file->job_id);
	sprintf(out_file, "/home/tmpdisk/image.dat.%d", odl_file->job_id);

	num_rec = image.number_of_records;
	rec_length = image.record_length;

	if (!strcmp(odl_file->prod_type, "STANDARD") || 
		!strcmp(odl_file->prod_type, "RAMP")) {
		if ((in_fd = open(in_file,O_RDONLY, 0)) == -1)
			info_handler(ierr_2, in_file, 
					"can't open %s for reading", in_file);
		if ((out_fd = creat(out_file, 0644)) == -1)
			info_handler(ierr_2, out_file,"can't create file %s", out_file);
		image_buf = (unsigned char *) malloc(rec_length);
		new_rec_length = rec_length + 192;
		buf = (unsigned char *) malloc(new_rec_length);
		fill_image_descriptor_record(buf);
		if (write(out_fd, buf, new_rec_length) != new_rec_length)
			info_handler(ierr_3, out_file, "write error on file %s\n", out_file);
		set_status(filestatus, 5, 0.01);

		counter = 1;
		for (i=1; i<=num_rec; i++) { 
			if (strcmp(odl_file->frame_mode, "ARCTIC") == 0) 
				lseek(in_fd, (long)((num_rec-i)*rec_length), 0);
				
			if (read(in_fd, image_buf, rec_length) != rec_length)
				info_handler(ierr_4, in_file, "read error on file %s\n", in_file);

			fill_processed_data_record(buf);
			memcpy(&buf[192], image_buf, rec_length);
			if (write(out_fd, buf, new_rec_length) != new_rec_length)
				info_handler(ierr_3, out_file, "write error on file %s\n", out_file); 
			if ((counter == 1000) || (i == num_rec)) {
				set_status(filestatus, 5, (float)i/(float)num_rec);
				counter = 1;
			}
			else
				++counter;
		}

		free(image_buf);
		free(buf);
	}
	else if (!strcmp(odl_file->prod_type, "COMPLEX")) {
		if ((in_fd = open(in_file,O_RDWR, 0)) == -1)
			info_handler(ierr_2, in_file, "can't open %s for read and write", in_file);
		buf = (unsigned char *) malloc(rec_length);
		fill_image_descriptor_record(buf);
		lseek(in_fd, 0L, 0);
		if (write(in_fd, buf, rec_length) != rec_length)
			info_handler(ierr_3, in_file, "write error on %s", in_file);
		set_status(filestatus, 5, 0.01);

		if (!strcmp(odl_file->frame_mode, "ANTARCTIC")) {
			counter = 1;
			for (i=1; i<num_rec; i++) {
				lseek(in_fd, (long)(i*rec_length), 0);
				if (read(in_fd, buf, rec_length) != rec_length)
					info_handler(ierr_4, in_file, "read error on file %s\n", in_file);
				fill_processed_data_record(buf);
				lseek(in_fd, (long)(i*rec_length), 0);
				if (write(in_fd, buf, rec_length) != rec_length)
					info_handler(ierr_3, in_file, "write error on file %s", in_file);
				if ((counter == 1000) || (i == (num_rec-1))) {
					set_status(filestatus, 5, (float)i/(float)(num_rec-1));
					counter = 1;
				}
				else
					++counter;
			}
		}
		else {
			n = 1000;
			buffer = (unsigned char *) malloc(rec_length*n);
			buffer2 = (unsigned char *) malloc(rec_length*n);
			buffer3 = (unsigned char *) malloc(rec_length*n);
			counter = 1;

			for (x=1,y=n,num_recs_left=(num_rec-1)/2;
			     num_recs_left > 0;
			     x += n, y += n, num_recs_left -= n) {

				if (num_recs_left < n) n = num_recs_left;
				lseek(in_fd, (long)(x*rec_length), 0);
				if (read(in_fd, buffer, rec_length*n) != rec_length*n)
					info_handler(ierr_4, in_file, "read error on file %s\n", in_file);

				lseek(in_fd, (long)((num_rec-y+1000-n)*rec_length), 0);
				if (read(in_fd, buffer2, rec_length*n) != rec_length*n)
					info_handler(ierr_4, in_file, "read error on file %s\n", in_file);

				lseek(in_fd, (long)(x*rec_length), 0);
				for (i=1; i<=n; i++) {
					memcpy(&buffer3[(i-1)*rec_length], &buffer2[(n-i)*rec_length], rec_length);
				}
				if (write(in_fd, buffer3, rec_length*n) != (rec_length*n))
					info_handler(ierr_3, in_file, "write error on %s", in_file);

				lseek(in_fd, (long)((num_rec-y+1000-n)*rec_length), 0);
				for (i=1; i<=n; i++) {
					memcpy(&buffer3[(i-1)*rec_length], &buffer[(n-i)*rec_length], rec_length);
				}
				if (write(in_fd, buffer3, rec_length*n) != (rec_length*n))
					info_handler(ierr_3, in_file, "write error on %s", in_file);

				if ((counter == 500) || (i == ((num_rec-1)/2))) {
					set_status(filestatus, 5, (float)i/(float)((num_rec-1)/2));
					counter = 1;
				}
				else
					++counter;
			}
			free(buffer);
			free(buffer2);
			free(buffer3);
			for (i=1; i<num_rec; i++) {
				lseek(in_fd, (long)(i*rec_length), 0);
      	if (read(in_fd, buf, rec_length) != rec_length)
					info_handler(ierr_4, in_file, "read error on file %s\n", in_file);
				fill_processed_data_record(buf);
				lseek(in_fd, (long)(i*rec_length), 0);
				if (write(in_fd, buf, rec_length) != rec_length)
					info_handler(ierr_3, in_file, "write error on file %s", in_file);
			}
		}
		rename(in_file, out_file);
		free(buf);
	}
	else
		info_handler(ierr_23,"", "Invalid product type");

	close(in_fd);
	close(out_fd);
	write_ceos();
	write_pmf();
	set_status(filestatus, 6, 0.01);
	transfer_back();
	set_status(filestatus, 6, 1.0);
		
	free(odl_file);
	free(aux_file);
	free(cali_file);

	printf("end the ceos program\n");

	return 0;

}

/****************************************************************************/
int transfer_back()  
{
	char *dest_file;
	char file[256];
	char buffer[500];
	int fd;

	dest_file = strchr(odl_file->image_file, '/');

	sprintf(file, "/home/tmpdisk/COPY.%d", odl_file->job_id);
	if ((fd = creat(file, 0755)) == -1)
		info_handler(ierr_2, file, "can't create file %s", file);

	sprintf(buffer, "rcp /home/tmpdisk/image.dat.%d %s:%s\n",
					odl_file->job_id,
					get_host_name(odl_file->image_file),
					dest_file);
	write(fd, buffer, strlen(buffer));
	dest_file = strchr(odl_file->ceos_leader_file, '/');
	sprintf(buffer, "rcp /home/tmpdisk/ceos_ldr.%d %s:%s\n", 
					odl_file->job_id,
					get_host_name(odl_file->ceos_leader_file),
					dest_file);
	write(fd, buffer, strlen(buffer));
	dest_file = strchr(odl_file->pmf_file, '/');
	sprintf(buffer, "rcp /home/tmpdisk/pmf.%d %s:%s\n",
					odl_file->job_id,
					get_host_name(odl_file->pmf_file),
					dest_file);
	write(fd, buffer, strlen(buffer));
	close(fd);

	unix_command("%s", file);

	return 0;
}
/****************************************************************************/
char * get_host_name(char *path)
{
	static char host_name[256];
  char *ptr;

 	strcpy(host_name, path);
  if ((ptr = strchr(host_name, ':')) != NULL) {
    *ptr = '\0';
		strcat(host_name, "-fddi");
	}
	else
		strcpy(host_name, "\0");

	return host_name;
}
/****************************************************************************/
int set_status(char *file_status, int istage, float ipercent)
{
	FILE *fp;

	if ((fp=fopen(file_status,"a"))== NULL)
		info_handler(ierr_2, file_status, "can't open file %s", file_status);
	ipercent = -ipercent;
	fprintf(fp,"%d %.2f\n",istage,ipercent);
	fclose(fp);

	return 0;
}




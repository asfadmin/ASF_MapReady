/* Alaska SAR Processor (ASP) %W% %E% %U% */
#include <stdio.h>
#include <string.h>
#include <fcntl.h>

#define PASS 0
#define FAIL -1

main()
{
	int dcrsi_fd, len;
	char cmd[132];

        if (( dcrsi_fd = open( "/dev/tty01", O_WRONLY ) ) == -1 ){
                printf("Cannot open /dev/tty01\n");
                exit(-1);
        }
	strcpy(cmd,"SL;MD;UL;");
	len = strlen(cmd);
	write(dcrsi_fd,cmd,len);
        close( dcrsi_fd );
        exit();
}

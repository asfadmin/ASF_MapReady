#include "socket.h"
#include "asf.h"

static void parse_url(char *url,
                      char **protocol, char **host, int *port, char **path)
{
  char *ds = strstr(url, "://");
  
  // protocol
  if (!ds) {
    *protocol = STRDUP("http");
    ds = url;
  }
  else {
    *ds = '\0';
    *protocol = STRDUP(url);
    *ds = ':';
    ds += 3; // skip over ://
  }

  // host
  char *ps = strstr(ds,"/");
  if (!ps) {
    // no path is specified, just a host[:port]
    *host = STRDUP(ds);
    ps = &ds[strlen(ds)]; // point to the end
  }
  else {
    *ps = '\0';
    *host = STRDUP(ds);
    *ps = '/';
  }

  // pull port out of host
  char *c = strstr(*host,":");
  if (!c) {
    *port=80;
  }
  else {
    *c = '\0';
    *port = atoi(c+1);
  }

  // the rest is the path
  *path = STRDUP(ps);
}

static char *skt_recv_line(SOCKET s)
{
  char c;
  char *buf;
  int i=0;

  int currlen=256;
  buf = CALLOC(currlen, sizeof(char));

  while (1) {
    skt_recvN(s,&c,1);
    if (c=='\r') continue; // ignore if by itself, break if \n follows
    if (c=='\n') {
      buf[i]='\0';
      break;
    }
    buf[i++]=c;
    if (i==currlen-1) {
      currlen += 256;
      char *newbuf = CALLOC(currlen, sizeof(char));
      memcpy(newbuf, buf, currlen-256);
      free(buf);
      buf = newbuf;
    }
  }

  return buf;
}

unsigned char *download_url(const char *url_in, int verbose, int *length)
{
  char *url=STRDUP(url_in);

  // parse the url
  char *protocol, *host, *path;
  int port;
  parse_url(url, &protocol, &host, &port, &path);

  if (verbose)
    printf("Connecting to URL: %s://%s:%d%s\n", protocol, host, port, path);

  int timeout = 60;
  if (verbose)
    printf("Looking up IP Address for: %s\n", host);
  skt_ip_t hostIP = skt_lookup_ip(host);
  if (verbose)
    printf("Connecting to %s\n", host);
  SOCKET s = skt_connect(hostIP, port, timeout);

  char *send_get = MALLOC(strlen(url)+128);
  sprintf(send_get,
          "GET %s HTTP/1.1\r\n"
          "Host: %s\r\n"
          "User-Agent: Mozilla/5.0\r\n"
          "\r\n", path, host);

  // send our get request
  skt_sendN(s, send_get, strlen(send_get));

  // wait...
  if (verbose)
    printf("Waiting for a response from %s\n", host);
  char *status = skt_recv_line(s);
  if (verbose)
    printf("Received status: %s\n", status);
  //char *status_trim = trim_spaces(status);
  free(status);

  // now can get the response code
  //int code = atoi(status_trim);
  //free(status_trim);

  // a zero-length line indicates the end of the HTTP headers... we ignore
  int len=-1;
  while (1) {
    char *line = skt_recv_line(s);
    if (strlen(line)==0) break;
    if (strstr(line, "Content-Length") != 0) {
      char *l = line + strlen("Content-Length") + 2;
      len=atoi(l);
    }
  }
  if (verbose)
    printf("Content Length: %d\n", len);

  if (len==-1) {
    asfPrintWarning("No Content-Length specified in the HTTP headers.\n");
    return NULL;
  }

  // receiving data...
  unsigned char *data = CALLOC(len+12, sizeof(char));
  int curr=0, chunkSize=1024;
  while (1) {
    int amt = chunkSize < len-curr ? chunkSize : len-curr;
    skt_recvN(s,data+curr,amt);
    curr += amt;
    if (verbose)
      printf("Retrieved %d Kb so far.\n", curr);
    if (curr==len)
      break;
    else if (curr>len)
      asfPrintError("Invalid Content-Length?\n");
  }

  if(verbose)
    printf("Done.\n");
  //if (verbose)
  //  printf("Received data:\n"
  //         "-------------------------------------------------------------\n"
  //         "%s\n"
  //         "-------------------------------------------------------------\n",
  //         data);

  free(protocol);
  free(host);
  free(path);
  free(url);
  free(send_get);
  skt_close(s);

  *length = len;
  return data;
}

int download_url_to_file(const char *url, const char *filename)
{
  int len;
  unsigned char *data = download_url(url, FALSE, &len);

  FILE *fp = FOPEN(filename, "w");
  FWRITE(data, sizeof(char), len, fp);
  FCLOSE(fp);

  return TRUE;
}

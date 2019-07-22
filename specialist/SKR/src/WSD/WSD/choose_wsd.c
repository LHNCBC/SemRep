#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAXLINE 2048

char cw_wsdchoice[MAXLINE+1];

static char sccs_id_choose_wsd_c[] = "@(#)choose_wsd.c	1.2 11/02/06";

/* load assignment table */
int
choose_wsd_initialize(char *wsd_root)
{
  int status;
  char assign_file[MAXLINE+1];
  char buf[MAXLINE+1];
  FILE* afp;
  char *hostname  = (char *)calloc(512, sizeof(char));

  memset((void *)cw_wsdchoice, (int)'\0', MAXLINE);
  if (hostname) {
    status=gethostname(hostname, sizeof(char)*512);
    printf("hostname: %s\n", hostname);
  } else {
    fprintf(stderr, "error allocating hostname\n");
    return -1;
  }
  sprintf(assign_file,"%s/config/wsd.assignments",wsd_root);
  printf("The assignment file is: %s\n", assign_file);
  afp = fopen(assign_file, "r");
  if (afp != NULL) {
    while (fgets(buf, MAXLINE, afp) != NULL) {
      if (strncmp(hostname, buf, strlen(hostname)) == 0) {
        strcpy(cw_wsdchoice, buf);
        break;
      }
/*       printf("line: %s", buf); */
    }
    fclose(afp);
  } else {
    return -1;
  }
  if (hostname != NULL) free(hostname);
  printf("wsd choice: %s", cw_wsdchoice);
  return 0;
}

/* return in parameter "choice" which server to use for host: "" for
   server1, "b" for server2 */
int
choose_wsd(char *choice)
{
  char host[MAXLINE];
  char group[MAXLINE];

  if (strlen(cw_wsdchoice) > 0) {
    sscanf(cw_wsdchoice, "%[^|]", host);
    sscanf(strchr(cw_wsdchoice,'|')+1, "%[^|]", group);
    sscanf(strrchr(cw_wsdchoice,'|')+1, "%[^|]", choice);
    /*   printf("host: %s \n", host); */
    /*   printf("group: %s\n", group); */
  }
  return 0;
}


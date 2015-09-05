#include <stdio.h>
#include <stdlib.h>
#ifndef MSDOS
#include <unistd.h>
#else
#include <io.h>
#define RAND
#endif

#include <time.h>
#include "des_ver.h"
#include "des_locl.h"

#ifdef VMS
#include <types.h>
#include <stat.h>
#else
#ifndef _IRIX
#include <sys/types.h>
#endif
#include <sys/stat.h>
#endif
#if defined(NOCONST)
#define const
#endif
#include "des.h"

#define BUFSIZE (8*1024)
#define VERIFY  1
#define KEYSIZ	8
#define KEYSIZB 1024 /* should hit tty line limit first :-) */
char key[KEYSIZB+1];
int do_encrypt,longk=0;
FILE *DES_IN,*DES_OUT,*CKSUM_OUT;
char uuname[200];
unsigned char uubuf[50];
int uubufnum=0;
#define INUUBUFN	(45*100)
#define OUTUUBUF	(65*100)
unsigned char b[OUTUUBUF];
unsigned char bb[300];
des_cblock cksum={0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00};
char cksumname[200]="";

int vflag,cflag,eflag,dflag,kflag,bflag,fflag,sflag,uflag,flag3,hflag,libdes_error;


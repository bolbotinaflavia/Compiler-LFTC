#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include "utils.h"

void err(const char *fmt,...){
	fprintf(stderr,"error: ");
	va_list va;
	va_start(va,fmt);
	vfprintf(stderr,fmt,va);
	va_end(va);
	fprintf(stderr,"\n");
	exit(EXIT_FAILURE);
	}

void *safeAlloc(size_t nBytes){
	void *p=malloc(nBytes);
	if(!p)err("not enough memory");
	return p;
	}

char *loadFile(const char *fileName){
	FILE *fis=fopen(fileName,"rb");
	if(!fis)err("unable to open %s",fileName);
	fseek(fis,0,SEEK_END);
	size_t n=(size_t)ftell(fis);
	fseek(fis,0,SEEK_SET);
	char *buf=(char*)safeAlloc((size_t)n+1);
	size_t nRead=fread(buf,sizeof(char),(size_t)n,fis);
	fclose(fis);
	if(n!=nRead)err("cannot read all the content of %s",fileName);
	buf[n]='\0';
	return buf;
	}

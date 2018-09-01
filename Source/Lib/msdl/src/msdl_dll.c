/***********************************************************************
 *    main.c:  cli specific stuff
 ***********************************************************************
 * Copyright (C) 2007 metro <me_t_ro@yahoo.com>
 *
 * This file is part of msdl, media stream downloader
 * See README for program usage and information.
 * See COPYING for license information.
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston MA 02110-1301, USA.
 *
 ***********************************************************************/



#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <ctype.h>
#include <unistd.h>
#include <getopt.h>
#include <time.h>

#include "msdl.h"
#include "msdllib.h"
#include "display.h"
#include "network.h"
#include "config.h"

#ifdef WIN32
#include <windows.h>
#endif

static void print_usage(void);
#ifdef DLL
static struct list_h *set_options(struct options_t *options, int DllOptionCount, DllOption *DllOptions);
#else
static struct list_h *set_options(struct options_t *options,int argc,char **argv);
#endif

/*
 * msdl -- media stream downloader ---
 * main entry point
 */
#ifdef DLL
int main(int argc, char **argv) 
{
  return 0;
}

int MsdlMain(int tag, DownloadProgressCallback callback, int DllOptionCount, DllOption *DllOptions)
#else
int main(int argc,char **argv)
#endif
{
#ifdef WIN32
  WORD version;
  WSADATA wsaData;

  version = MAKEWORD(1, 1);
  WSAStartup(version, &wsaData);
#endif

    struct list_h *targets = NULL,*target;
    struct dlresult_t *result;
    struct options_t *options;
    

    int success = 0;
    int failed = 0;
  
    /* for displaying finished time */
    time_t start_time,fin_time,diffsec;
    
    start_time = time(NULL);
    
    options = new_options_t();
#ifdef DLL
    options->tag = tag;
    options->downloadProgressCallback = callback;
    targets = set_options(options, DllOptionCount, DllOptions);
#else
    targets = set_options(options,argc,argv);
#endif
    init_dispinfo(options);

#ifndef DLL
    display(MSDL_DBG,"%s %s\n",PACKAGE,VERSION);
#endif

    result  = new_dlresult_t();
    for(target = targets; target ; target = target->next) {
  
	msdl(target->p,options,result);
	
    }
    
    fin_time = time(NULL);
  
    success = list_h_num(result->success_list);
    failed = list_h_num(result->failed_list);

    diffsec = fin_time - start_time;
    
    display(MSDL_NOR,"FINISHED --%02d:%02d:%02d--\n",
	    diffsec/3600,(diffsec%3600)/60,diffsec % 60);
    
    /* don't display anything if only one file target */
    if(success > 1) {
	display(MSDL_NOR,"%d files downloaded\n",success);
    }
    else if(success == 0 && failed > 0) {
	display(MSDL_NOR,"ALL download failed\n");
    }


    /* print failed log so that later user can copy-and-paste :) */
    if(failed > 0) {
	struct list_h *p = NULL;
	display(MSDL_NOR,"fialed: %d files\n",list_h_num(result->failed_list));
	for(p = result->failed_list ; p ; p = p->next) {
	    display(MSDL_NOR,"%s\n",p->p);
	}
    }
  
    free_options_t(options);
    free_dlresult_t(result);
    
    clean_dispinfo();
    
#ifdef WIN32
  WSACleanup();
#endif
    return failed;
}



char *package_info = 
    PACKAGE " " VERSION " : Media Stream DownLoader\n";



char *usage =
    "Usage: " PACKAGE " [options] targets\n"
    "  -o, --output <localfile>    specify output file name. '-o -': stdout.\n"
    "  -l, --logfile <logfile>     print log to logfile instead of console.\n"
    "  -v, --verbose               show verbose messages.\n"
    "  -V, --version               show version information.\n"
    "  -q, --quiet                 do quietly. no output.\n"
    "  -c, --continue <url>        continue (resume) downloading.\n"
    "  -a, --auto-retry <num>      auto-retry aborted download. (num times).\n"
    "  -b, --bandwidth <bandwidth> set bandwidth.\n"
    "  -s, --speed <speed>         set streaming speed.\n"
    "  -r, --range <range>         set range (for RTSP), default \"0.000-\".\n"
    "  -m, --metafile <url>        treat url as metafile.\n"
    "  -n, --no-metafile <url>     DO NOT treat url as metafile.\n"
    "  -h, --help                  display this help.\n"
    "  -p, --protocol <protocol>   specify download protocol.\n"
    "      --byterange <range>     set byte range (for http and ftp).\n"
    "      --username <username>   user name for basic authentication.\n"
    "      --password <password>   password for basic authentication.\n"
    "      --no-proxy              DO NOT use proxy, even if HTTP_PROXY set.\n"
    "      --no-passive-ftp        DO NOT use passive mode in FTP.\n"
    "      --stream-timeout <time> Quit Streaming after time.\n"
    "      --debug                 show debug message (super verbose).\n"
    "Supported protocols: mms(mmst) mmsh rtsp http ftp\n"
    "To use proxy for mms, mmsh, and http, set HTTP_PROXY variable.\n"
    "\"-s\" and \"-r\" are for *-rtsp, and these options may be ignored.\n"
    "If you find any bugs, please report to <" PACKAGE_BUGREPORT ">.\n";




/*
 * print package info
 */
static void print_package_info(void)
{
    display(MSDL_NOR,"%s",package_info);
}



/*
 * print usage
 */
static void print_usage(void)
{
    display(MSDL_NOR,"%s",usage);
}



/*
 * read arguments. returns target url
 */
#ifdef DLL
static struct list_h *set_options(struct options_t *options, int DllOptionCount, DllOption *DllOptions)
#else
static struct list_h *set_options(struct options_t *options,int argc,char **argv)
#endif
{
    int ch = 0;
    int option_index = 0;
    int target_count = 0; /* argument count */
    struct list_h *targets = NULL;
    struct target_t *t = NULL;

#ifndef DLL
    struct option long_options[] = {
	{"output",1,0,'o'},      /* equal to '-o' */
	{"logfile",1,0,'l'},     /*          '-l' */
	{"protocol",1,0,'p'},    /*          '-p' */
	{"quiet",0,0,'q'},       /*          '-q' */
	{"bandwidth",1,0,'b'},   /*          '-b' */
	{"speed",1,0,'s'},       /*          '-s' */
	{"range",1,0,'r'},       /*          '-r' */
	{"continue",0,0,'c'},    /*          '-c' */
	{"auto-retry",1,0,'a'},  /*          '-a' */
	{"metafile",1,0,'m'},    /*          '-m' */
	{"no-metafile",1,0,'n'}, /*          '-n' */
	{"help",0,0,'h'},        /*          '-h' */
	{"verbose",0,0,'v'},     /*          '-v' */
	{"version",0,0,'V'},     /*          '-V' */
	{"byterange",1,0,0},
	{"username",1,0,0},
	{"password",1,0,0},
	{"no-proxy",0,0,0},       /* turn off proxy, no matter HTTP_PROXY set  */
	{"no-passive-ftp",0,0,0}, /* do not use passive mode in ftp            */
	{"stream-timeout",1,0,0}, /* force stream finish by time @arg (ex. 20s */

	{"debug",0,0,0},          /* debug messages, super verbose mode        */
	{NULL,0,0,0}
    };
#endif
    char *env = NULL;
    
    
    /*
      set default options
     */
    options->loglevel = MSDL_NOR; /* normal */
    options->no_passive_ftp_f = 0;

    
    /*
      get environment variable
    */
    if((env = getenv("HTTP_PROXY"))) {
	options->http_proxy = strdup(env);
    }
    
    
    /*
      set command line options
    */
#ifdef DLL
    for (option_index = 0; option_index < DllOptionCount; option_index++) {

        ch = DllOptions[option_index].shortOpt;
        optarg = DllOptions[option_index].optArg;

        switch(ch) {

        case 0:   /* URL to download */
	    t = new_target_t(optarg,0);
	    list_h_append(&targets,t);
	    target_count++;
            break;

#else
    while(1) {

	ch = getopt_long(argc,argv,"o:l:p:b:s:r:a:m:n:hcVvq",long_options,&option_index);
    
	if(ch == -1) /* end of options */
	    break;
    
	switch(ch) {
      
	case 0:   /* long options, such as --help */
	    
	    if(!strcmp(long_options[option_index].name,"username")) {
		if(options->username) free(options->username);
		options->username = strdup(optarg);
	    }
	    else if(!strcmp(long_options[option_index].name,"password")) {
		if(options->password) free(options->password);
		options->password = strdup(optarg);
	    }
	    else if(!strcmp(long_options[option_index].name,"byterange")) {
		if(options->byterange) free(options->byterange);
		options->byterange = strdup(optarg);
	    }
	    else if(!strcmp(long_options[option_index].name,"no-proxy")) {
		if(options->http_proxy) free(options->http_proxy);
		options->http_proxy = NULL;
	    }
	    else if(!strcmp(long_options[option_index].name,"no-passive-ftp")) {
		options->no_passive_ftp_f = 1;
	    }
	    else if(!strcmp(long_options[option_index].name,"stream-timeout")) {
		if(options->stream_timeout) free(options->stream_timeout);
		options->stream_timeout = strdup(optarg);
	    }
	    else if(!strcmp(long_options[option_index].name,"debug")) {
		options->loglevel = MSDL_DBG;
		dispinfo_set_loglevel(MSDL_DBG);
	    }
	    
	    break;
#endif
      
	case 'p': /* specify protocol               */
	    if(options->protocol) free(options->protocol);
	    options->protocol = strdup(optarg);
	    break;
      
	case 'o': /* output file name specification */
	    list_h_append(&(options->local_filename_list),strdup(optarg));
	    break;

	case 'l':
	    if(options->logfile) free(options->logfile);
	    options->logfile = strdup(optarg);
	    break;
      
	case 'b': /* bandwidth                      */
	    options->bandwidth = atoi(optarg);
	    break;
	    
	case 's': /* speed (for rtsp)               */
	    if(options->speed) free(options->speed);
	    options->speed = strdup(optarg);
	    break;

	case 'r': /* range (for rtsp)               */
	    if(options->range) free(options->range);
	    options->range = strdup(optarg);
	    break;

	case 'c': /* resume download target         */
	    options->resume = 1;
	    break;
	    
	case 'a':
	    options->auto_retry_times = atoi(optarg);
	    if(options->auto_retry_times == 0) { /* if argument == 0, set default value */
		if(!strcmp(optarg,"inf")) {
		    options->auto_retry_times = AUTO_RETRY_INFINITE;
		}
		else {
		    options->auto_retry_times = AUTO_RETRY_DEFAULT;
		}
	    }
	    break;

	case 'm': /* treat as metafile              */
	    t = new_target_t(optarg,IS_METAFILE);
	    list_h_append(&targets,t);
	    target_count++;
	    break;
	    
	case 'n': /* do NOT treat as metafile       */
	    t = new_target_t(optarg,FORCE_NOT_METAFILE);
	    list_h_append(&targets,t);
	    target_count++;
	    break;
	    
	case 'h': /* help                           */
	    print_package_info();
	    print_usage();
	    goto exit_now;
	    break;

	case 'V': /* version                        */
	    print_package_info();
	    goto exit_now;
	    break;

	case 'v': /* verbose                        */
	    if(options->loglevel != MSDL_DBG) { /* MSDL_DBG has more priority */
		options->loglevel = MSDL_VER;
		dispinfo_set_loglevel(MSDL_VER);
	    }
	    break;
	    
	case 'q': /* quiet                          */
	    /* when met option, don't display anything */
	    options->loglevel = MSDL_QUIET;
	    dispinfo_set_loglevel(MSDL_QUIET);
	    break;
	    
	default:
	    display(MSDL_ERR,"cannot recognize option '%c'\n",ch);
	    print_usage();
	    goto exit_now;
	}
    }

#ifndef DLL
    if(optind < argc) {
    
	while(optind < argc) {
	    t = new_target_t(argv[optind++],0);
	    list_h_append(&targets,t);
	    target_count++;
	}
    }
#endif
  
    options->targets = targets;
    
  
    if(target_count == 0) {
	display(MSDL_ERR,"no target\n");
	print_usage();
	goto exit_now;
    }
  
    /*
      show warnings
    */
    
    if(options->protocol && target_count > 1) {
	display(MSDL_ERR,
		"warning: protocol specified but trying to \n"
		"         download more than 1 files. keep \n"
		"         in mind that protocol you specified\n"
		"         will be applied to all targets, thus\n"
		"         some files can not be downloaded.\n");
    }

    if(options->bandwidth) {
	display(MSDL_ERR,
		"warning: you have set bandwidth to %d\n"
		"         for this setting, you might not receive\n"
		"         best quality stream.\n",options->bandwidth);
    }

    if(options->auto_retry_times == AUTO_RETRY_INFINITE) {
	display(MSDL_NOR,
		"warning: you have set download tries to infinite\n");
    }

    return targets;

  exit_now:
    free_options_t(options);
    exit(1);
}

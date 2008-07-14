#include <dlfcn.h>

#include <time.h>
#include <inttypes.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

/* how many times to speed up the time-space continuum */
#define FACTOR  4

static void *libc;
static int (*old_usleep      )(useconds_t);
static int (*old_gettimeofday)(struct timeval*, void*);

void init()
{
    if(!(libc = dlopen("libc.so.6", RTLD_LAZY))) {
        fprintf(stderr, "error loading libc\n");
        abort();
    }

    if(!(old_usleep = dlsym(libc, "usleep"))) {
        fprintf(stderr, "error finding usleep\n");
        abort();
    }

    if(!(old_gettimeofday = dlsym(libc, "gettimeofday"))) {
        fprintf(stderr, "error finding gettimeofday\n");
        abort();
    }
}

void fini()
{
    if(libc)
        dlclose(libc);
}

int usleep(useconds_t usec)
{
    if(!old_usleep) init();
    return old_usleep(usec / FACTOR);
}

int gettimeofday(struct timeval *tv, void *tz)
{
    uint64_t tmp;
    int ret;

    if(!old_gettimeofday) init();
    if((ret = old_gettimeofday(tv, tz)) == -1)
        return ret;

    tmp         = tv->tv_sec*1000000 + tv->tv_usec;
    tmp        *= FACTOR;
    tv->tv_sec  = tmp / 1000000;
    tv->tv_usec = tmp % 1000000;

    return ret;
}


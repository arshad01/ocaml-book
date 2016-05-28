#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/memory.h>

#ifndef _WC_H_
#define _WC_H_

typedef struct _wc_stats_t
{
    int nawords;  /* Count of all words */
    int nuwords;  /* Count of unique words */
    int nrwords;  /* Count of repeated words */
    int nchars;
    int nlines;
} wc_stat_t;

wc_stat_t calc_stats (FILE *);
int       calc_words (char *);
int       wc_main (int argc, char const *argv[]);
value     wc_c (value);
#endif
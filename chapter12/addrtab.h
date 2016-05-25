#include <stdlib.h>
#include <string.h>

#ifndef _ADDRTAB_H_
#define _ADDRTAB_H_

/*
  Simple hashtable implementation for storing addresses.
*/
typedef struct _addr_list_t_ {
    unsigned long   key;
    char *          val;
    int             count;
    unsigned char   printed;
    struct _addr_list_t_ *next;
} addr_list_t;

typedef struct _hash_table_t_ {
    int size;
    addr_list_t **table;
} hash_table_t;

hash_table_t    *create_tab(int);
void            free_tab(hash_table_t *);
addr_list_t     *get(hash_table_t *, unsigned long key);
int             put (hash_table_t *, unsigned long key, char * name);
unsigned char   hash(hash_table_t *, unsigned long);

#endif
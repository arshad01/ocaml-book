#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include "addrtab.h"

#define PASS1       1
#define PASS2       2

#define MINCYCCNT   5

#define TRUE        1
#define FALSE       0

value print_ws       (value);
void  print_value    (value, int, hash_table_t*);
void  print_string   (value);
void  print_closure  (value, int, hash_table_t*);
value make_ht        (void);
void  fin_ht         (value);

value print_ws (value v)
{
    value ht_block = make_ht ();

    hash_table_t *ht = (hash_table_t *) Field(ht_block, 1);

    print_value (v, PASS1, ht);
    print_value (v, PASS2, ht);
    fflush(stdout);
    return Val_unit;
}

void print_value (value v, int pass, hash_table_t *ht)
{
    int size, i, n, ret;
    unsigned long key;
    char buf[256];
    addr_list_t* entry;

    if (Is_long(v))
    {
        if (pass == PASS2)
            printf("%ld ", Long_val(v));
        return;
    }

    size=Wosize_val(v);

    switch (Tag_val(v))
    {
        case Closure_tag:
            print_closure (v, pass, ht);
            break;

        case String_tag:
            print_string(v);
            break;

        case Double_tag:
            if (pass == PASS2)
                printf("%g ", Double_val(v));
            break;

        case Double_array_tag:
            if (pass == PASS2)
            {
                printf("[| ");
                n = size/Double_wosize;
                for (i=0; i<n; i++)
                {
                    printf("%g", Double_field(v,i));
                    if (i < (n-1))
                        printf("; ");
                    else
                        printf(" ");
                }
                printf("|]"); 
            }
            
            break;

        case Abstract_tag:
            if (pass == PASS2)
                printf("(abstract) ");
            break;

        case Custom_tag:
            if (pass == PASS2)
                printf("(custom) ");
            break;

        default:
            if (pass == PASS2 && Tag_val(v) >= No_scan_tag)
            {
                printf("(unknown) ");
                break;
            };

            /*
                For structured values, PASS1 gathers information about addresses and
                PASS2 prints it. We use MINCYCCNT as a threshold for printing cyclic/shared
                values. The name of the value is just its stringified address.
            */
            if (pass == PASS1)
            {
                key = (unsigned long)v;
                entry = get(ht, key);
                if ((entry == NULL) || (entry->count < MINCYCCNT))
                {
                    buf[0] = '\0';
                    sprintf(buf,"var_%lx",key);
                    put(ht, key, strdup(buf));
                }

                for (i=0; i<size; i++)
                {
                    key = (unsigned long)Field(v,i);
                    entry = get(ht, key);
                    if ((entry == NULL) || (entry->count < MINCYCCNT))
                        print_value(Field(v,i), pass, ht);
                }     
            }
            else if (pass == PASS2)
            {
                key = (unsigned long)v;
                entry = get(ht, key);
                if ((entry != NULL) && (entry->count >= MINCYCCNT))
                {
                    printf("(v=%s) ", entry->val);

                    if (entry->printed == FALSE)
                    {
                        entry->printed = TRUE;
                        printf("( ");
                        for (i=0; i<size; i++)
                        {
                            print_value(Field(v,i), pass, ht);
                            if (i < (size-1))
                            printf(", ");
                        }
                        printf(") ");
                    }
                } else  
                {
                    printf("( ");
                    for (i=0; i<size; i++)
                    {
                        print_value(Field(v,i), pass, ht);
                        if (i < (size-1))
                        printf(", ");
                    }
                    printf(") ");
                }
            }            
    }
    return;     
}

void print_string(value v)
{
    char *s;
    int i, size;

    s = (char *) v;
    size = string_length(v);

    printf("\"");
    for (i=0; i<size; i++)
    {
        unsigned char p = Byte_u(s, i);
        if ((p > 31) && (p < 128))
            printf("%c", s[i]);
        else
            printf("%u", p);
    }
    printf("\"");
    return;
}

void print_closure (value v, int pass, hash_table_t* ht)
{
    int i,size;

    size=Wosize_val(v);

    if (pass == PASS2)
    {
        printf("< %p", Code_val(v));
        if (size > 1) 
        {
            printf(", ");
            for (i=1; i<size; i++)
            {
                print_value(Field(v,i), pass, ht);
                if (i < size-1)
                    printf(", ");
            }
                
        }
            
        printf(" > ");
    }
    
    return;
}

value make_ht (void)
{
    value block;

    hash_table_t *ht = create_tab(256);

    if (ht != NULL)
    {
        block = alloc_final(2, fin_ht, sizeof(addr_list_t *) * 256, 100000);
        Field(block,1)=(value)ht;
        return block;
    }
    
    failwith ("Could not allocate address table.");

    return block;
}

void fin_ht (value v)
{
    hash_table_t *ht = (hash_table_t *) Field(v,1);
    free_tab(ht);
    //printf("hashtable gc'd\n");
    //fflush(stdout);
}




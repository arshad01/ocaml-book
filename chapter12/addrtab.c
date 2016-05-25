#include "addrtab.h"

hash_table_t *create_tab(int size)
{
    int i;
    hash_table_t *tab;

    if (size < 1)
        return NULL;

    tab = malloc(sizeof(hash_table_t));
    if (tab == NULL)
        return NULL;

    tab->table = malloc(sizeof(addr_list_t *) * size);
    if (tab->table == NULL)
        return NULL;

    tab->size = size;
    for (i=0; i < size; i++)
        tab->table[i] = NULL;

    return tab;
}

void free_tab(hash_table_t *ht)
{
    int i;
    addr_list_t *list, *temp;

    if (ht == NULL)
        return;

    for (i=0; i < ht->size; i++)
    {
        list = ht->table[i];
        while (list != NULL)
        {
            temp = list;
            list = list->next;
            free (temp->val);
            free (temp);
        }
    }

    free (ht->table);
    free (ht);
}

addr_list_t *get(hash_table_t *ht, unsigned long key)
{
    addr_list_t *list;
    unsigned char hval;

    hval = hash(ht, key);

    for (list=ht->table[hval]; list != NULL; list=list->next)
        if (key == list->key)
            return list;

    return NULL;
}

int put(hash_table_t *ht, unsigned long key, char * name)
{
    addr_list_t *new_entry, *cur_entry;

    cur_entry = get(ht, key);

    if (cur_entry != NULL)
    {
        cur_entry->count++;
        return 2;
    }

    new_entry = malloc(sizeof(addr_list_t));
    if (new_entry == NULL)
        return 1;

    unsigned char hval = hash(ht, key);

    new_entry->key = key;
    new_entry->val = strdup(name);
    new_entry->count = 1;
    new_entry->printed = 0;
    new_entry->next = ht->table[hval];
    ht->table[hval] = new_entry;

    return 0;
}

/*
  One-At-a-Time hash function
*/
unsigned char hash(hash_table_t *ht, unsigned long key)
{
    int len = sizeof(unsigned long);
    int i;
    unsigned char h = 0;

    for (i=0; i < len; i++) 
    {
        h += (key >> (i*8)) & 0xF;
        h += (h << 10);
        h ^= (h >> 6);
    }

    h += (h << 3);
    h ^= (h >> 11);
    h += (h << 15);

    return h % ht->size;
}

#ifdef TEST_HTAB
#include <stdio.h>

int main(int argc, char const *argv[])
{
    addr_list_t* l;

    unsigned long a1 = 0x7f8d7950086c;
    unsigned long a2 = 0x7f8d79500870;
    unsigned long a3 = 0x7f8d7950087d;

    hash_table_t* ht = create_tab(256);
    put(ht, a1, "addr1");
    put(ht, a2, "addr2");
    put(ht, a2, "addr2");

    l = get(ht, a1);
    if (l != NULL)
        printf("passed: addr=%lx, name=%s, count=%d\n",l->key,l->val,l->count);
    else
        printf("failed: address not found.\n");

    l = get(ht, a2);
    if (l != NULL)
        printf("passed: addr=%lx, name=%s, count=%d\n",l->key,l->val,l->count);
    else
        printf("failed: address not found.\n");

    l = get(ht, a3);
    if (l != NULL)
        printf("failed: addr=%lx, name=%s, count=%d\n",l->key,l->val,l->count);
    else
        printf("passed: address not found.\n");

    free_tab(ht);

    return 0;
}
#endif












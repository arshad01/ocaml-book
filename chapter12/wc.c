#include "wc.h"

wc_stat_t calc_stats (FILE *fp)
{
    char *line = NULL;
    size_t len = 0;
    ssize_t read;
    wc_stat_t stats;

    stats.nchars  = 0;
    stats.nawords = 0;
    stats.nuwords = 0;
    stats.nrwords = 0;
    stats.nlines  = 0;

    CAMLlocal1(num_repeated_words);
    CAMLlocal1(num_unique_words);
    CAMLlocal1(clear);
    CAMLlocal1(nrw);
    CAMLlocal1(nuw);

    num_repeated_words = *(caml_named_value("num_repeated_words"));
    num_unique_words = *(caml_named_value("num_unique_words"));
    clear = *(caml_named_value("clear"));

    callback(clear, Val_unit); /* Reset hashtable before processing each file */

    while ((read = getline(&line, &len, fp)) != -1)
    {
        stats.nchars += (int)read;
        stats.nlines++;
        stats.nawords += calc_words(line);

        if (line)
        {
            free (line);
            line = NULL;
        }
    }

    nuw = callback(num_unique_words, Val_unit);
    nrw = callback(num_repeated_words, Val_unit);

    stats.nuwords = Long_val(nuw);
    stats.nrwords = Long_val(nrw); 

    return stats;
}

int calc_words (char *line)
{
    int nwords=0;
    int inword=0;
    const char* it = line;
    const char* st = it;
    char word[1024];
    int  wlen;

    CAMLlocal1(add_word);
    CAMLlocal1(oc_word);

    add_word = *(caml_named_value("add_word"));
    
    word[0] = '\0';

    do 
    {
        switch (*it)
        {
            case '\0':
            case ' ' :
            case '\t':
            case '\n':
            case '\r':
                if (inword)
                {
                    inword = 0;
                    nwords++;

                    wlen = (it-st);
                    (void)strncpy(word, st, wlen);
                    word[wlen] = '\0';

                    oc_word = copy_string(word);
                    callback(add_word, oc_word);

                    st = it;
                } 
                st++;
                break;

            default:
                inword=1;
        }
    } while(*it++);

    return nwords;
}

int wc_main(int argc, char const *argv[])
{
    FILE *fp;
    int i;
    const char *fname;
    wc_stat_t stats;
    wc_stat_t total;

    total.nchars  = 0;
    total.nawords = 0;
    total.nrwords = 0;
    total.nuwords = 0;
    total.nlines  = 0;

    if (argc == 1)
    {
        fp = stdin;
        stats = calc_stats(fp);
        printf("%8d%8d (unq=%-4d, rep=%-4d) %8d\n", 
            stats.nlines, stats.nawords, stats.nuwords, stats.nrwords, stats.nchars);
        fflush(stdout);
    } else
    {
        for (i=1; i<argc; i++)
        {
            fname = argv[i];
            fp = fopen(fname, "r");
            if (fp == NULL)
            {
                printf("failed to open file %s\n", fname);
                fflush(stdout);
            } else
            {
                stats = calc_stats(fp);

                total.nchars  += stats.nchars;
                total.nawords += stats.nawords;
                total.nrwords += stats.nrwords;
                total.nuwords += stats.nuwords;
                total.nlines  += stats.nlines;

                printf("%8d%8d (unq=%-4d, rep=%-4d) %8d %s\n", 
                    stats.nlines, stats.nawords, stats.nuwords, stats.nrwords, stats.nchars, fname);
                fflush(stdout);
                fclose(fp);
            }
        }

        if (argc > 2)
        {
           printf("%8d%8d (unq=%-4d, rep=%-4d) %8d total\n", 
                total.nlines, total.nawords, total.nuwords, total.nrwords, total.nchars); 
           fflush(stdout);
        }
    }

    return 0;
}

value wc_c (value args)
{
    CAMLparam1(args);
    int i, argc;
    char **argv;

    argc = Wosize_val(args);

    if (argc > 0)
    {
        argv = malloc(sizeof(char *) * argc);

        for (i=0; i<argc; i++)
        {
            argv[i] = strdup(Bp_val(Field(args, i)));
        }

        wc_main (argc, (const char **)argv);

        for (i=0; i<argc; i++)
        {
            free(argv[i]);
        }

        free(argv);
    } else
    {
        wc_main(1,NULL);
    }

    CAMLreturn(Val_unit);
}

#ifdef NAT
int main(int argc, char const *argv[])
{
    char * caml_argv[argc+1];
    int i;

    CAMLlocal1(wc_ocaml);

    for (i=0; i<argc; i++)
        caml_argv[i] = (char *)argv[i];
    caml_argv[argc] = NULL;

    caml_main(caml_argv);

    wc_ocaml = *(caml_named_value("wc_ocaml"));

    callback(wc_ocaml, Val_unit);

    return 0;
}
#endif
















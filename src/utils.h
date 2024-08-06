#ifndef UTILS_H
#define UTILS_H
#include <Rinternals.h>

#define PBSTR ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
#define PBWIDTH 60

typedef struct
{
        int n;
        union
        {
                double *d;
                int *i;
                int *l;
                const char *s;
                SEXP *rSexp;
        } Element;

} ListElement;

ListElement fGetListElement(SEXP list, const char *str);

extern "C" void printProgress(double *percentage, char **var);
#endif


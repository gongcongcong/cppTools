#include <stdio.h>
#include <string.h>
#include <Rinternals.h>

#define PBSTR ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
#define PBWIDTH 60

extern "C" void printProgress(double percentage, char *var) {
        double val = percentage * 100.0;
        int lpad = (int) (percentage * PBWIDTH);
        int rpad = PBWIDTH - lpad;
        if (strcmp(var, "") == 0) {
                var = (char *)"*";
        }
        printf("\r%2.2f%% (%s) [%.*s%*s]", val, var, lpad, PBSTR, rpad, "");
        fflush(stdout);
}


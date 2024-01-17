#include <math.h>
#include <stdio.h>
#include "utils.h"

extern "C" void R_eGFR(double *pcr, double *age, double *sex, int *n, double *egfr, int *unit) {
        for (int i = 0; i < *n; i++) {
                double p = (double)(i + 1) / (*n);
                printProgress(p, (char *)"");
                if (*unit == 1) {//1 == mg/dL
                        egfr[i] = 186 * pow(pcr[i], -1.154) * pow(age[i], -0.203) * 1.227 * sex[i];
                } else {
                        egfr[i] = 186 * pow(pcr[i] * 0.011312, -1.154) * pow(age[i], -0.203) * 1.227 * sex[i];
                }
        }
}



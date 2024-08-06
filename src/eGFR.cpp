#include <math.h>
#include <stdio.h>
#include "utils.h"
#include <string.h>
#define R_NO_REMAP

extern "C" SEXP R_eGFR(SEXP pcr, SEXP age, SEXP sex, SEXP unit) {
        int n = Rf_length(pcr);
        if (Rf_length(age) != Rf_length(sex)) {
                Rf_error("The number of Age and Sex vector is not same");
        }
        if (Rf_length(age) != n) {
                Rf_error("The number of Age and PCR vector is not same");
        }
        SEXP egfr = PROTECT(allocVector(REALSXP, n));
        for (int i = 0; i < n; i++) {
                if (strcmp(CHAR(STRING_ELT(unit, 0)), "mg/dL") == 0) {//1 == mg/dL
                        REAL(egfr)[i] = 186 * pow(REAL(pcr)[i], -1.154) * pow(REAL(age)[i], -0.203) * 1.227 * REAL(sex)[i];
                } else {
                        REAL(egfr)[i] = 186 * pow(REAL(pcr)[i] * 0.011312, -1.154) * pow(REAL(age)[i], -0.203) * 1.227 * REAL(sex)[i];
                }
        }
        UNPROTECT(1);
        return egfr;
}



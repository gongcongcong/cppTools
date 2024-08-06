#include <Rinternals.h>
#include <R.h>
#include <R_ext/Rdynload.h>

void R_eGFR(double *pcr, double *age, double *sex, int *n, double *egfr, char **unit);
SEXP R_shoelace(SEXP x, SEXP y);
SEXP R_alpha_count(SEXP file, SEXP seq_min, SEXP seq_max, SEXP verbose);
void printProgress(double *percentage, char **var);

static const R_CMethodDef DotCEntries[] = {
        {"R_eGFR", (DL_FUNC) &R_eGFR, 6},
        {"printProgress", (DL_FUNC) &printProgress, 2},
        {NULL, NULL, 0}
};

static const R_CallMethodDef CallEntries[] = {
        {"R_shoelace", (DL_FUNC) &R_shoelace, 2},
        {"R_alpha_count", (DL_FUNC) &R_alpha_count, 4},
        {NULL, NULL, 0}
};

void R_init_cppTools(DllInfo *dll)
{
        R_registerRoutines(dll, DotCEntries, CallEntries, NULL, NULL);
        R_useDynamicSymbols(dll, FALSE);
}

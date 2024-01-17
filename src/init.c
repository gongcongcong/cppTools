#include <Rinternals.h>
#include <R_ext/Rdynload.h>

void R_matrix_test(int *n);
void R_eGFR(double *pcr, double *age, double *sex, int *n, double *egfr, int *unit);
SEXP R_shoelace(SEXP x, SEXP y);
SEXP R_alpha_count(SEXP file, SEXP seq_min, SEXP seq_max, SEXP verbose);

static const R_CMethodDef DotCEntries[] = {
        {"R_eGFR", (DL_FUNC) &R_eGFR, 6},
        {"R_matrix_test", (DL_FUNC) &R_matrix_test, 1},
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

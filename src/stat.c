#include <R.h>
#include <Rinternals.h>
#define R_NO_REMAP

SEXP R_mode(SEXP x) {
        if (TYPEOF(x) != REALSXP) {
                Rf_error("x must be numeric");
        }

        int len = LENGTH(x);
        double *data = REAL(x);

        // Create a hash table to store frequencies of each number
        SEXP table = PROTECT(Rf_allocVector(REALSXP, len));
        SEXP counts = PROTECT(Rf_allocVector(INTSXP, len));
        double *table_data = REAL(table);
        int *count_data = INTEGER(counts);

        int unique_count = 0;
        for (int i = 0; i < len; i++) {
                int found = 0;
                for (int j = 0; j < unique_count; j++) {
                        if (table_data[j] == data[i]) {
                                count_data[j]++;
                                found = 1;
                                break;
                        }
                }
                if (!found) {
                        table_data[unique_count] = data[i];
                        count_data[unique_count] = 1;
                        unique_count++;
                }
        }

        // Find the maximum frequency
        int max_count = 0;
        for (int i = 0; i < unique_count; i++) {
                if (count_data[i] > max_count) {
                        max_count = count_data[i];
                }
        }

        // Find how many numbers have the maximum frequency
        int mode_count = 0;
        for (int i = 0; i < unique_count; i++) {
                if (count_data[i] == max_count) {
                        mode_count++;
                }
        }

        // Collect all modes
        SEXP result = PROTECT(Rf_allocVector(REALSXP, mode_count));
        double *result_data = REAL(result);
        int index = 0;
        for (int i = 0; i < unique_count; i++) {
                if (count_data[i] == max_count) {
                        result_data[index++] = table_data[i];
                }
        }

        UNPROTECT(3);
        return result;
}

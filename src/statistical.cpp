#include <Rinternals.h>
#include "utils.h"
#include <R.h>
#include <stdio.h>
#include <string.h>
#define R_NO_REMAP

extern "C" SEXP R_shoelace(SEXP x, SEXP y)
{
    if (Rf_isNumeric(x) == 0 || Rf_isNumeric(y) == 0) {
        Rf_error("'x' and 'y' must be numeric vectors");
        return R_NilValue;
    }
    int n = Rf_length(x);
    if (n != Rf_length(y)) {
        Rf_error("'x' and 'y' must have the same length");
        return R_NilValue;
    }
    double ret = 0;
    for (int i = 0; i < n - 1; i++) {
        ret += REAL(x)[i] *  REAL(y)[i + 1] -   REAL(y)[i] * REAL(x)[i + 1];
    }
    ret += REAL(x)[n - 1] * REAL(y)[0] - REAL(y)[n - 1] * REAL(x)[0];
    ret = ret >= 0 ? ret : -ret; // abs
    return ScalarReal(0.5 * ret);
}

extern "C" SEXP R_alpha_count(SEXP file, SEXP seq_min, SEXP seq_max, SEXP verbose)
{
    if (TYPEOF(file) != STRSXP) {
        Rf_error("Input is not a character vector");
        return R_NilValue;
    }
    const char *fileChar = R_CHAR(STRING_ELT(file, 0));
    FILE *fastaFile = fopen(fileChar, "r");
    if (fastaFile == NULL)
    {
        Rf_error("File not found");
        return R_NilValue;
    }
    char line[2048];
    int letterCount[26] = {0};  // Assuming only uppercase letters, adjust if needed
    int iTmpLetterCount[26] = {0};
    int iSequenceLength = 0;
    while (fgets(line, sizeof(line), fastaFile)) {
        if (line[0] == '>')
        {
            if (iSequenceLength >= INTEGER(seq_min)[0] && iSequenceLength <= INTEGER(seq_max)[0])
            {
                for (int i = 0; i < 26; i++)
                {
                    letterCount[i] += iTmpLetterCount[i];
                }
            }
            iSequenceLength = 0;
            memset(iTmpLetterCount, 0, sizeof(iTmpLetterCount));
            continue;
        } else {
            iSequenceLength += strlen(line) - 1;
            for (int i = 0; line[i]; i++)
            {
                if (line[i] >= 'A' && line[i] <= 'Z')
                {
                    iTmpLetterCount[line[i] - 'A']++;
                }
            }
        }
    }
    if (iSequenceLength > INTEGER(seq_min)[0] && iSequenceLength < INTEGER(seq_max)[0])
    {
        for (int i = 0; i < 26; i++)
        {
            letterCount[i] += iTmpLetterCount[i];
        }
    }
    // Display results
    if (LOGICAL(verbose)[0] == TRUE)
    {
        for (int i = 0; i < 26; i++) {
            Rprintf("Letter %c: %d\n", 'A' + i, letterCount[i]);
        }
    }
    fclose(fastaFile);
    SEXP res = PROTECT(allocVector(INTSXP, 26));
    for (int i = 0; i < 26; i++)
    {
        INTEGER(res)[i] = letterCount[i];
    }
    UNPROTECT(1);
    return res;
}




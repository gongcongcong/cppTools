#include <stdio.h>
#include <string.h>
#include <Rinternals.h>
#include "utils.h"

#define R_NO_REMAP

extern "C" void printProgress(double *percentage, char **var) {
        double val = *percentage * 100.0;
        int lpad = (int)(*percentage * PBWIDTH);
        int rpad = PBWIDTH - lpad;

        // Ensure var is not NULL before dereferencing
        const char *label = (*var != NULL) ? *var : "*";

        printf("\r%2.2f%% (%s) [%.*s%*s]", val, label, lpad, PBSTR, rpad, "");
        fflush(stdout);
}


ListElement fGetListElement(SEXP list, const char *str) {
        SEXP rsexpElement = R_NilValue;
        SEXP rsexpListNames = Rf_getAttrib(list, R_NamesSymbol);
        int iListLength = Rf_length(list);
        for (int i = 0; i < iListLength; i++) {
                const char * pzListName = R_CHAR(STRING_ELT(rsexpListNames, i));
                if (strcmp(pzListName, str) == 0) {
                        rsexpElement = VECTOR_ELT(list, i);
                        break;
                }
        }
        if (rsexpElement == R_NilValue)
        {
                Rf_error("'%s' is not the element of list", str);
                return ListElement();
        }

        ListElement ret;
        ret.n = Rf_length(rsexpElement);
        int iTypeOfSexp = TYPEOF(rsexpElement);
        switch (iTypeOfSexp)
        {
        case LGLSXP:
                ret.Element.l = LOGICAL(rsexpElement);
                break;
        case INTSXP:
                ret.Element.i = INTEGER(rsexpElement);
                break;
        case REALSXP:
                ret.Element.d = REAL(rsexpElement);
                break;
        case STRSXP:
                ret.Element.s = R_CHAR(STRING_ELT(rsexpElement, 0));
                break;
        default:
                ret.Element.rSexp = &rsexpElement;
                break;
        }
        return ret;
}

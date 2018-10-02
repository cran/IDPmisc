#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .C calls */
void lwreg(double *, double *, int *, int *, double *, double *, double *);

static const R_CMethodDef CEntries[] = {
    {"lwreg", (DL_FUNC) &lwreg, 7},
    {NULL, NULL, 0}
};

void R_init_IDPmisc(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}

#include "fast_transpose.h"

/* fast_transpose.c
 *
 * Drop-in replacement for base t() on vectors, 1-dim arrays, and matrices.
 *
 *  - n x 1 / 1 x n / 1-D inputs: the transpose is the identity permutation
 *    of the data, so a single contiguous memcpy is used.
 *  - True 2-D case: blocked (tiled) transpose. Input is read in contiguous
 *    bursts (no long-stride access) and output is written contiguously;
 *    each tile stays in L1, and both inner loops are branch-free so they
 *    auto-vectorize at -O3.
 *
 *  Attribute handling matches R's do_transpose(), so results are identical
 *  to t().
 */

#define FT_TILE 32   /* tile side; 32x32 doubles = 8 KB, hot in L1 */

/* Tiled transpose of an nrow x ncol column-major buffer into an
 * ncol x nrow buffer.  `src`/`dst` must be pointers to ElemType
 * (e.g. REAL(a)/REAL(r), (char **)a/(char **)r for STRSXP,
 * (SEXP *)a/(SEXP *)r for VECSXP); `nrow`/`ncol` must be in scope.
 * The source buffer is only read. */
#define FT_BLOCKED_TRANSPOSE(src, dst, ElemType)               \
do {                                                            \
    ElemType * const _fa = (src);                               \
    ElemType * const _fr = (dst);                               \
    for (R_xlen_t p0 = 0; p0 < nrow; p0 += FT_TILE) {           \
        const R_xlen_t pb = nrow - p0 < FT_TILE ? nrow - p0 : FT_TILE; \
        for (R_xlen_t q0 = 0; q0 < ncol; q0 += FT_TILE) {       \
            const R_xlen_t qb = ncol - q0 < FT_TILE ? ncol - q0 : FT_TILE; \
            ElemType tile[FT_TILE][FT_TILE];                    \
            for (R_xlen_t qq = 0; qq < qb; qq++)                \
                for (R_xlen_t pp = 0; pp < pb; pp++)            \
                    tile[pp][qq] = _fa[(q0 + qq) * nrow + (p0 + pp)]; \
            for (R_xlen_t pp = 0; pp < pb; pp++)                \
                for (R_xlen_t qq = 0; qq < qb; qq++)            \
                    _fr[(p0 + pp) * ncol + (q0 + qq)] = tile[pp][qq]; \
        }                                                       \
    }                                                           \
} while (0)

SEXP fast_transpose(SEXP a)
{
    SEXP r, dims, dimnames, dimnamesnames = R_NilValue,
         ndimnamesnames, rnames, cnames;
    int ldim;
    R_xlen_t len, nrow, ncol;

    if (!isVector(a))
        error("fast_transpose: argument is not a vector");

    dims = getAttrib(a, R_DimSymbol);
    ldim = dims == R_NilValue ? 0 : length(dims);
    rnames = cnames = dimnames = R_NilValue;

    switch (ldim) {
    case 0:
        len = nrow = XLENGTH(a);
        ncol = 1;
        rnames = getAttrib(a, R_NamesSymbol);
        dimnames = rnames;             /* so isNull() below works */
        break;
    case 1:
        len = nrow = XLENGTH(a);
        ncol = 1;
        dimnames = getAttrib(a, R_DimNamesSymbol);
        if (dimnames != R_NilValue) {
            rnames = VECTOR_ELT(dimnames, 0);
            dimnamesnames = getAttrib(dimnames, R_NamesSymbol);
        }
        break;
    case 2:
        nrow = nrows(a);
        ncol = ncols(a);
        len = XLENGTH(a);
        dimnames = getAttrib(a, R_DimNamesSymbol);
        if (dimnames != R_NilValue) {
            rnames = VECTOR_ELT(dimnames, 0);
            cnames = VECTOR_ELT(dimnames, 1);
            dimnamesnames = getAttrib(dimnames, R_NamesSymbol);
        }
        break;
    default:
        error("fast_transpose: argument is not a matrix");
    }

    PROTECT(dimnamesnames);
    PROTECT(r = allocVector(TYPEOF(a), len));

    switch (TYPEOF(a)) {
    case LGLSXP: case INTSXP: case REALSXP: case CPLXSXP:
    case RAWSXP: case STRSXP: case VECSXP:
        break;
    default:
        UNPROTECT(2);
        error("fast_transpose: unsupported vector type");
    }

    if (ncol == 1 || nrow == 1) {
        /* Identity permutation: data order is already transposed. */
        if (len > 0)
            switch (TYPEOF(a)) {
            case LGLSXP:
            case INTSXP:
                memcpy(INTEGER(r), INTEGER(a), (size_t) len * sizeof(int));
                break;
            case REALSXP:
                memcpy(REAL(r), REAL(a), (size_t) len * sizeof(double));
                break;
            case CPLXSXP:
                memcpy(COMPLEX(r), COMPLEX(a), (size_t) len * sizeof(Rcomplex));
                break;
            case RAWSXP:
                memcpy(RAW(r), RAW(a), (size_t) len);
                break;
            case STRSXP:
                memcpy((char **) r, (const char **) a,
                       (size_t) len * sizeof(char *));
                break;
            case VECSXP:
                memcpy((SEXP *) r, (const SEXP *) a,
                       (size_t) len * sizeof(SEXP));
                break;
            }
    }
    else {
        /* True 2-D case: blocked transpose. */
        switch (TYPEOF(a)) {
        case LGLSXP:
        case INTSXP:
            FT_BLOCKED_TRANSPOSE(INTEGER(a), INTEGER(r), int);
            break;
        case REALSXP:
            FT_BLOCKED_TRANSPOSE(REAL(a), REAL(r), double);
            break;
        case CPLXSXP:
            FT_BLOCKED_TRANSPOSE(COMPLEX(a), COMPLEX(r), Rcomplex);
            break;
        case RAWSXP:
            FT_BLOCKED_TRANSPOSE(RAW(a), RAW(r), unsigned char);
            break;
        case STRSXP:
            FT_BLOCKED_TRANSPOSE((char **) a, (char **) r, char *);
            break;
        case VECSXP:
            FT_BLOCKED_TRANSPOSE((SEXP *) a, (SEXP *) r, SEXP);
            break;
        }
    }

    PROTECT(dims = allocVector(INTSXP, 2));
    INTEGER(dims)[0] = (int) ncol;
    INTEGER(dims)[1] = (int) nrow;
    setAttrib(r, R_DimSymbol, dims);
    UNPROTECT(1); /* dims */

    if (!isNull(dimnames)) {
        PROTECT(dimnames = allocVector(VECSXP, 2));
        SET_VECTOR_ELT(dimnames, 0, cnames);
        SET_VECTOR_ELT(dimnames, 1, rnames);
        if (!isNull(dimnamesnames)) {
            PROTECT(ndimnamesnames = allocVector(VECSXP, 2));
            SET_VECTOR_ELT(ndimnamesnames, 1, STRING_ELT(dimnamesnames, 0));
            SET_VECTOR_ELT(ndimnamesnames, 0,
                           (ldim == 2) ? STRING_ELT(dimnamesnames, 1)
                                       : R_BlankString);
            setAttrib(dimnames, R_NamesSymbol, ndimnamesnames);
            UNPROTECT(1); /* ndimnamesnames */
        }
        setAttrib(r, R_DimNamesSymbol, dimnames);
        UNPROTECT(1); /* dimnames */
    }

    copyMostAttrib(a, r);
    UNPROTECT(2); /* r, dimnamesnames */
    return r;
}
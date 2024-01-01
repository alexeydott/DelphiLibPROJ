#ifndef PJ_HUGE_VAL_H
#define PJ_HUGE_VAL_H

#include <math.h>

#ifdef __cplusplus
extern "C" {
#endif


#if defined(__BORLANDC__) && defined(__clang__) && defined(_WIN64) && defined(USE_PJ_HUGE_VAL)

typedef union { unsigned char __c[8]; double __d; } _pj_huge_val_t;

#if defined(__BIG_ENDIAN__)
#  define _PJ_HUGE_VAL_bytes        { 0x7f, 0xf0, 0, 0, 0, 0, 0, 0 }
#else
#  define _PJ_HUGE_VAL_bytes        { 0, 0, 0, 0, 0, 0, 0xf0, 0x7f }
# endif

static _pj_huge_val_t _pj_huge_val = { _PJ_HUGE_VAL_bytes };
#define pj_huge_val (_pj_huge_val.__d)

#ifdef HUGE_VAL
#undef HUGE_VAL
#define HUGE_VAL   pj_huge_val
#endif

#endif /*USE_PJ_HUGE_VAL*/

#ifdef __cplusplus
}
#endif /*__cplusplus*/

#endif /*PJ_HUGE_VAL_H*/

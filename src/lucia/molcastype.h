/* molcastype.h $Revision: 7.7 $ */
/*
 *
 * header file for int definition
 *
 */

#ifdef _I8_
#ifdef _x86_I8_           /* ---- x86 I8  ---- */
#define INT long long int
#define UN_INT unsigned long long int
#define INT_FORMAT "%lld"
#else
#define INT long int      /* ----   I8   ----- */
#define UN_INT unsigned long int
#define INT_FORMAT "%ld"
#endif
#else
#define INT int           /* ----   I4   ----- */
#define UN_INT unsigned int
#define INT_FORMAT "%d"
#endif
/* $Id: molcastype.h 25744 2014-06-08 07:43:42Z d3y133 $ */

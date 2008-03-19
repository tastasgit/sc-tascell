/*
 * total.c
 *  Put totals of "cpp_test.tbl".
 *  Usage:  total <n> <infile> <outfile>
 *      Specify the number of preprocessors by <n>.
 *  1998/08     kmatsui
 *  2002/08     Updated for "cpp-test.txt" V.1.3    kmatsui
 */

#include    "stdio.h"
#include    "stdlib.h"
#include    "string.h"
#include    "ctype.h"

#define     MAX_COLS    (MAX_CASES + MINMAX)
#define     MAX_CASES   30
#define     MINMAX      2
#define     OFFS        7
#define     LEN         4
#define     COLS        (MINMAX + cases)
#define     LLEN        ((COLS * LEN) + OFFS + 2 + 2)
#define     MAX_LLEN    ((MAX_COLS * LEN) + OFFS + 2 + 2)

#define     C90         1
#define     C99         2
#define     CPL         3

int     cases;
int     subtotal[ MAX_COLS];
int     mttl90[ MAX_COLS];  /* mid-total of C90 conformance */
int     mttl99[ MAX_COLS];  /* mid-total of C99 new features conformance    */
int     mttlcpl[ MAX_COLS]; /* mid-total of C++ only features conformance   */
int     grandtotal[ MAX_COLS];
char    buf[ MAX_LLEN];

void    usage();
void    add_points();
void    put_subtotal();
void    put_mttl();
void    put_grandtotal();
char    *stpcpy();

main( argc, argv)
    int     argc;
    char    **argv;
{
    int     i, len, sitem, mitem90, mitem99, mitemcpl, gitem, cplus;

    if (argc < 2 || argc > 4)
        usage();
    len = strlen( argv[ 1]);
    for (i = 0; i < len; i++) {
        if (! isdigit( argv[ 1][ i]))
            usage();
    }
    if ((cases = atoi( argv[ 1])) > MAX_CASES)
        usage();
    if (argc > 2)
        if (freopen( argv[ 2], "r", stdin) == NULL)
            usage();
    if (argc == 4)
        if (freopen( argv[ 3], "w", stdout) == NULL)
            usage();

    sitem = mitem90 = mitem99 = mitemcpl = gitem = cplus = 0;

    while (fgets( buf, MAX_LLEN, stdin) != NULL) {
        if (isalpha( buf[ 0]) && buf[ 1] == '.' && isdigit( buf[ 2])) {
            add_points( C90);
            sitem++;
        } else if (isalpha( buf[ 0]) && buf[ 1] == '.' && isalpha( buf[ 2])) {
            if (cplus) {
                add_points( CPL);
                mitemcpl++;
            } else {
                add_points( C99);
                mitem99++;
            }
        } else if (memcmp( buf, "stotal", 6) == 0) {
            put_subtotal();
            fprintf( stderr, "sitem:%d\n", sitem);
            mitem90 += sitem;
            sitem = 0;
        } else if (memcmp( buf, "mttl90", 6) == 0) {
            put_mttl( C90);
            fprintf( stderr, "  mitem90:%d\n", mitem90);
            gitem += mitem90;
            mitem90 = 0;
        } else if (memcmp( buf, "mttl99", 6) == 0) {
            put_mttl( C99);
            fprintf( stderr, "  mitem99:%d\n", mitem99);
            gitem += mitem99;
            mitem99 = 0;
        } else if (memcmp( buf, "mttl++", 6) == 0) {
            put_mttl( CPL);
            fprintf( stderr, "  mitem++:%d\n", mitemcpl);
            gitem += mitemcpl;
            mitemcpl = cplus = 0;
        } else if (memcmp( buf, "gtotal", 6) == 0) {
            put_grandtotal();
            fprintf( stderr, "      gitem:%d\n", gitem);
        } else if (memcmp( buf, "[C++:", 5) == 0) {
            cplus = 1;
        }
        /* Else as it is.   */
        fputs( buf, stdout);
    }

    return 0;
}

void    usage()
{
    char    **mesp;
    static char     *mes[] = {
        "Total:  Put totals of \"cpp_test.tbl\".\n",
        "Usage:  total <num> cpp_test.old cpp_test.new\n",
        "Specify the number of preprocessors by <num>.\n",
        NULL
    };

    mesp = mes;
    while (*mesp)
        fputs( *mesp++, stderr);
    exit( 0);
}

void    add_points( norm)
    int     norm;
{
    int     i;
    int     len;
    int     min, max, point;
    int     *arr;
    char    *p;

    switch (norm) {
    case C90:
        arr = subtotal;
        break;
    case C99:
        arr = mttl99;
        break;
    case CPL:
        arr = mttlcpl;
        break;
    }
    len = strlen( buf);

    for (i = 0, p = buf + OFFS + 1; i < COLS && p - buf < len;
            i++, p += LEN, i == MINMAX ? (p += 2) : 0) {
        if (*(p + 2) == ' ')    /* No point written */
            continue;
        point = atoi( p);
        switch (i) {
        case 0:     min = point;    break;
        case 1:     max = point;    break;
        default:
            if (point < min || max < point) {
                fprintf( stderr, "Out of range: (No.%d) %d\n"
                        , i - 1, point);
                fputs( buf, stderr);
            }
            break;
        }
        arr[ i] += point;
    }
}

void    put_subtotal()
{
    int     i;
    char    *p;
    buf[ OFFS - 1] = ' ';

    for (i = 0, p = buf + OFFS; i < COLS; i++, p += LEN) {
        if (i == MINMAX)
            p = stpcpy( p, "  ");
        sprintf( p, "%4d", subtotal[ i]);
        mttl90[ i] += subtotal[ i];
        subtotal[ i] = 0;
    }

    strcpy( p, "\n");
}

void    put_mttl( norm)
    int     norm;
{
    int     i;
    char    *p;
    int     *mttl;

    switch (norm) {
    case C90:
        mttl = mttl90;
        break;
    case C99:
        mttl = mttl99;
        break;
    case CPL:
        mttl = mttlcpl;
        break;
    }
    buf[ OFFS - 1] = ' ';

    for (i = 0, p = buf + OFFS; i < COLS; i++, p += LEN) {
        if (i == MINMAX)
            p = stpcpy( p, "  ");
        sprintf( p, "%4d", mttl[ i]);
        grandtotal[ i] += mttl[ i];
        mttl[ i] = 0;
    }

    strcpy( p, "\n");
}

void    put_grandtotal()
{
    int     i;
    char    *p;

    buf[ OFFS - 1] = ' ';

    for (i = 0, p = buf + OFFS; i < COLS; i++, p += LEN) {
        if (i == MINMAX)
            p = stpcpy( p, "  ");
        sprintf( p, "%4d", grandtotal[ i]);
    }

    strcpy( p, "\n");
}

#if !__TURBOC__ && !LSI_C && !__MWC09__
char    *stpcpy( p, app)
    char    *p;
    const char  *app;
{
    strcpy( p, app);
    return p + strlen( p);
}
#endif


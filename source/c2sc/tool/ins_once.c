/*
 * ins_once.c
 *  Quick and dirty program to insert '#pragma __once' into the header files.
 *  1998/08     kmatsui
 *  2002/08     kmatsui
 *      Added -p, -o, -g option, removed -s option.
 *      Compile with -DPATH_DELIM='x' option, if the path-delimiter is any
 *          other than '/'.
 */

#include    "stdio.h"
#include    "stdlib.h"
#include    "string.h"
#include    "ctype.h"
#include    "errno.h"

#ifndef PATH_DELIM
#define PATH_DELIM  '/'
#endif

#define TRUE    1
#define FALSE   0

#define IFNDEF      0x100
#define IF          0x101
#define PRAGMA      0x102
#define DEFINED     0x103
#define __MCPP      0x110
#define __ONCE      0x111

void    usage( void);
void    test_a_file( char *);
void    conv_a_file( char *);
void    insert_once( FILE *, FILE *, char *);
void    prepend_once( FILE *, FILE *);
int     look_directive( FILE *);
int     get_token( char **);
void    ins_once( FILE *);

int     test;
    /* Only test files whether beginning with #ifndef or #if !defined.  */
int     pre_ansi;
/*
 * If TRUE, insert 9 lines for the old pre-ansi preprocessors.
 * If FALSE insert only '#pragma __once' line for the preprocessors
 * which can accept #pragma.
 */
int     prepend;
/*
 * If TRUE, prepend '#pragma __once' line to the file.
 * If FALSE, insert the line after the first #ifndef line.
 */
int     gcc;
/*
 * If TRUE, do not insert '#pragma __once' line to "stddef.h".
 * This is the option for GCC family.
 */

main( argc, argv)
    int     argc;
    char    **argv;
{
    extern int      getopt( int, char **, char *);
    extern int      optind;
    extern char     *optarg;
    int     opt;
    char    *except[] = { "assert.h", "cassert", "cassert.h", NULL};
    char    *g_except[] = { "stddef.h", NULL};
    char    *arg;
    char    **ep;

    if (argc < 2)
        usage();
    while (optind < argc
            && (opt = getopt( argc, argv, "gopt")) != EOF) {
        switch (opt) {
        case 't':
            test = TRUE;
            break;
        case 'o':
            pre_ansi = TRUE;
            break;
        case 'p':
            prepend = TRUE;
            break;
        case 'g':
            gcc = TRUE;
            break;
        default:
            usage();
            break;
        }
    }
    argv += (optind - 1);
    while (*++argv) {
        if ((arg = strrchr( *argv, PATH_DELIM)) != NULL)
            arg++;
        else
            arg = *argv;
        for (ep = except; *ep; ep++) {
            if (strcmp( arg, *ep) == 0)
                goto skip;
        }
        if (gcc) {
            for (ep = g_except; *ep; ep++) {
                if (strcmp( arg, *ep) == 0)
                    goto skip;
            }
        }
        if (test)
            test_a_file( *argv);
        else
            conv_a_file( *argv);
        continue;
skip:   fprintf( stderr, "Skipped %s\n", *ep);
    }
    return 0;
}

void    usage( void)
{
    static char     *mes[] = {
   "ins_once: Insert '#pragma __once' to header files except \"assert.h\"\n",
   "            and \"stddef.h\" (for GNU C).\n",
   "Usage: ins_once [-DPATH_DELIM=\\] [-t|-p|-o|-g] [header1.h [header2.h [...]]]\n",
   "    -t : Only test files whether beginning with #ifndef or #if !defined.\n",
   "    -p : Prepend the line to the file\n",
   "        (default: insert after the first #ifndef line -- for GNU C).\n",
   "    -o : For the OLD C preprocessors which cannot accept #pragma.\n",
   "    -g : Do not convert \"stddef.h\".\n",
        NULL,
    };
    char    **mesp = mes;

    while( *mesp)
        fputs( *mesp++, stderr);
    if (errno) {
        fputs( strerror( errno), stderr);
        fputc( '\n', stderr);
    }
    exit( errno);
}

void    test_a_file( fname)
    char    *fname;
/*
 * Only test the file whether it begins with #ifndef or #if !defined.
 */
{
    char    buf[ BUFSIZ];
    FILE    *fp_in;
    int     token;
    char    *cp;

    fp_in = fopen( fname, "r");
    if (fp_in == NULL)
        usage();

    while (fgets( buf, BUFSIZ, fp_in) != NULL) {
        cp = buf;
        if (get_token( &cp) == '#') {       /* The first directive  */
            if (((token = get_token( &cp)) != IFNDEF)   /* #ifndef  */
                    && (token != IF || get_token( &cp) != '!'
                        || get_token( &cp) != DEFINED)) {   /* #if ! defined*/
                fputs( fname, stderr);
                fputs( ": doesn't begin with #ifndef nor #if !defined\n",
                        stderr);
            }
            break;
        }
    }
    fclose( fp_in);
}

void    conv_a_file( fname)
    char    *fname;
/*
 * Insert '#pragma __once' line to seemingly apropriate place according
 * the command-line options.
 */
{
    char    *tmp = "tmp_once";
    FILE    *fp_in, *fp_out;

    if ((fp_in = fopen( fname, "r")) == NULL)
        usage();
    if ((fp_out = fopen( tmp, "w")) == NULL)
        usage();
    fprintf( stderr, "Converting %s\n", fname);

    if (prepend)
        prepend_once( fp_in, fp_out);
    else
        insert_once( fp_in, fp_out, fname);

    fclose( fp_in);
    fclose( fp_out);
    if (remove( fname) != 0 || rename( tmp, fname) != 0)
        usage();
}

void    insert_once( fp_in, fp_out, fname)
    FILE    *fp_in, *fp_out;
    char    *fname;
/*
 * Insert '#pragma __once' line after the first directive line, if the
 * directive is #ifndef or #if !defined, else append the line at the end
 * of the file.
 */
{
    char    buf[ BUFSIZ];
    int     token;
    int     no_ifndef = TRUE;
    char    *cp;

    while (fgets( buf, BUFSIZ, fp_in) != NULL) {
        fputs( buf, fp_out);
        cp = buf;
        if (get_token( &cp) == '#') {       /* The first directive  */
            if (((token = get_token( &cp)) == IFNDEF)   /* #ifndef  */
                    || (token == IF && get_token( &cp) == '!'
                        && get_token( &cp) == DEFINED)) {   /* #if ! defined*/
                no_ifndef = FALSE;
                if (! look_directive( fp_in))
                    ins_once( fp_out);
                /* Else already written in   */
            } else {                            /* Other directive  */
                fputs( fname, stderr);
                fputs( ": doesn't begin with #ifndef nor #if !defined\n",
                        stderr);
            }
            break;
        }
    }
    while (fgets( buf, BUFSIZ, fp_in) != NULL)
        fputs( buf, fp_out);
    if (no_ifndef)
        ins_once( fp_out);          /* Append the line to the file  */
}

void    prepend_once( fp_in, fp_out)
    FILE    *fp_in, *fp_out;
/*
 * Prepend the '#pragma __once' line at the top of the file.
 */
{
    char    buf[ BUFSIZ];

    if (! look_directive( fp_in))
        ins_once( fp_out);          /* Prepend the line to the file */

    while (fgets( buf, BUFSIZ, fp_in) != NULL)
        fputs( buf, fp_out);
}

int     look_directive( fp)
    FILE    *fp;
/*
 * Look whether the next line is '#pragma __once'.
 */
{
    char    buf[ BUFSIZ];
    long    pos;
    int     res = 0;
    int     token;
    char    *cp;

    pos = ftell( fp);
    cp = buf;
    if (fgets( buf, BUFSIZ, fp) && buf[ 0] == '\n'
            && fgets( buf, BUFSIZ, fp) && get_token( &cp) == '#') {
        if (!pre_ansi && get_token( &cp) == PRAGMA && get_token( &cp) == __ONCE)
            res = 1;
        else if (pre_ansi && get_token( &cp) == IF
                && get_token( &cp) == __MCPP)
            res = 1;
    }
    fseek( fp, pos, SEEK_SET);
    return res;
}

int     get_token( cpp)
    char    **cpp;
/* Get the next 'token', without parsing comments, literals, etc.   */
{
    int     token;
    char    *cp = *cpp;

    while (*cp != '\n' && isspace( *cp))
        cp++;
    if (memcmp( cp, "ifndef", 6) == 0) {
        token = IFNDEF;
        cp += 6;
    } else if (memcmp( cp, "if", 2) == 0) {
        token = IF;
        cp += 2;
    } else if (memcmp( cp, "pragma", 6) == 0) {
        token = PRAGMA;
        cp += 6;
    } else if (memcmp( cp, "defined", 7) == 0) {
        token = DEFINED;
        cp += 7;
    } else if (memcmp( cp, "__MCPP", 11) == 0) {
        token = __MCPP;
        cp += 11;
    } else if (memcmp( cp, "__once", 6) == 0) {
        token = __ONCE;
        cp += 6;
    } else {
        token = *cp++;
    }
    *cpp = cp;
    return token;
}

void    ins_once( fp)
    FILE    *fp;
{
    static char     *once[] = {
        /* Redundant directives not to reject old preprocessors.    */
        "\n",
        "#if     __MCPP >= 2\n",
        "#ifdef  __STDC__\n",
        "#pragma __once\n",
        "#else\n",
        "#ifdef  __cplusplus\n",
        "#pragma __once\n",
        "#endif\n",
        "#endif\n",
        "#endif\n",
        "\n",
         NULL,
    };
    static char     *once_only[] = {
        "\n#pragma __once\n\n",
        NULL,
    };
    static char    **cpp;
    char           **tpp;

    if (cpp == NULL) {                  /* Have to initialize           */
        if (pre_ansi)
            cpp = once;
        else
            cpp = once_only;
    }

    tpp = cpp;
    while (*tpp)
        fputs( *tpp++, fp);
}


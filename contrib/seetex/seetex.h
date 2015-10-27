/*---------------------------------------------------------*\
$Id: seetex.h 19708 2010-10-29 18:04:21Z d3y133 $

 seetex.h  include file for seetex

 Note: must follow <stdio.h> include
\*---------------------------------------------------------*/
#define TRUE  1
#define FALSE 0
#define DEBUG_MODE FALSE
#define MAX_LINE_LEN 132

/* function declarations */
int is_tex_file(char *filename);
int is_fortran_file(char *filename);
int seetex_process(FILE *srcid, FILE *texid);
int get_line(FILE *file, char *buf, int size);
void seetex_error(char *errmsg, int error_code);
void usage(int err);
void print_syntax();
int seetex_end(char *line);
int seetex_begin(char *line);
void put_line_strip(char *line,FILE *id);
void put_line_strip_check(char *line,FILE *id);

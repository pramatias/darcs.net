
#include <sys/stat.h>
#include <string.h>
#include <stdlib.h>

int sloppy_atomic_create(const char *p);
int atomic_create(const char *p);

#ifdef _WIN32
int mkstemp(char *p);
int pipe( int fildes[2] );
int renamefile(const char *from, const char *to);
#endif

int stdout_is_a_pipe();

int maybe_relink(const char *src, const char *dst, int careful);

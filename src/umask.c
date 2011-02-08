#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <errno.h>
#include "umask.h"

int
set_umask(char *mask_string)
{
#ifndef WIN32
    int rc;
    unsigned mask;
    char *end;

    mask = strtoul(mask_string, &end, 8);
    if(!end || *end != '\0') {
        errno = EINVAL;
        return -1;
    }

    rc = umask(mask);
    return rc;
#else
    /* umask() has no useful meaning on win32. */
    return 0;
#endif /* #ifndef WIN32 ... else ... */
}

int
reset_umask(int old_mask)
{
#ifndef WIN32
    umask(old_mask);
    return 1;
#else
    return 0;
#endif /* #ifndef WIN32 ... else ... */
}

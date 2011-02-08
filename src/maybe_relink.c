/*
  Copyright (C) 2005 Juliusz Chroboczek

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; see the file COPYING.  If not, write to
  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
  Boston, MA 02110-1301, USA.
*/

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include <unistd.h>
#include <errno.h>
#include <sys/time.h>

#ifdef _WIN32
int
maybe_relink(const char *src, const char *dst, int careful)
{
    return 0;
}

#else

/* Tries to link src to dst if both files exist and have the same
   contents.  If careful is false only the file sizes are compared; if
   it is true, the full contents are compared.

   This code assumes that dst cannot change behind our back -- the
   caller is supposed to protect it by a lock.  On the other hand, it
   does handle simultaneous access to src, but only if src is never
   modified in place.  It should also be safe over NFS.

   Assumes that rename cannot fail mid-way on a single filesystem.

   Returns 1 on success, 0 if the files are already linked, -1 for an
   error in errno, -2 if the files cannot be linked because they are not
   the same, on different devices, or on a filesystem with no support for
   hard links, -3 if there was a race condition, -4 if something unexpected
   happened. */

int
maybe_relink(char *src, char *dst, int careful)
{
#define RELINK_BUFFER_SIZE 8192

    int len, rc, saved_errno;
    char *tempname;
    struct stat srcstat, dststat, tempstat;
    struct timeval now;

    rc = stat(src, &srcstat);
    if(rc < 0) {
        if(errno == ENOENT)
            return -2;
        else
            return -1;
    }

    rc = stat(dst, &dststat);
    if(rc < 0) return -1;

    if(!S_ISREG(srcstat.st_mode) || !S_ISREG(dststat.st_mode)) {
        return -4;
    }

    if(srcstat.st_dev != dststat.st_dev) {
        return -2;
    }

    if(srcstat.st_ino == dststat.st_ino)
        /* Files are already linked */
        return 0;

    if(srcstat.st_size != dststat.st_size)
        return -2;

    /* link is atomic even on NFS, we will fail gracefully if the name
       is not unique. */
    gettimeofday(&now, NULL);
    rc = strlen(dst) + 6;
    tempname = malloc(rc);
    if(tempname == NULL) return -1;
    len = snprintf(tempname, rc, "%s-%04x", dst,
                   ((unsigned)(now.tv_usec ^ (now.tv_usec >> 16))) & 0xFFFF);
    if(len < 0 || len >= rc) {
        free(tempname);
        return -4;
    }

    rc = link(src, tempname);
    if(rc < 0) {
        /* We need to try to remove the link in case this was a
           problem with NFS over an unreliable transport. */
        goto fail;
    }

    rc = stat(tempname, &tempstat);
    if(rc < 0) goto fail;

    /* Check for a race condition.  The size and mtime checks are
       gratuitious, but they don't cost much, and might save your data
       if you're on a filesystem without i-nodes. */
    if(tempstat.st_ino != srcstat.st_ino ||
       tempstat.st_size != srcstat.st_size ||
       tempstat.st_mtime != srcstat.st_mtime) {
        unlink(tempname);
        free(tempname);
        return -3;
    }
    if(careful) {
        int fd1, fd2, i, rc1, rc2;
        char buf1[RELINK_BUFFER_SIZE], buf2[RELINK_BUFFER_SIZE];

        fd1 = open(tempname, O_RDONLY);
        if(fd1 < 0) goto fail;
        fd2 = open(dst, O_RDONLY);
        if(fd2 < 0) { close(fd1); goto fail; }

        i = 0;
        /* This comparison is approximate: it doesn't deal with short
           reads and EINTR.  It's okay, as these cases are rare and if
           they happen, we're still safe. */
        while(i < tempstat.st_size) {
            rc1 = read(fd1, buf1, RELINK_BUFFER_SIZE);
            if(rc1 < 0) { close(fd1); close(fd2); goto fail; }
            rc2 = read(fd2, buf2, RELINK_BUFFER_SIZE);
            if(rc2 < 0) { close(fd1); close(fd2); goto fail; }
            if(rc1 == 0 || rc1 != rc2 || memcmp(buf1, buf2, rc1) != 0) {
                close(fd1); close(fd2);
                unlink(tempname);
                free(tempname);
                return -2;
            }
            i += rc1;
        }
        close(fd1); close(fd2);
    }

    rc = rename(tempname, dst);
    if(rc < 0) goto fail;

    free(tempname);
    return 1;

 fail:
    saved_errno = errno;
    unlink(tempname);
    free(tempname);
    errno = saved_errno;
    if(errno == EPERM || errno == EOPNOTSUPP)
        return -2;
    return -1;

#undef RELINK_BUFFER_SIZE
}

#endif

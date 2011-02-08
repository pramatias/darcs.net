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
#include <windows.h>
#include <io.h>
#endif

int sloppy_atomic_create(const char *p)
{
    int fd;
    fd = open(p, O_WRONLY | O_EXCL | O_CREAT, 0666);
    if(fd < 0)
        return -1;
    close(fd);
    return 1;
}

#ifdef _WIN32

int atomic_create(const char *p)
{
    return sloppy_atomic_create(p);
}

#else

static int careful_atomic_create(const char *p)
{
    /* O_EXCL is not available over NFSv2, and even under NFSv3, it is
       broken on many systems.  The following protocol is provably
       safe assuming that:
       - creation of hard links is atomic;
       - stat hits the server rather than working from the cache.
    */

    static char hostname[65] = {'\0'};
    int fd, rc, saved_errno;
#define FILENAME_SIZE (11 + 15 + 8 + 1)
    char *filename;
    char *lastslash;
    int dirlen;
    struct timeval now;
    struct stat sb;

    if(hostname[0] == '\0') {
        char *c;
        int i;
        /* POSIX guarantees 65 is enough. */
        rc = gethostname(hostname, 65);
        if(rc < 0 || rc >= 65) {
            fprintf(stderr, "Error reading hostname when locking.\n");
            strcpy(hostname, "kremvax");
        }
        c = strchr(hostname, '.');
        if(c != NULL)
            *c = '\0';
        hostname[15] = '\0';
        /* clean up a few possible nasty characters folks might put in their hostname */
        for (i=0;i<15;i++)
          if (hostname[i] == ':' || hostname[i] == '/' || hostname[i] == '\\')
            hostname[i] = '-';
    }

    lastslash = strrchr(p, '/');
    dirlen = lastslash ? lastslash - p + 1 : 0;

    filename = malloc(dirlen + FILENAME_SIZE);
    if(filename == NULL)
        return -1;

    if(dirlen > 0)
        memcpy(filename, p, dirlen);
    filename[dirlen] = '\0';

    gettimeofday(&now, NULL);

    rc = snprintf(filename + dirlen, FILENAME_SIZE, "darcs_lock_%s%04x%04x",
                  hostname, ((unsigned)getpid()) & 0xFFFF,
                  ((unsigned)(now.tv_usec ^ (now.tv_usec >> 16))) & 0xFFFF);
    if(rc < 0 || rc >= FILENAME_SIZE) {
        fprintf(stderr, "Error writing to lock filename (%d)\n", 
                rc < 0 ? errno : 0);
        goto fail2;
    }

    fd = open(filename, O_WRONLY | O_EXCL | O_CREAT, 0666);
    if(fd < 0)
        goto fail2;

    /* Paranoia: should cause the client to flush its metadata cache. */
    rc = close(fd);
    if(rc < 0) {
        fprintf(stderr, "Error closing file %s. (%d)\n", filename, errno);
        goto fail;
    }

    rc = link(filename, p);
    if(rc >= 0)
        goto success;
    else if(errno == EPERM || errno == EOPNOTSUPP) {
        /* Linux returns EPERM when making hard links on filesystems
           that don't support them. */
        /* It seems that MacOS returns EOPNOTSUPP on filesystems that
           don't support hard links. */
        unlink(filename);
        free(filename);
        return sloppy_atomic_create(p);
    } else if(errno != EEXIST && errno != EIO)
        goto fail;

    /* The link may still have been successful if we're running over
       UDP and got EEXIST or EIO.  Check the file's link count. */

    rc = stat(filename, &sb);
    if(rc < 0) {
        goto fail;
    }

    if(sb.st_nlink != 2) {
        errno = EEXIST;
        goto fail;
    }

 success:
    unlink(filename);
    free(filename);
    return 1;

 fail:
    saved_errno = errno;
    unlink(filename);
    errno = saved_errno;
 fail2:
    free(filename);
    return -1;
}

int atomic_create(const char *p)
{
    static int sloppy = -1;

    if(sloppy < 0) {
        char *s = getenv("DARCS_SLOPPY_LOCKS");
        sloppy = (s != NULL);
    }

    if(sloppy)
        return sloppy_atomic_create(p);
    else
        return careful_atomic_create(p);
}

#endif

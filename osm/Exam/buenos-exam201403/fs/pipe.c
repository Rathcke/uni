/*
 * Pipe FS header file for OSM exam at DIKU 2014.
 *
 * You may need to modify this file.
 */

#include "kernel/kmalloc.h"
#include "kernel/assert.h"
#include "vm/pagepool.h"
#include "drivers/gbd.h"
#include "fs/vfs.h"
#include "fs/pipe.h"
#include "lib/libc.h"

/* Data structure for use internally in pipefs. */
typedef struct {
  /* Data needed for functions below */
} pipe_t;

typedef struct {
  semaphore_t *lock;
  pipe_t pipes[16];
} pipefs_t;

/***********************************
 * fs_t function implementations
 ***********************************/

/* Initialize pipefs. We allocate one page of dynamic memory for the
 * fs_t and pipefs_t structures.  Note that, in contrast to other
 * filesystems, we take no disk parameter.  You may want to extend
 * this function. */
fs_t *pipe_init(void)
{
    uint32_t addr;
    fs_t *fs;
    pipefs_t *pipefs;
    semaphore_t *sem;

    /* check semaphore availability before memory allocation */
    sem = semaphore_create(1);
    if (sem == NULL) {
        kprintf("pipe_init: could not create a new semaphore.\n");
        return NULL;
    }

    addr = pagepool_get_phys_page();
    if(addr == 0) {
        semaphore_destroy(sem);
        kprintf("pipe_init: could not allocate memory.\n");
        return NULL;
    }
    addr = ADDR_PHYS_TO_KERNEL(addr);      /* transform to vm address */

    /* Assert that one page is enough */
    KERNEL_ASSERT(PAGE_SIZE >= (sizeof(pipefs_t)+sizeof(fs_t)));

    /* fs_t, pipefs_t and all required structure will most likely fit
       in one page, so obtain addresses for each structure and buffer
       inside the allocated memory page. */
    fs  = (fs_t *)addr;
    pipefs = (pipefs_t *)(addr + sizeof(fs_t));

    /* save semaphore to the pipefs_t */
    pipefs->lock = sem;

    fs->internal = (void *)pipefs;

    /* We always have this name. */
    stringcopy(fs->volume_name, "pipe", VFS_NAME_LENGTH);

    fs->unmount   = pipe_unmount;
    fs->open      = pipe_open;
    fs->close     = pipe_close;
    fs->create    = pipe_create;
    fs->remove    = pipe_remove;
    fs->read      = pipe_read;
    fs->write     = pipe_write;
    fs->getfree   = pipe_getfree;
    fs->filecount = pipe_filecount;
    fs->file      = pipe_file;

    return fs;
}

/* You have to implement the following functions.  Some of them may
   just return an error.  You have to carefully consider what to do.*/

int pipe_unmount(fs_t *fs)
{
    fs=fs;
    return VFS_NOT_SUPPORTED;
}

int pipe_open(fs_t *fs, char *filename)
{
    fs=fs; filename=filename;
    return VFS_NOT_SUPPORTED;
}

int pipe_close(fs_t *fs, int fileid)
{
    fs = fs; fileid = fileid;
    return VFS_NOT_SUPPORTED;
}

int pipe_create(fs_t *fs, char *filename, int size)
{
    fs=fs; filename=filename; size=size;
    return VFS_NOT_SUPPORTED;
}

int pipe_remove(fs_t *fs, char *filename)
{
    fs=fs; filename=filename;
    return VFS_NOT_SUPPORTED;
}

int pipe_read(fs_t *fs, int fileid, void *buffer, int bufsize, int offset)
{
    fs=fs; fileid=fileid; buffer=buffer; bufsize=bufsize; offset=offset;
    return VFS_NOT_SUPPORTED;
}

int pipe_write(fs_t *fs, int fileid, void *buffer, int datasize, int offset)
{
    fs=fs; fileid=fileid; buffer=buffer; datasize=datasize; offset=offset;
    return VFS_NOT_SUPPORTED;
}

int pipe_getfree(fs_t *fs)
{
    fs=fs;
    return VFS_NOT_SUPPORTED;
}

int pipe_filecount(fs_t *fs, char *dirname)
{
    fs=fs; dirname=dirname;
    return VFS_NOT_SUPPORTED;
}

int pipe_file(fs_t *fs, char *dirname, int idx, char *buffer)
{
    fs=fs; dirname=dirname; idx=idx; buffer=buffer;
    return VFS_NOT_SUPPORTED;
}

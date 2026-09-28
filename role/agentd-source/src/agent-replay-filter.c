/* Correct stock zmx's initial scrollback replay (verified with 0.8.1).
 * The caller has checked that the session contains more rows than fit on
 * screen. Flush the replayed scrollback before zmx clears and paints the viewport.
 * After that ONE separator, every subsequent byte passes through unchanged.
 * The stream has no explicit snapshot marker, so output already in flight
 * can still interfere with identification. See docs/idle-reattach.md. */
#define _POSIX_C_SOURCE 200809L
#include <errno.h>
#include <poll.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static bool write_all(const void *data, size_t size)
{
    const char *p=data;
    while (size) {
        ssize_t n=write(STDOUT_FILENO,p,size);
        if (n<0 && errno==EINTR) continue;
        if (n<0 && (errno==EAGAIN || errno==EWOULDBLOCK)) {
            struct pollfd fd={.fd=STDOUT_FILENO,.events=POLLOUT};
            int ready;
            do { ready=poll(&fd,1,-1); } while (ready<0 && errno==EINTR);
            if (ready>0) continue;
        }
        if (n<=0) return false;
        p+=n; size-=(size_t)n;
    }
    return true;
}
int main(int argc, char **argv)
{
    char *end;
    if (argc!=2) return 2;
    errno=0;
    long rows=strtol(argv[1],&end,10);
    if (errno || end==argv[1] || *end || rows<1 || rows>65535) return 2;
    const char separator[]="\033[2J\033[H\033[0m";
    const size_t limit=16*1024*1024;
    char *initial=malloc(limit);
    if (!initial) return 1;
    size_t used=0;
    bool initial_phase=true;
    char bytes[16384];
    for (;;) {
        struct pollfd fd={.fd=STDIN_FILENO,.events=POLLIN};
        int ready=poll(&fd,1,initial_phase && used?200:-1);
        if (ready<0 && errno==EINTR) continue;
        if (ready<0) { free(initial); return 1; }
        if (!ready) {
            fprintf(stderr,"agentctl: replay separator absent; retaining stock output\n");
            if (!write_all(initial,used)) { free(initial); return 1; }
            initial_phase=false;
            free(initial); initial=NULL;
            continue;
        }
        ssize_t n=read(STDIN_FILENO,bytes,sizeof(bytes));
        if (n<0 && errno==EINTR) continue;
        if (n<=0) {
            bool ok=n==0 && (!initial_phase || write_all(initial,used));
            free(initial); return ok?0:1;
        }
        if (!initial_phase) {
            if (!write_all(bytes,(size_t)n)) { free(initial); return 1; }
            continue;
        }
        if ((size_t)n>limit-used) {
            fprintf(stderr,"agentctl: replay exceeds inspection limit; retaining stock output\n");
            if (!write_all(initial,used) || !write_all(bytes,(size_t)n)) { free(initial); return 1; }
            initial_phase=false;
            free(initial); initial=NULL;
            continue;
        }
        size_t start=used>sizeof(separator)?used-sizeof(separator):0;
        memcpy(initial+used,bytes,(size_t)n); used+=(size_t)n;
        for (size_t i=start;i+sizeof(separator)-1<=used;i++) {
            /* The CLI itself prefixes ESC[2J ESC[H. If the first content is
             * SGR 0, that accidentally looks like our separator at offset 0. */
            if (i<7) continue;
            if (memcmp(initial+i,separator,sizeof(separator)-1)) continue;
            if (!write_all(initial,i)) { free(initial); return 1; }
            for (long row=0;row<rows;row++)
                if (!write_all("\r\n",2)) { free(initial); return 1; }
            if (!write_all(initial+i,used-i)) { free(initial); return 1; }
            initial_phase=false;
            free(initial); initial=NULL;
            break;
        }
    }
}

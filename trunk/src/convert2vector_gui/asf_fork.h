#ifndef ASF_FORK_H
#define ASF_FORK_H

typedef int forkFunc(void *params);

int asfFork(forkFunc child_fn, void *child_params,
            forkFunc parent_fn, void *parent_params);

#endif

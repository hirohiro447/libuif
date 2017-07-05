#include <pthread.h>

void pthread_create_opaque(pthread_t *threadptr, void *procptr, void *dataptr, int *err){
//   creates a new thread using an opaque pointer to the pthread_t structure
  *err = pthread_create(threadptr, NULL, procptr, dataptr);
}

void pthread_join_opaque(pthread_t *threadptr, int *err) {
//  joines a thread using an opaque pointer to the pthread_t structure
 *err = pthread_join(*threadptr, NULL);
}


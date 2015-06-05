/*
 * $HEADER$
 *
 * Simple ring test program
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "mpi.h"

#define SIZE 1024
#define POS 38
#define INITIAL_VALUE 10


int main(int argc, char *argv[])
{
    int i;
    int rank, size, tag, next, prev;
    int array_size = SIZE;
    int pos = POS;

    int *send_array;
    int *recv_array;

    /* Start up MPI */

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
 
    /* Arbitrarily choose 201 to be our tag.  Calculate the rank of
       the next process in the ring.  Use the modulus operator so that
       the last process "wraps around" to rank zero. */

    tag = 201;
    next = (rank + 1) % size;
    prev = (rank + size - 1) % size;
    send_array = malloc(sizeof(int) * SIZE);
    recv_array = malloc(sizeof(int) * SIZE);

    /* If we are the "master" process, get a integer from the user to
       specify how many times we want to go around the ring */

    for (i = 0; i < array_size; ++i) {
        send_array[i] = 17;
        recv_array[i] = -1;
    }

    if (rank == 0) {
        send_array[pos] = INITIAL_VALUE;

        printf("Process %d sending %d to %d, tag %d (%d procs in ring)\n", 
               rank, send_array[pos], next, tag, size);
        MPI_Send(send_array, array_size, MPI_INT, next, tag, MPI_COMM_WORLD); 
        printf("Process 0 sent to %d\n", next);
    }

    /* Pass the message around the ring.  The exit mechanism works as
       follows: the message (a positive integer) is passed around the
       ring.  Each time is passes rank 0, it is decremented.  When
       each processes receives the 0 message, it passes it on to the
       next process and then quits.  By passing the 0 first, every
       process gets the 0 message and can quit normally. */

    while (1) {
        recv_array[pos] = -1;
        MPI_Recv(recv_array, array_size, MPI_INT, prev, tag, MPI_COMM_WORLD, 
                 MPI_STATUS_IGNORE);
        send_array[pos] = recv_array[pos];
        if (rank == 0) {
            --send_array[pos];
            printf("Process 0 decremented num: %d\n", send_array[pos]);
        }
        MPI_Send(send_array, array_size, MPI_INT, next, tag, MPI_COMM_WORLD);
        if (send_array[pos] == 0) {
            printf("Process %d exiting\n", rank);
            break;
        }
    }

    /* The last process does one extra send to process 0, which needs
       to be received before the program can exit */

    if (rank == 0) {
        MPI_Recv(recv_array, array_size, MPI_INT, prev, tag, MPI_COMM_WORLD,
                 MPI_STATUS_IGNORE);
    }
    
    /* Quit */

    MPI_Finalize();
    return 0;
}

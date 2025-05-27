#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <string.h>
#include <signal.h>
#include <semaphore.h>

/*
 ============================================================================
 Name        : 2023_ex2_part1.c
 Author      :
 Version     :
 Copyright   : Your copyright notice
 Description : Hello World in C, Ansi-style
 ============================================================================
 */

#define SNAME "MySem"

/*
 * This is a syntax assistance for part 1 question 3
 open semaphore
 sem_t *sem = sem_open(SNAME, O_CREAT, 0777, 0);

 increment semaphore
sem_post(sem);

 decrement semaphore
sem_wait(sem);
*/

int main(void) {
	int status;
    int parentToChild[2], childToParent[2];
    pipe(parentToChild);
    pipe(childToParent);


    status = fork();
    if (status == 0){
        close(parentToChild[1]);
        close(childToParent[0]);
        char answer[5];
        read(parentToChild[0], answer,5);
        if(strcmp(answer, "ready")){
        	printf("No! it's not true! it's impossible\n");
            write(childToParent[1], "ready", 5);
        }

        read(parentToChild[0], answer,5);
        if(strcmp(answer, "ready")){
            printf("Noooooooooooooo\n");
            write(childToParent[1], "ready", 5);
        }
    }
    else{
        close(childToParent[1]);
        close(parentToChild[0]);
        char answer[5];
        printf("Luke, I am your father!\n");
        write(parentToChild[1],"ready", 5);
        read(childToParent[0], answer,5);
        if(strcmp(answer, "ready")){
            printf("Search your feelings, you know it to be true.\n");
            write(parentToChild[1], "ready", 5);
        }

        read(childToParent[0], answer,5);
        if(strcmp(answer, "ready")){
            printf("luke, you can destroy the emperor, he has foreseen it.\n");
        }
    }
  return 1;
}

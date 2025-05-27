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
	sem_t *sem = sem_open(SNAME, O_CREAT, 0777, 0);
	status = fork();
	if (status == 0){
		sem_wait(sem);
		printf("No! it's not true! it's impossible\n");
		sem_post(sem);
		sleep(2);
		sem_wait(sem);
		printf("Noooooooooooooo\n");
		sem_post(sem);

	}
	else{
		printf("Luke, I am your father!\n");
		sem_post(sem);
		sleep(2);
		sem_wait(sem);
		printf("Search your feelings, you know it to be true.\n");
		sem_post(sem);
		sleep(2);
		sem_wait(sem);
		printf("luke, you can destroy the emperor, he has foreseen it.\n");
	}
	return 1;
}


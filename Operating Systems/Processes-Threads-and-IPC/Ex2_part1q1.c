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

int main(void) {
	int status;

	status = fork();
	if (status == 0){
		sleep(7);
		printf("No! it's not true! it's impossible\n");
		sleep(12);
		printf("Noooooooooooooo\n");
		sleep(6);

	}
	else{
		printf("Luke, I am your father!\n");
		sleep(13);
		printf("Search your feelings, you know it to be true.\n");
		sleep(11);
		printf("luke, you can destroy the emperor, he has foreseen it.\n");
	}
	return 1;
}

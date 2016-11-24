/* Created by S. Cozzini and G.P. Brandino for the Course P1.6 Data Management @ MHPC
 * Last Revision: November 2015
 */

#include<stdio.h>
#include<stdlib.h>
#include "mpi.h"
int main(int argc, char **argv){

	int i, rank, size, offset, N=16 ;

	MPI_File fhw;
	MPI_Status status;
	MPI_Init(&argc, &argv);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &size);

	int loc_size = N/size;
	//int* buf=(int*) malloc(N*sizeof(int));
	int* buf=(int*) malloc( loc_size * sizeof(int) );
/*
	for ( i=0;i<N;i++){
	  buf[i] = i ;
	}*/

	for ( i=0;i<loc_size;i++){
		buf[i] = rank * loc_size + i ;
	}

	int rest = N % size;
	printf("rest: %d\n", rest);

	offset = rank*loc_size*sizeof(int);


	if( rest != 0){
		if( rank == size - 1)
			loc_size += rest;
		}


	MPI_File_open(MPI_COMM_WORLD, "datafile", MPI_MODE_CREATE|MPI_MODE_WRONLY, MPI_INFO_NULL, &fhw);
	printf("Rank: %d, Offset: %d\n", rank, offset);

	//MPI_File_write_at(fhw, offset, buf + offset_buff, write_buff, MPI_INT, &status);

	MPI_File_write_at(fhw, offset, buf, loc_size, MPI_INT, &status);
	//MPI_File_write_at(fhw, rank*N*sizeof(int), buf, N, MPI_INT, &status);
	// A partire da buff, scrivimi N interi sul file fhw, cominciando a scrivere dal byte offset (secondo arg)
	// La funzione write scrive su un file fhw che E' CONDIVISO da tutti, quindi occhio agli indici!!

	free(buf);
	MPI_File_close(&fhw);
	MPI_Finalize();

	return 0;
}

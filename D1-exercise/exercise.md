# D1-exercise1


## 1 - writeFile_offset.c and readFile_offset.c


1. Compile both codes. 
2. Check the `writeFile_offset.c` code and understand what it is
   doing.

   Select the correct answer, assuming the code runs on 4 processes

   a. It writes 16 integers from 0 to 15

   b. It writes 16 integers, each process writes 0,1,2,3 (This one!!!
        The command at the beginning was ```c MPI_File_write_at(fhw, offset, buf, (N/size), MPI_INT, &status);```)

   c. It breaks your computer

3. Run the code using 4 processes. Compare your answer to the previous
   question with the output, by means of `od -i` on the produced
   datafile.

4. Modify the code in order to print all the 16 integer by each
   process

   ```c
   int* buf=(int*) malloc( N * sizeof(int) );

   ...

   MPI_File_write_at(fhw, rank*N*sizeof(int), buf, N, MPI_INT, &status); 
   ```

5. Modify the code such that each process print a slice of the `buf`
   array.

   ```c

   int loc_size = N/size;
   int* buf=(int*) malloc( loc_size * sizeof(int) );

   ...

   for ( i=0;i<loc_size;i++){
     buf[i] = rank * loc_size + i ;
   }

   ...

   MPI_File_write_at(fhw, rank*loc_size*sizeof(int), buf, loc_size, MPI_INT, &status);

   ```

6. Run the code using 1,2 and 3 processes. Is the code writing the 16
   integers in all cases?

   No, in the case of 1 and 3 processor a rest is appearing and the program does not write every integers.

7. Fix the code to run on an arbitrary number of processes, such that
   each process write a slice of the `buf` array.  (Hint: use the
   loadbalacing strategy )

   ```c

   if( rest != 0){
    if( rank == size - 1)
     loc_size += rest;
   }

   ```

8. Check the readFile_offset.c code and understand what it is doing.
  
   Answer the following questions

   a. How many data each process reads?
   b. What do you expect each process to read?

9. Fix the code, to read the correct amount of data, as in point 5.

   To fix the code we need to put ```#define FILESIZE 64```
  
10. (Homework) Try to compile and run on different systems and see if
    the results are consistent.

## 2 - writeFile_pointer.f90 and readFile_pointer.c


1. Compile both codes.

2. Check the writeFile_pointer.f90 code and understand what it is
   doing.
 
   Select the correct answer

   a. It does exactly what writeFile_offset does 

   b. It writes 16*[number of process] integers (This one!!!)

   c. It breaks your computer

3. Run the code. Compare your answer to the previous question with the
   output, by means of `od -i` on the produced datafile.

4. Modify the code such that it behaves as writeFile_offset as in 1-5
   ```c
   PROGRAM write_file
   USE MPI
   IMPLICIT NONE
   INTEGER :: err, i, myid, file, intsize, loc_size, nprocs
   INTEGER :: status(MPI_STATUS_SIZE)
   INTEGER, PARAMETER :: count=16
   INTEGER, DIMENSION(:), ALLOCATABLE :: buf
   INTEGER(KIND=MPI_OFFSET_KIND) :: disp

   CALL MPI_INIT(err)
   CALL MPI_COMM_RANK(MPI_COMM_WORLD, myid,err)
   CALL MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs,err)

   loc_size = int(count/nprocs)

   allocate(buf(loc_size))

   DO i = 1, loc_size
   buf(i) = myid*loc_size + i -1
   END DO

   CALL MPI_FILE_OPEN(MPI_COMM_WORLD, 'datafile', MPI_MODE_WRONLY + MPI_MODE_CREATE, MPI_INFO_NULL, file, err)
   CALL MPI_TYPE_SIZE(MPI_INTEGER, intsize,err)
   disp= myid*loc_size* intsize

   CALL MPI_FILE_SEEK(file, disp, MPI_SEEK_SET, err)
   CALL MPI_FILE_WRITE(file, buf, loc_size, MPI_INTEGER, status, err)
   CALL MPI_FILE_CLOSE(file, err)

   deallocate(buf)

   CALL MPI_FINALIZE(err)
   END PROGRAM write_file
   ```

5. Run readFile_pointer and check that everything works correctly


## 3 - using set_view


1. Take a look at the F90 code where `file_set_view` routine is introduced

2. Compile the code and compare results with output of writeFile_pointer.f90

3. Modify the code to use file_set_view using mpi derived data_type (MPI_TYPE_CONTIGUOUS or MPI_TYPE_VECTOR) and get the same results as the code you start from.

4. Modify the code to write the file using the following pattern (assuming you use 4 processes):

   1 2 11 12 21 22 31 32 3 4 13 14 23 24 33 34

   (hint: use MPI_TYPE_VECTOR)


4 MPI_SubArray.c (Optional) 
===========================

1. Compile and run the MPI_SubArray.c code. The code is set now to run on a grid of 2x3 processes, and using a global matrix of 8x15. The code can
   just use a matrix such that the number of rows that is a multiple of the number of rows in the grid, and the number of columns that is multiple of the number of columns in the grid.

   1. Check, by reading the code and by means of od -i, what the code is doing.

2. Modify the code such it can handle an arbitrary matrix and an arbitrary grid.
   In the provided_code folder you can find a binary file called datafile_10x17_grid_3x5.
   This is the expected output for a matrix with 10 rows and 17 columns, on a process grid with
   3 rows and 5 columns. Your code should be able to produce this file, with the above parameters.


! Created by S. Cozzini and G.P. Brandino for the Course P1.6 Data Management @ MHPC
! Last Revision: November 2015

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

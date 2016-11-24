! Created by S. Cozzini and G.P. Brandino for the Course P1.6 Data Management @ MHPC
! Last Revision: November 2015

PROGRAM write_using_set_view

  USE mpi

  INTEGER :: ierr, i, myrank, thefile
  INTEGER, PARAMETER :: BUFSIZE=4
  INTEGER, DIMENSION(BUFSIZE) :: buf
  INTEGER(kind=MPI_OFFSET_KIND) :: disp

  CALL MPI_INIT(ierr)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD, myrank, ierr)

  DO i = 1, BUFSIZE
     buf(i) = myrank * 10 + i
  ENDDO

! FIRST EXERCISE
  ! CALL MPI_TYPE_CONTIGUOUS(BUFSIZE, MPI_INT, my_new_type, ierr) ! Creiamo un type contiguo
  ! CALL MPI_TYPE_COMMIT(my_new_type, ierr) ! Committiamo il tyoe
  !
  ! CALL MPI_FILE_OPEN(MPI_COMM_WORLD, 'testfile', &
  !      MPI_MODE_WRONLY + MPI_MODE_CREATE, &
  !      MPI_INFO_NULL, thefile, ierr)
  !
  ! CALL MPI_TYPE_SIZE(my_new_type, intsize,ierr)
  !
  ! disp = myrank * 1 * intsize ! Ora devo contare il Type intero
  !
  ! !CALL MPI_FILE_SET_VIEW(thefile, disp, MPI_INTEGER, &
  !      !MPI_INTEGER, 'native', &
  !      !MPI_INFO_NULL, ierr)
  !
  ! CALL MPI_FILE_SET_VIEW(thefile, disp, MPI_INTEGER, &
  !       my_new_type, 'native', &
  !       MPI_INFO_NULL, ierr)
  !
  ! !CALL MPI_FILE_WRITE(thefile, buf, BUFSIZE, MPI_INTEGER, &
  !      !MPI_STATUS_IGNORE, ierr)
  !
  ! CALL MPI_FILE_WRITE(thefile, buf, 1, my_new_type, &
  !      MPI_STATUS_IGNORE, ierr)
  !
  ! CALL MPI_FILE_CLOSE(thefile, ierr)
  !
  ! CALL MPI_FINALIZE(ierr)

! SECOND EXERCISE

CALL MPI_TYPE_VECTOR(2,2,8, MPI_INTEGER, my_new_type, ierr) ! Creiamo un type che ci fa comodo
CALL MPI_TYPE_COMMIT(my_new_type, ierr) ! Committiamo il tyoe

CALL MPI_FILE_OPEN(MPI_COMM_WORLD, 'testfile', &
     MPI_MODE_WRONLY + MPI_MODE_CREATE, &
     MPI_INFO_NULL, thefile, ierr)

CALL MPI_TYPE_SIZE(MPI_INT, intsize,ierr)


disp = myrank * 2 * intsize
!write(*,*) MPI_INTEGER, MPI_INT, int_size, integersize

CALL MPI_FILE_SET_VIEW(thefile, disp, MPI_INTEGER, &
      my_new_type, 'native', &
      MPI_INFO_NULL, ierr)

CALL MPI_FILE_WRITE(thefile, buf, 4, MPI_INTEGER, &
     MPI_STATUS_IGNORE, ierr)

CALL MPI_FILE_CLOSE(thefile, ierr)

CALL MPI_FINALIZE(ierr)

END PROGRAM write_using_set_view

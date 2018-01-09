program tomography
    use parameters
    use inputparameterhandler
    use filehandler
    use systemmatrixhandler
    use customsolver
    implicit none
    include "mpif.h"
    integer :: n,m, inputdatalength, i,j, rounds, points
    integer, allocatable :: indexarray(:)
    !rk from parameters module
    real(kind=rk), allocatable :: inputdata(:,:), A(:,:), x(:), b(:), bestx(:), message(:)
    real(kind=rk) :: xmax, ymax, value, best_value
    character(len=maxbuffer) :: inputdatafile, outputfile
    integer(kind=8) :: seed

    !mpi variables
    integer :: myid, ntasks, ierror, tag=1, stat(MPI_STATUS_SIZE)

    !init MPI
    call MPI_INIT(ierror)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, ntasks, ierror)
    call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierror)

    call getinputparameters(n,m,xmax,ymax,inputdatafile,outputfile, points, rounds)

    !from filehandler
    inputdatalength = getinputdatalength(inputdatafile)
    !inputdatacolumns from paramameters module
    allocate(inputdata(inputdatalength,inputdatacolumns))
    call loadinputdata(inputdatafile, inputdata, inputdatalength)

    allocate(A(inputdatalength,n*m))
    call inputdatatosystemmatrix(A,inputdatalength,n*m,inputdata,inputdatalength,xmax,ymax,n,m)

    allocate(x(n*m))
    allocate(bestx(n*m))
    allocate(message(n*m+1))
    allocate(b(inputdatalength))

    !now solve Ax = b
    b = inputdata(:,5)

    seed = 161612 + myid*(24626+myid)
    call init_genrand64(seed)
   
    !Essentially do i=1,points in parallel, note that i is just a counter of rounds and has no meaning	
    do i=1+myid, points, ntasks
	do j=1,n*m
	    x(j) = genrand64_real1()*xmax
	end do
        call solveaxb(A,n,m,inputdatalength,x,n*m,b,inputdatalength,20.0d0, rounds, value) 
	if(value<best_value .or. i==1+myid) then
		bestx = x
		best_value = value
	end if
	if(mod(i,points/100)==0 .and. points>1000) then
		print *, i/(points/100), "% done"
	end if
    end do

    !slaves send the best x they have
    if(myid/=0) then
	message(1:n*m) = bestx
	message(n*m+1) = best_value
	call MPI_SEND(message, m*n+1, MPI_DOUBLE_PRECISION, 0,tag,MPI_COMM_WORLD, ierror)
    end if

    !master receives and compares to others
    if(myid==0) then
	print *,"best values:", 0, best_value 
	do i=1,ntasks-1
	    call MPI_RECV(message, m*n+1, MPI_DOUBLE_PRECISION, i, tag, MPI_COMM_WORLD, stat, ierror)
	    print *,"best values:", i, message(m*n+1) 
            if(message(n*m+1) < best_value) then
		bestx = message(1:m*n)
		best_value = message(m*n+1)
	    end if
	end do
    end if

    x = bestx
    if(myid==0) then
    	print *, "best final:", best_value
    end if

    if(myid==0) then
	open(unit=1,file="asd.txt")
	do i = 1, m*n, n
	    write(1,*) x(i:i+m-1)
	end do
	close(1)
    end if

    call MPI_FINALIZE(ierror)

end program tomography

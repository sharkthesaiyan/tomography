program tomography
    use parameters
    use inputparameterhandler
    use filehandler
    use systemmatrixhandler
    use customsolver
    implicit none
    integer :: n,m, inputdatalength, i
    integer, allocatable :: indexarray(:)
    !rk from parameters module
    real(kind=rk), allocatable :: inputdata(:,:), A(:,:), x(:), b(:)
    real(kind=rk) :: xmax, ymax
    character(len=maxbuffer) :: inputdatafile, outputfile
    integer(kind=8) :: seed = 272161513

    call getinputparameters(n,m,xmax,ymax,inputdatafile,outputfile)

    !from filehandler
    inputdatalength = getinputdatalength(inputdatafile)
    !inputdatacolumns from paramameters module
    allocate(inputdata(inputdatalength,inputdatacolumns))
    call loadinputdata(inputdatafile, inputdata, inputdatalength)

    allocate(A(inputdatalength,n*m))
    call inputdatatosystemmatrix(A,inputdatalength,n*m,inputdata,inputdatalength,xmax,ymax,n,m)

    allocate(x(n*m))
    allocate(b(inputdatalength))


    !now just solve Ax = b, using lapack subroutine DGELSS or something else
    b = inputdata(:,5)

    call solveaxb(A,n,m,inputdatalength,x,n*m,b,inputdatalength,20.0d0, seed) 

    open(unit=1,file="asd.txt")
	do i = 1, m*n, n
		write(1,*) x(i:i+m-1)
	end do
    close(1)

end program tomography

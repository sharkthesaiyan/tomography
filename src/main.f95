program tomography
    use parameters
    use inputparameterhandler
    use filehandler
    use systemmatrixhandler
    implicit none
    integer :: n,m, inputdatalength, i
    integer, allocatable :: indexarray(:)
    !rk from parameters module
    real(kind=rk), allocatable :: grid(:,:), inputdata(:,:), A(:,:), x(:), b(:)
    real(kind=rk) :: xmax, ymax
    character(len=maxbuffer) :: inputdatafile, outputfile

    call getinputparameters(n,m,xmax,ymax,inputdatafile,outputfile)

    print *, n,m,inputdatafile,outputfile,xmax,ymax

    if(n>0 .and. m>0) then
        allocate(grid(n,m))
    end if

    !from filehandler
    inputdatalength = getinputdatalength(inputdatafile)
    !inputdatacolumns from paramameters module
    allocate(inputdata(inputdatalength,inputdatacolumns))
    call loadinputdata(inputdatafile, inputdata, inputdatalength)

    allocate(A(inputdatalength,n*m))

    call inputdatatosystemmatrix(A,inputdatalength,n*m,inputdata,inputdatalength,xmax,ymax,n,m)

    print *, A

    !now just solve Ax = b, using lapack subroutine DGELSS or something else

!    open(unit=1,file="asd.txt")
!	do i = 1, m*n, n
!		write(1,*) x(i:i+m-1)
!	end do
!    close(1)

end program tomography

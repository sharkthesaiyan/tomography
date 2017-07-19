program tomography
    use parameters
    use inputparameterhandler
    use filehandler
    use systemmatrixhandler
    implicit none
    external DGELSS 
    integer :: n,m, inputdatalength, lwork, info, i, rank
    !rk from parameters module
    real(kind=rk), allocatable :: grid(:,:), inputdata(:,:), A(:,:), x(:), b(:), singulars(:), workarray(:)
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

    print *, "asdasdsad"
    do i =1,inputdatalength
    	print *, inputdata(i,:)
    end do
    print *, "asdasdasds"

    allocate(A(inputdatalength,n*m))

    print *, "aaa:", shape(a)

    allocate(singulars(min(inputdatalength,n*m)))
    call inputdatatosystemmatrix(A,inputdatalength,n*m,inputdata,inputdatalength,xmax,ymax,n,m)

    allocate(x(n*m))
    allocate(b(max(inputdatalength,n*m)))

    b(1:inputdatalength) = inputdata(:,5) 

    lwork = 2*(3*min(inputdatalength,n*m) + max(2*min(inputdatalength,m*n),max(inputdatalength,m*n), 1))
    allocate(workarray(lwork))

!    print '(25(f5.2,x))', transpose(A)
    print *, ""
!    print *, b

    !now just solve Ax = b, using lapack subroutine DGELSS
    call DGELSS(inputdatalength,n*m,1,A,inputdatalength,b,max(inputdatalength,n*m),singulars,rcond,rank,workarray,lwork,info)
    
    print *, ""
    print *, info
    print *, ""
!    print "(5f7.3)", b(1:25)

    open(unit=1,file="asd.txt")
	do i = 1, m*n, n
		write(1,*) b(i:i+m-1)
	end do
    close(1)

end program tomography

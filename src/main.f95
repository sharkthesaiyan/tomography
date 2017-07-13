program tomography
    use parameters
    use inputparameterhandler
    use filehandler
    use systemmatrixhandler
    implicit none
    integer :: n,m, inputdatalength
    !rk from parameters module
    real(kind=rk), allocatable :: grid(:,:), inputdata(:,:), systemmatrix(:,:)
    character(len=maxbuffer) :: inputdatafile, outputfile

    call getinputparameters(n,m,inputdatafile,outputfile)

    print *, n,m,inputdatafile,outputfile

    if(n>0 .and. m>0) then
        allocate(grid(n,m))
    end if

    !from filehandler
    inputdatalength = getinputdatalength(inputdatafile)
    !inputdatacolumns from paramameters module
    allocate(inputdata(inputdatalength,inputdatacolumns))
    call loadinputdata(inputdatafile, inputdata, inputdatalength)

    allocate(systemmatrix(inputdatalength,n*m))
    call inputdatatosystemmatrix(systemmatrix,inputdatalength,n*m,inputdata,inputdatalength)

end program tomography

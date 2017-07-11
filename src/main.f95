program tomography
    use parameters
    use inputparameterhandler
    implicit none
    integer :: n,m
    character(len=maxbuffer) :: inputdatafile, outputfile

    call getinputparameters(n,m,inputdatafile,outputfile)

    print *, n,m,inputdatafile,outputfile

end program tomography

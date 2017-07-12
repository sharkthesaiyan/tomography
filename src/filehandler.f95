module filehandler
	use parameters
	implicit none

contains

	subroutine loadinputdata(inputdatafile, inputdata, inputdatalength)
		implicit none
		character(len=maxbuffer), intent(in) :: inputdatafile
		integer, intent(in) :: inputdatalength
		real(kind=rk) :: inputdata(inputdatalength, inputdatacolumns)
		integer :: i, dummy
		
		
		open(unit=1,file=inputdatafile)
			!move to second row of the file where the actual data begins
			read(1,*) dummy
			do i=1,inputdatalength
				read(1,*) inputdata(i,:)
			end do
		close(1)


	end subroutine loadinputdata	
	
	integer function getinputdatalength(inputdatafile)
		implicit none
		!maxbuffer from parameters module
		character(len=maxbuffer), intent(in) :: inputdatafile
		integer :: length

		!data length is the only thing in first row
		open(unit=1,file=inputdatafile)
			read(1,*) length
		close(1)
		
		getinputdatalength = length

	end function getinputdatalength
	

end module filehandler

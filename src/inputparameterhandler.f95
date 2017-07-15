module inputparameterhandler
	use parameters
	implicit none
	
contains
	!rk and maxbuffer from parameters module
	subroutine getinputparameters(n,m,xmax,ymax,inputdatafile,outputfile)
		implicit none
		integer, intent(inout) :: n,m
		real(kind=rk), intent(inout) :: xmax, ymax
		character(len=maxbuffer) :: inputdatafile, outputfile

		if(command_argument_count()==6) then
			n = cmd2int(1)
			m = cmd2int(2)
			xmax = cmd2real(3)
			ymax = cmd2real(4)
			call get_command_argument(5,inputdatafile)
			call get_command_argument(6,outputfile)			
		else
			write(stderr,*), "Wrong number of command line arguments"
		end if
	end subroutine getinputparameters


	real(kind=rk) function cmd2real(n)
		implicit none
		integer, intent(in) :: n
		character(len=maxbuffer) :: arg
		
		if(command_argument_count()>=n) then
			call get_command_argument(n,arg)
			read(arg,*) cmd2real
		end if
	end function cmd2real

	integer function cmd2int(n)
		implicit none
		integer, intent(in) :: n
		character(len=maxbuffer) :: arg
		
		if(command_argument_count()>=n) then
			call get_command_argument(n,arg)
			read(arg,*) cmd2int
		end if
	end function cmd2int

end module inputparameterhandler

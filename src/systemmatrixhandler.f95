module systemmatrixhandler
	use parameters
	implicit none
	
contains

	function measurementToMatrixRow(inputrow,gridunits)
		implicit none
		integer, intent(in) :: gridunits
		real(kind=rk) :: measurementToMatrixRow(gridunits)
		real(kind=rk), intent(in) :: inputrow(inputdatacolumns)		
		real(kind=rk) :: startcoordinates(2), endcoordinates(2)

	end function measurementToMatrixRow

	subroutine inputdatatosystemmatrix(systemmatrix,n,m,inputdata, datalength)
		implicit none
		integer,intent(in) :: n,m,datalength
		real(kind=rk) :: systemmatrix(n,m), inputdata(datalength,inputdatacolumns)
		integer :: i

		do i=1,datalength
			systemmatrix(i,:) = measurementToMatrixRow(inputdata(i,:),m)
		end do

	end subroutine inputdatatosystemmatrix


end module systemmatrixhandler

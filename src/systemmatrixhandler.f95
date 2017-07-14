module systemmatrixhandler
	use parameters
	implicit none
	
contains

	subroutine followraylocation(currenti,currentj,startx,starty,k,travellength)
		implicit none
		integer :: currenti, currentj
		real(kind=rk) :: startx, starty, k, travellength
		real(kind=rk) :: xtime, ytime
		!possible directions of travel are right, up and down

		if(k>=0.0d0) then
			xtime =  1.0d0 - startx
			ytime = (1.0d0 - starty)/k
		else
			xtime =  (1.0d0 - startx)
			ytime = starty/(-1.0*k)
		end if

		if(xtime<ytime) then
			travellength = sqrt(xtime**2 + (xtime*k)**2 )
			startx = 0.0d0	
			starty = starty + xtime*k
			currentj = currentj + 1

		else if(k>=0.0) then
			travellength = sqrt(ytime**2 + (1.0 - starty)**2 )
			starty = 0.0d0
			startx = startx + ytime
			currenti = currenti + 1
		else
			travellength = sqrt(ytime**2 + starty**2 )
			starty = 1.0d0
			startx = startx + ytime
			currenti = currenti - 1
		end if

	end subroutine followraylocation

	function measurementToMatrixRow(inputrow,gridx,gridy)
		implicit none
		integer, intent(in) :: gridx, gridy
		real(kind=rk) :: measurementToMatrixRow(gridx*gridy)
		real(kind=rk), intent(in) :: inputrow(inputdatacolumns)		
		real(kind=rk) :: startcoordinates(2), endcoordinates(2)
		integer :: currenti, currentj, startx, starty


	end function measurementToMatrixRow

	subroutine inputdatatosystemmatrix(systemmatrix,n,m,inputdata,datalength)
		implicit none
		integer,intent(in) :: n,m,datalength
		real(kind=rk) :: systemmatrix(n,m), inputdata(datalength,inputdatacolumns)
		integer :: i

		do i=1,datalength
			systemmatrix(i,:) = measurementToMatrixRow(inputdata(i,:),n,m)
		end do

	end subroutine inputdatatosystemmatrix


end module systemmatrixhandler

program kissa
	use systemmatrixhandler
	implicit none
	integer :: i,j
	real(kind=8) :: x,y,h,k

	i = 2
	j = 2
	k = 1.0	
	h = 0.0
	x = 0.0
	y = 0.0

	print *, j,i,x,y,k,h
	call followraylocation(i,j,x,y,k,h)
	print *, j,i,x,y,k,h
	call followraylocation(i,j,x,y,k,h)
	print *, j,i,x,y,k,h

end program kissa

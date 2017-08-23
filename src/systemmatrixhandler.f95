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
			currenti = currenti + 1

		else if(k>=0.0) then
			travellength = sqrt(ytime**2 + (1.0 - starty)**2 )
			starty = 0.0d0
			startx = startx + ytime
			currentj = currentj + 1
		else
			travellength = sqrt(ytime**2 + starty**2 )
			starty = 1.0d0
			startx = startx + ytime
			currentj = currentj - 1
		end if

	end subroutine followraylocation

	function measurementToMatrixRow(inputrow,gridrows,gridcolumns,xmax,ymax)
		implicit none
		integer, intent(in) :: gridrows, gridcolumns
		real(kind=rk) :: measurementToMatrixRow(gridrows*gridcolumns)
		real(kind=rk), intent(in) :: inputrow(gridrows*gridcolumns), xmax, ymax
		integer :: currenti, currentj, i, endi, endj, oldi, oldj
		real(kind=rk) :: startcoordinates(2), endcoordinates(2), k, rectanglesize, travellength, startx, starty, endx, endy
		
		startcoordinates = inputrow(1:2)
		endcoordinates = inputrow(3:4)
		
		k = (endcoordinates(2) - startcoordinates(2)) / (endcoordinates(1) - startcoordinates(1))

		rectanglesize = xmax/gridcolumns
		currenti = int(startcoordinates(1)/rectanglesize) + 1
		currentj = int(startcoordinates(2)/rectanglesize) + 1

		endi = int(endcoordinates(1)/rectanglesize) + 1 
		endj = int(endcoordinates(2)/rectanglesize) + 1

		startx = startcoordinates(1) - (currenti-1)*rectanglesize
		starty = startcoordinates(2) - (currentj-1)*rectanglesize
		
		measurementtomatrixrow = 0.0d0

		startx = startx/rectanglesize
		starty = starty/rectanglesize

		endx = endcoordinates(1) - (endi-1)*rectanglesize
		endy = endcoordinates(2) - (endj-1)*rectanglesize

		do
			if(currenti == endi .and. currentj == endj) then
				travellength = sqrt( (endx - startx*rectanglesize)**2 + (endy - starty*rectanglesize)**2 )					
				measurementtomatrixrow((currenti-1)*gridrows + currentj) = travellength
				exit
			end if
			oldi = currenti
			oldj = currentj
			call followraylocation(currenti, currentj, startx, starty, k, travellength)
			measurementtomatrixrow((oldi-1)*gridrows + oldj) = travellength*rectanglesize
		end do
		
	end function measurementToMatrixRow

	subroutine inputdatatosystemmatrix(systemmatrix,n,m,inputdata,datalength,xmax,ymax,gridn,gridm)
		implicit none
		integer,intent(in) :: n,m,datalength, gridn, gridm
		real(kind=rk) :: systemmatrix(n,m), inputdata(datalength,inputdatacolumns), xmax, ymax
		integer :: i

		do i=1,datalength
			systemmatrix(i,:) = measurementToMatrixRow(inputdata(i,:),gridn,gridm,xmax,ymax)
		end do

	end subroutine inputdatatosystemmatrix


end module systemmatrixhandler

!program kissa
!	use systemmatrixhandler
!	implicit none
!	integer :: i,j
!	real(kind=8) :: x,y,h,k
!
!	print *, measurementtomatrixrow([0.5d0,0.5d0,3.5d0,3.5d0,9.0d0],3,3,6.0d0,6.0d0)
!
!end program kissa

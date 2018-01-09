module customsolver
	use mt19937_64 
	use parameters
	implicit none

contains

	real(kind=rk) function punish(x,nx,m,n)
		implicit none
		integer, intent(in) :: nx,m,n
		real(kind=rk), intent(in) :: x(nx)
		integer :: i
		real(kind=rk) :: penalty, deriv2
		!punish for big jumps i.e. large second order derivatives
		penalty = 0.0d0
		do i=1, nx
			!----
			if(mod(i,m)/=1 .and. mod(i,m)/=0) then
				deriv2 = (x(i+1) - 2*x(i) + x(i-1))**2
				penalty = penalty + deriv2
			end if			
			
			if(i>m .and. i<n*(m-1)) then
				deriv2 = (x(i+m) - 2*x(i) + x(i-m))**2
				penalty = penalty + deriv2
			end if
		end do

		punish = penalty
		
	end function punish

	subroutine solveaxb(A,m,n,na,x,nx,b,nb,xmax,seed)
		implicit none
		integer, intent(in) :: m,n,na, nx, nb
		real(kind=rk), intent(in) :: A(na, m*n), b(nb), xmax
		real(kind=rk), intent(inout) :: x(nx)
		real(kind=rk) :: btest(nb),  erro(nb), y(nx), step, gradient(nx), old_value, new_value, best_x(nx), best_value
		integer :: i,j,k, rounds, points		
		integer(kind=8), intent(in) :: seed


		call init_genrand64(seed)
		step = 0.5
		rounds = 1000
		points = 100
		best_value = -1.0d0
		best_x = 0.0
		
		do k=1,points
			print *, k, new_value, best_value
			!First the initial guess for x
			do i=1,nx
				x(i) = genrand64_real1()*xmax
			end do

			!then minimize ||Ax-b|| + f(x) using gradient descend method
			old_value = norm2(b - matmul(A,x)) + punish(x,nx,m,n)
			do j = 1,rounds	
				do i=1,nx
					y = x
					y(i) = y(i) + step
					btest = matmul(A,y)
					erro = b - btest
					new_value = norm2(erro) + punish(x,nx,m,n)
					gradient(i) = new_value - old_value
				end do
				x = x - gradient*step/(norm2(gradient))
				!boundaries: no negative values or values over xmax
				do i=1,nx
					if(x(i)<0.0) then
						x(i) = 0.0d0	
					else if(x(i)>xmax) then
						x(i) = xmax
					end if
				end do	

				old_value = new_value
			end do
			!new_value = norm2(b - matmul(A,x)) + punish(x,nx,m,n)

			if(new_value < best_value .or. best_value < 0.0) then
				best_value = new_value
				best_x = x
			end if
		end do

		x = best_x

	end subroutine solveaxb 

end module customsolver

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

	subroutine solveaxb(A,m,n,na,x,nx,b,nb,xmax,rounds, final_value)
		implicit none
		integer, intent(in) :: m,n,na, nx, nb, rounds
		real(kind=rk), intent(in) :: A(na, m*n), b(nb), xmax
		real(kind=rk), intent(out) :: final_value
		real(kind=rk), intent(inout) :: x(nx)
		real(kind=rk) :: btest(nb),  erro(nb), y(nx), step, gradient(nx), old_value, new_value, punishweight
		integer :: i,j
		
		step = 0.5
		punishweight = 0.0

		old_value = norm2(b - matmul(A,x)) + punishweight*punish(x,nx,m,n)
		do j = 1,rounds	
			do i=1,nx
				y = x
				y(i) = y(i) + step
				btest = matmul(A,y)
				erro = b - btest
				new_value = norm2(erro) + punishweight*punish(x,nx,m,n)
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

			old_value = norm2(b - matmul(A,x)) + punishweight*punish(x,nx,m,n)
		end do

		final_value = old_value

	end subroutine solveaxb 

end module customsolver

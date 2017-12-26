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
		real(kind=rk) :: penalty
		!punish for big jumps
		penalty = 0.0d0
		do i=1, nx
			!----
			if(mod(i,m)/=1) then
				penalty = penalty + (x(i) - x(i-1))**2
			end if			
			
			if(i>m) then
				penalty = penalty + (x(i) - x(i-m))**2
			end if

		end do

		punish = penalty
		
	end function punish

	subroutine solveaxb(A,m,n,na,x,nx,b,nb,xmax,seed)
		implicit none
		integer, intent(in) :: m,n,na, nx, nb
		real(kind=rk), intent(in) :: A(m*n,na), b(nb), xmax
		real(kind=rk), intent(inout) :: x(nx)
		real(kind=rk) :: btest(nb),  erro(nb), y(nx), step, gradient(nx), old_value, new_value
		integer :: i,j,k, rounds, points		
		integer(kind=8), optional :: seed

		if(present(seed)) then
			call init_genrand64(seed)
		else
			seed = 157361865			
			call init_genrand64(seed)
		end if

		step = 0.1
		rounds = 100
		points = 100

		do k=1,points
			!First the initial guess for x
			do i=1,nx
				x(i) = genrand64_real1()*xmax
			end do

			!then minimize ||Ax-b|| + f(x) using gradient descend method
			old_value = norm2(b - matmul(A,x)) + punish(x,nx,m,n)
			do j = 1,rounds
				x = 0.0d0	

				do i=1,nx
					y = x
					y(i) = y(i) + step
					btest = matmul(A,y)
					erro = b - btest
					new_value = norm2(erro) + punish(x,nx,m,n)
					gradient(i) = new_value - old_value
				end do
				x = x - gradient*step
			
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
		end do

	end subroutine solveaxb 

end module customsolver

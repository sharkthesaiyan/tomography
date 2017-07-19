module parameters
	use, intrinsic :: iso_fortran_env, only : stdin=>input_unit, &
                                          stdout=>output_unit, &
                                          stderr=>error_unit

	integer, parameter :: 	rk = selected_real_kind(10,40), & !64 bit
       &			maxbuffer = 100, &
       &			inputdatacolumns = 5
	real(kind=rk), parameter :: 	pi = 3.14159265358979323846264, rcond = -1.0

end module parameters

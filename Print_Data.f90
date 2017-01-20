subroutine print_Data(numX,numY,U1,U2,U3,U4,t,gamma)
	real*8,dimension(numY,numX),intent(in)::U1,U2,U3,U4
	real*8,intent(in)::gamma,t
	
	do i=1,numX
		do j=1,numY
			write(404) U1(j,i)
			write(405) U2(j,i)
			write(406) U3(j,i)
			write(407) U4(j,i)
		end do
	end do
	write(408,"(100ES25.15)") t
	
end subroutine
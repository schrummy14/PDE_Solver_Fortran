subroutine getQ(U1,U2,U3,U4,r,x_vel,y_vel,P,gamma)

	real*8,intent(out)::r,x_vel,y_vel,P
	real*8,intent(in) ::U1,U2,U3,U4,gamma

	r     = U1
	x_vel = U2/r
	y_vel = U3/r
	P     = (gamma-1)*(U4-r*(x_vel**2+y_vel**2)/2)

end subroutine
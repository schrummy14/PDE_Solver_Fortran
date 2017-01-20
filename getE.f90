subroutine getE(U1,U2,U3,U4,E1,E2,E3,E4,gamma)

	real*8,intent(out)::E1,E2,E3,E4
	real*8,intent(in) ::U1,U2,U3,U4,gamma
	real*8::r,x_vel,y_vel,P

	call getQ(U1,U2,U3,U4,r,x_vel,y_vel,P,gamma)
	
	E1 = r*x_vel
	E2 = r*x_vel**2+P
	E3 = r*x_vel*y_vel
	E4 = x_vel*(U4+P)

end subroutine
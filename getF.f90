subroutine getF(U1,U2,U3,U4,F1,F2,F3,F4,gamma)

	real*8,intent(out)::F1,F2,F3,F4
	real*8,intent(in) ::U1,U2,U3,U4,gamma
	real*8::r,x_vel,y_vel,P

	call getQ(U1,U2,U3,U4,r,x_vel,y_vel,P,gamma)

	F1 = r*y_vel
	F2 = r*x_vel*y_vel
	F3 = r*y_vel**2+P
	F4 = y_vel*(U4+P)

end subroutine
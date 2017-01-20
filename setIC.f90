subroutine setIC(numX,numY,X,Y,U1,U2,U3,U4,dx,dy,gamma)
	real*8,dimension(numY,numX),intent(out)::X,Y,U1,U2,U3,U4
	real*8,intent(out)::dx,dy
	real*8,intent(in)::gamma
	integer,intent(in)::numX,numY
	real*8::roe0,u0,v0,P0,roeN,uN,vN,PN
	integer::i,j
	
	dx = 20.0/(numX-1)
	dy = 20.0/(numY-1)
	
	u0 = 0.0
	v0 = 0.0
	P0 = 101325.0
	roe0 = P0/(8.314*300.0)
	
	uN = 0.0
	vN = 0.0
	PN = 75*P0
	roeN = PN/(8.314*300.0)
	
	do j=1,numY
		do i=1,numX
			X(j,i) = (i-1)*dx - 10
			Y(j,i) = (j-1)*dy - 10
			write(409,"(100ES25.15)"),X(j,i),Y(j,i)
			if(((X(j,i)+10)**2+(Y(j,i)-10)**2)<4) then
				U1(j,i) = roeN
				U2(j,i) = roeN*uN
				u3(j,i) = roeN*vN
				u4(j,i) = PN/(gamma-1) + roeN*(uN**2+vN**2)/2
			!else if(((X(j,i)-5)**2+(Y(j,i))**2)<4) then
			!    U1(j,i) = roeN
			!    U2(j,i) = roeN*uN
			!    u3(j,i) = roeN*vN
			!    u4(j,i) = PN/(gamma-1) + rN*(uN**2+vN**2)/2
			else
				U1(j,i) = roe0
				U2(j,i) = roe0*u0
				u3(j,i) = roe0*v0
				u4(j,i) = P0/(gamma-1) + roe0*(u0**2+v0**2)/2
			end if
		end do
	end do

end subroutine
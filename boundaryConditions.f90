subroutine boundaryConditions(numX,numY,X,Y,U1,U2,U3,U4)
	
	integer,intent(in)::numX,numY
	
	real*8,dimension(numY,numX),intent(inout)::U1,U2,U3,U4
	real*8,dimension(numY,numX),intent(in)::X,Y
	
	integer::i,j

	do j=1,numY
		U1(j,1) = U1(j,2)
		U2(j,1) = -U2(j,2)
		U3(j,1) = -U3(j,2)
		U4(j,1) = U4(j,2)
	end do

	do i=1,numX
		U1(1,i) = U1(2,i)
		U2(1,i) = -U2(2,i)
		U3(1,i) = -U3(2,i)
		U4(1,i) = U4(2,i)
	end do

	do j=1,numY
		U1(j,numX) = U1(j,numX-1)
		U2(j,numX) = -U2(j,numX-1)
		U3(j,numX) = -U3(j,numX-1)
		U4(j,numX) = U4(j,numX-1)
	end do

	do i=1,numX
		U1(numY,i) = U1(numY-1,i)
		U2(numY,i) = -U2(numY-1,i)
		U3(numY,i) = -U3(numY-1,i)
		U4(numY,i) = U4(numY-1,i)
	end do
	
end subroutine
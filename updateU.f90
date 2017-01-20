subroutine updateU(numX,numY,dx,dy,dt,U1,U2,U3,U4,gamma)
	real*8,dimension(numY,numX),intent(inout)::U1,U2,U3,U4
	real*8,intent(in)::dx,dy,dt,gamma
	integer,intent(in)::numX,numY
	real*8,dimension(numY,numX)::res1,res2,res3,res4
	real*8,dimension(numY,numX)::U1temp1,U2temp1,U3temp1,U4temp1
	real*8,dimension(numY,numX)::U1temp2,U2temp2,U3temp2,U4temp2
	
	call roeSolver(NumX,NumY,dx,dy,dt,U1,U2,U3,U4,res1,res2,res3,res4,gamma)
	U1temp1 = U1 + res1
	U2temp1 = U2 + res2
	U3temp1 = U3 + res3
	U4temp1 = U4 + res4
	
	call roeSolver(NumX,NumY,dx,dy,dt,U1temp1,U2temp1,U3temp1,U4temp1,res1,res2,res3,res4,gamma)
	U1temp2 = 0.75*U1 + 0.25*U1temp1 + 0.25*res1
	U2temp2 = 0.75*U2 + 0.25*U2temp1 + 0.25*res2
	U3temp2 = 0.75*U3 + 0.25*U3temp1 + 0.25*res3
	U4temp2 = 0.75*U4 + 0.25*U4temp1 + 0.25*res4
	
	call roeSolver(NumX,NumY,dx,dy,dt,U1temp2,U2temp2,U3temp2,U4temp2,res1,res2,res3,res4,gamma)
	U1 = 1.0/3.0*U1 + 2.0/3.0*(U1temp2 + 0.25*res1)
	U2 = 1.0/3.0*U2 + 2.0/3.0*(U2temp2 + 0.25*res2)
	U3 = 1.0/3.0*U3 + 2.0/3.0*(U3temp2 + 0.25*res3)
	U4 = 1.0/3.0*U4 + 2.0/3.0*(U4temp2 + 0.25*res4)
	
end subroutine
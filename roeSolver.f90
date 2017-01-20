subroutine roeSolver(NumX,NumY,dx,dy,dt,U1,U2,U3,U4,res1,res2,res3,res4,gamma)

	real*8,intent(out)::res1(NumY,NumX),res2(NumY,NumX),res3(NumY,NumX),res4(NumY,NumX)
	real*8,intent(in) ::dx,dy,dt,U1(NumY,NumX),U2(NumY,NumX),U3(NumY,NumX),U4(NumY,NumX),gamma
	
	real*8::Eip12_1,Eip12_2,Eip12_3,Eip12_4,Fjp12_1,Fjp12_2,Fjp12_3,Fjp12_4
	real*8::Eim12_1,Eim12_2,Eim12_3,Eim12_4,Fjm12_1,Fjm12_2,Fjm12_3,Fjm12_4
	real*8::E1,E2,E3,E4
	real*8::E1p1,E2p1,E3p1,E4p1
	real*8::E1m1,E2m1,E3m1,E4m1
	real*8::F1,F2,F3,F4
	real*8::F1p1,F2p1,F3p1,F4p1
	real*8::F1m1,F2m1,F3m1,F4m1
	integer::i,j
	
	do j=2,NumY-1
		do i=2,NumX-1
			
			call getE(U1(j,i+1),U2(j,i+1),U3(j,i+1),U4(j,i+1),E1p1,E2p1,E3p1,E4p1,gamma)
			call getE(U1(j,i-1),U2(j,i-1),U3(j,i-1),U4(j,i-1),E1m1,E2m1,E3m1,E4m1,gamma)
			call getE(U1(j,i),U2(j,i),U3(j,i),U4(j,i),E1,E2,E3,E4,gamma)
			
			call getF(U1(j+1,i),U2(j+1,i),U3(j+1,i),U4(j+1,i),F1p1,F2p1,F3p1,F4p1,gamma)
			call getF(U1(j-1,i),U2(j-1,i),U3(j-1,i),U4(j-1,i),F1m1,F2m1,F3m1,F4m1,gamma)
			call getF(U1(j,i),U2(j,i),U3(j,i),U4(j,i),F1,F2,F3,F4,gamma)
			
			call getRoeFlux_E(E1p1,E2p1,E3p1,E4p1,&
							  E1,E2,E3,E4,&
							  U1(j,i+1),U2(j,i+1),U3(j,i+1),U4(j,i+1),&
							  U1(j,i),U2(j,i),U3(j,i),U4(j,i),&
							  Eip12_1,Eip12_2,Eip12_3,Eip12_4,gamma)
			
			call getRoeFlux_E(E1,E2,E3,E4,&
							  E1m1,E2m1,E3m1,E4m1,&
							  U1(j,i),U2(j,i),U3(j,i),U4(j,i),&
							  U1(j,i-1),U2(j,i-1),U3(j,i-1),U4(j,i-1),&
							  Eim12_1,Eim12_2,Eim12_3,Eim12_4,gamma)
			
			call getRoeFlux_F(F1,F2,F3,F4,&
							  F1m1,F2m1,F3m1,F4m1,&
							  U1(j,i),U2(j,i),U3(j,i),U4(j,i),&
							  U1(j-1,i),U2(j-1,i),U3(j-1,i),U4(j-1,i),&
							  Fjm12_1,Fjm12_2,Fjm12_3,Fjm12_4,gamma)
			
			call getRoeFlux_F(F1p1,F2p1,F3p1,F4p1,&
							  F1,F2,F3,F4,&
							  U1(j+1,i),U2(j+1,i),U3(j+1,i),U4(j+1,i),&
							  U1(j,i),U2(j,i),U3(j,i),U4(j,i),&
							  Fjp12_1,Fjp12_2,Fjp12_3,Fjp12_4,gamma)
			
			res1(j,i) = -dt/dx*(Eip12_1-Eim12_1) - dt/dy*(Fjp12_1-Fjm12_1)
			res2(j,i) = -dt/dx*(Eip12_2-Eim12_2) - dt/dy*(Fjp12_2-Fjm12_2)
			res3(j,i) = -dt/dx*(Eip12_3-Eim12_3) - dt/dy*(Fjp12_3-Fjm12_3)
			res4(j,i) = -dt/dx*(Eip12_4-Eim12_4) - dt/dy*(Fjp12_4-Fjm12_4)
			
		end do
	end do

end subroutine
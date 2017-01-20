subroutine getRoeFlux_F(E1R,E2R,E3R,E4R,&
						E1L,E2L,E3L,E4L,&
						U1R,U2R,U3R,U4R,&
						U1L,U2L,U3L,U4L,&
						Eip12_1,Eip12_2,Eip12_3,Eip12_4,gamma)

	real*8,intent(in) ::E1R,E2R,E3R,E4R,&
						E1L,E2L,E3L,E4L,&
						U1R,U2R,U3R,U4R,&
						U1L,U2L,U3L,U4L,gamma
	real*8,intent(out)::Eip12_1,Eip12_2,Eip12_3,Eip12_4
	real*8::roeVal_1,roeVal_2,roeVal_3,roeVal_4,roeR,roeL,eigVal,&
			roe_roe,roe_u,roe_v,roe_P
	
	roeR = U1R
	roeL = U1L
	
	call roeAverage(roeR,roeL,U1R,U1L,roeVal_1)
	call roeAverage(roeR,roeL,U2R,U2L,roeVal_2)
	call roeAverage(roeR,roeL,U3R,U3L,roeVal_3)
	call roeAverage(roeR,roeL,U4R,U4L,roeVal_4)
	
	call getQ(roeVal_1,roeVal_2,roeVal_3,roeVal_4,roe_roe,roe_u,roe_v,roe_P,gamma)
	
	eigVal = abs(roe_v) + sqrt(gamma*roe_P/roe_roe)
	
	Eip12_1 = 0.5*(E1R+E1L - eigVal*(U1R-U1L))
	Eip12_2 = 0.5*(E2R+E2L - eigVal*(U2R-U2L))
	Eip12_3 = 0.5*(E3R+E3L - eigVal*(U3R-U3L))
	Eip12_4 = 0.5*(E4R+E4L - eigVal*(U4R-U4L))

end subroutine
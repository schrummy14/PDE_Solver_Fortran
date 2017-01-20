subroutine set_dt(numX,numY,dx,dy,U1,U2,U3,U4,CFL_Num,dt,gamma)
	
	integer,intent(in)::numX,numY
	
	real*8,dimension(numY,numX),intent(in) :: U1,U2,U3,U4
	real*8,intent(in) ::dx,dy,CFL_Num,gamma
	real*8,intent(out)::dt
	real*8::r,u,v,P,max_eig_x,max_eig_y,eig_x,eig_y,speed_sound
	
	max_eig_x = 0.0
	max_eig_y = 0.0
	
	do j=1,numY
		do i=1,numX
			call getQ(U1(j,i),U2(j,i),U3(j,i),U4(j,i),r,u,v,P,gamma)
			speed_sound = sqrt(gamma*P/r)
			eig_x = abs(u)+speed_sound
			eig_y = abs(v)+speed_sound
			if(eig_x>max_eig_x)then
				max_eig_x=eig_x
			end if
			if(eig_y>max_eig_y)then
				max_eig_y=eig_y
			end if
		end do
	end do
	
	dt = (CFL_Num*dx*dy)/(max_eig_x*dy+max_eig_y*dx)

end subroutine
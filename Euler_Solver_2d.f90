program Euler_Solver_2d

    implicit none

    ! Variables
    integer:: NumX, NumY
    real*8,parameter :: gamma=1.4
    
    real*8,allocatable,dimension(:,:)::X,Y,U1,U2,U3,U4
    real*8::dx,dy,dt,Begin_Time,End_Time,t,tspan,CFL_Num,tEnd,tstep
    integer::its,time_its
    logical::done
	character*1 creturn

    ! Body of Euler_Solver_2d
	print*,"Enter values for NumX, NumY, CFL Number, tEnd, and tstep"
	read(*,*),NumX,NumY,CFL_Num,tEnd,tstep
	
    call CPU_TIME(Begin_Time)
	allocate(X(NumY,NumX),Y(NumY,NumX),U1(NumY,NumX),U2(NumY,NumX),&
			 U3(NumY,NumX),U4(NumY,NumX))
	
	creturn = achar(13)
	!open(unit=403,file='Output_U1.bin',form='unformatted')
	open(unit=404,file='Output_U1.bin', STATUS='unknown', ACCESS="STREAM")
	open(unit=405,file='Output_U2.bin', STATUS='unknown', ACCESS="STREAM")
	open(unit=406,file='Output_U3.bin', STATUS='unknown', ACCESS="STREAM")
	open(unit=407,file='Output_U4.bin', STATUS='unknown', ACCESS="STREAM")
    !open(unit=404,file='Output_roe.txt',status='unknown')
    !open(unit=405,file='Output_u.txt',status='unknown')
    !open(unit=406,file='Output_v.txt',status='unknown')
    !open(unit=407,file='Output_P.txt',status='unknown')
    open(unit=408,file='Time.txt',status='unknown')
    open(unit=409,file='Corrds.txt',status='unknown')
    call setIC(numX,numY,X,Y,U1,U2,U3,U4,dx,dy,gamma)
	call boundaryConditions(numX,numY,X,Y,U1,U2,U3,U4)
	
	print *,'Size of Simulation = ',size(U1,2),' by ',size(U1,1)
    
    t = 0.0
    its = 0
    time_its=0
    dt = 0.0
    tspan = 0.0
    done = .false.
    do while(.true.)
        its = its+1
        if(t.eq.0.0.or.t>tspan.or.done)then
            time_its=time_its+1
			write(*,101,ADVANCE='NO')creturn,t/tEnd*100.0
101		FORMAT(a,' Percent Done = ',100f8.4)
            call print_Data(numX,numY,U1,U2,U3,U4,t,gamma)
            if(done)then
                goto 42
            end if
            tspan = tspan+tstep
        end if
        
        call set_dt(numX,numY,dx,dy,U1,U2,U3,U4,CFL_Num,dt,gamma)
		dt = min(dt,1.0*tstep)
        if(t+dt>tEnd)then
            done = .true.
            dt = tEnd-t
        end if
        
        call updateU(numX,numY,dx,dy,dt,U1,U2,U3,U4,gamma)
        call boundaryConditions(numX,numY,X,Y,U1,U2,U3,U4)

        t = t + dt
        
    end do

42  close(404)
    close(405)
    close(406)
    close(407)
    close(408)
    close(409)
    call CPU_TIME(End_Time)
	print*,''
    print*,'Total CPU time = ',End_Time-Begin_Time
    print*,'Total iterations = ',its
    print*,'Total time steps = ',time_its
 end program Euler_Solver_2d
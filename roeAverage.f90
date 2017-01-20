subroutine roeAverage(roeR,roeL,UR,UL,roeVal)

	real*8,intent(in) ::roeR,roeL,UR,UL
	real*8,intent(out)::roeVal
	
	roeVal = (sqrt(roeR)*UR + sqrt(roeL)*UL)/(sqrt(roeR)+sqrt(roeL))
	
end subroutine
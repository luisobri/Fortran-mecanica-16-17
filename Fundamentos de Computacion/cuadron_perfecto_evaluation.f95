logical function cuadron_perfecto_evaluation (a,b)
	implicit none
	double precision, INTENT(IN):: a, b
    if(a>0 .and. b>0) then
      if (mod(sqrt(a+b),1.)==0 .and. mod(sqrt(a-b),1.)==0) then
        cuadron_perfecto_evaluation=.true.
      else
	    cuadron_perfecto_evaluation=.false.
      end if
    end if
end function
        
    
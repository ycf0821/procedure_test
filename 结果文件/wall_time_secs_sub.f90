!>Purpose:
!>  This subroutine is used to Calculate current wall time
!>
!>Interface:
!>
!>Input:
!>  None
!>
!>Output:
!>  tcurrent:
!>
!>externals:
!>
!>
!>Method:
!>  
!>
!>Reference:
!>
!>
!>Author:
!>  2-Apr-2016 SLCOE *DUT*
!>  12-Apr-2016 SLCOE *DUT*
!>Modifications:
!>
!> 
!> 
!> 
!> End Modifications

subroutine wall_time_secs(tcurrent)
  use global, only: SP
  implicit none
  integer, dimension(8) :: walltime
  real(kind=SP), intent(out) :: tcurrent
  real(kind=SP) :: msecs,secs,mins,hrs,days,months,mscale,years

  call date_and_time(VALUES=walltime)

  msecs = real(walltime(8))
  secs = real(walltime(7))
  mins = real(walltime(6))
  hrs = real(walltime(5))
  days = real(walltime(3))
  months = real(walltime(2))
  years = real(walltime(1))

  if((months==1).or.(months==3).or.(months==5).or.  &
    (months==7).or.(months==8).or.(months==10).or.  &                                                                                   
    (months==12)) then
    mscale = 31.0
  elseif((months==4).or.(months==6).or.  &
          (months==9).or.(months==11)) then
    mscale = 30.0
  elseif(years==4*int(years/4)) then
    mscale = 29.0
  else
    mscale = 28.0
  endif

  tcurrent = months*mscale*24.0*60.0*60.0+days*24.0*60.0*60.0+  &
             hrs*60.0*60.0+60.0*mins+secs+msecs/1000.0

end subroutine wall_time_secs
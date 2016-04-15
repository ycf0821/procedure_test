!>Purpose:
!>  This subroutine is used to specify left boundary
!>  Called by update_wave_bc
!>
!>Interface:
!>
!>Input:
!>  None
!>
!>Output:
!>  None
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

Subroutine tidal_wave_left_boundary
  use global, only: SP,PI,ZERO,GRAV,Ibeg,Nloc,Eta,Hc,D,TIME
  implicit none
  integer :: j

  do j = 1,Nloc
    Eta(Ibeg,j) = 1.0*sin(2.0*PI*(2.0*TIME/86400.0))
    D(Ibeg,j) = Eta(Ibeg,j)+Hc(Ibeg,j)
  enddo
end Subroutine tidal_wave_left_boundary
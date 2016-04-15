!>Purpose:
!>  This subroutine is used to show statistics 
!>
!>Interface:
!>
!>Input:
!>  None
!>  
!>
!>Output:
!>  None
!> 
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

Subroutine statistics
  use global
  implicit none
  real(kind=SP) :: MassVolume,CellMass,Energy,MaxEta,MinEta,MaxU, &
              MaxV,MaxW
  integer :: i,j,k

  MassVolume = ZERO
  Energy = ZERO
  do j = Jbeg,Jend
    do i = Ibeg,Iend
      MassVolume = MassVolume+D(i,j)*dx*dy
      do k = Kbeg,Kend
        CellMass = Rho0*dsig(k)*D(i,j)*dx*dy
        Energy = Energy+CellMass*GRAV*(D(i,j)*sigc(k)-Hc(i,j))+  &
                 0.5*CellMass*(U(i,j,k)**2+V(i,j,k)**2+W(i,j,k)**2)
      enddo
    enddo
  enddo

  MaxEta = MAXVAL(Eta(Ibeg:Iend,Jbeg:Jend))
  MinEta = MINVAL(Eta(Ibeg:Iend,Jbeg:Jend))
  MaxU = MAXVAL(abs(U(Ibeg:Iend,Jbeg:Jend,Kbeg:Kend)))
  MaxV = MAXVAL(abs(V(Ibeg:Iend,Jbeg:Jend,Kbeg:Kend)))
  MaxW = MAXVAL(abs(W(Ibeg:Iend,Jbeg:Jend,Kbeg:Kend))) 


!>print log file 
  WRITE(3,*),'----------------- STATISTICS ----------------'
  WRITE(3,*),' TIME        DT         DT_CONSTRAINT'
  WRITE(3,'(2E12.4,A8)') TIME,dt,TRIM(dt_constraint)
  WRITE(3,*),' MassVolume  Energy      MaxEta      MinEta      Max U       Max V       MaxW'
  WRITE(3,'(10E12.4)'), MassVolume,Energy,MaxEta,MinEta,MaxU,MaxV,MaxW

end Subroutine statistics

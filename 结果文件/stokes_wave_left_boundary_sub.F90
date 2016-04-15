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
!>  Newton-Ralphson Method
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

Subroutine stokes_wave_left_boundary
  use global, only: SP,PI,ZERO,Ibeg,GRAV,TIME,Nloc,Kloc,Amp_Wave,Per_Wave,Dep_Wave, &
                     Ein_X0,Din_X0,Uin_X0,Vin_X0,Win_X0,Hfx,Jbeg,Jend,Kbeg,Kend,sigc
  implicit none
  integer  :: j,k,Iter
  real(kind=SP) :: Segma,Celerity,Wave_Length,Wave_Number,Fk,Fkdif,Zlev

!>Find wave number for linear wave (Newton-Ralphson Method)
  Segma = 2.0*PI/Per_Wave
  Celerity = sqrt(GRAV*Dep_Wave)
  Wave_Length = Celerity*Per_Wave
  Wave_Number = 2.0*PI/Wave_Length
     
  Iter = 0
  do
    Fk = GRAV*Wave_Number*tanh(Wave_Number*Dep_Wave)-Segma**2
    if(abs(Fk)<=1.0e-8.or.Iter>1000) exit
    Fkdif = GRAV*Wave_Number*Dep_Wave*(1.0-tanh(Wave_Number*Dep_Wave)**2)+  &
    GRAV*tanh(Wave_Number*Dep_Wave) 
    Wave_Number = Wave_Number-Fk/Fkdif
    Iter = Iter+1
  enddo
  Wave_Length = 2.0*PI/Wave_Number
  Celerity = Wave_Length/Per_Wave
   
  do j = 1,Nloc
    Ein_X0(j) = 0.5*Amp_Wave*cos(PI/2-Segma*TIME)+  &
                Amp_Wave**2*Wave_Number/16.*cosh(Wave_Number*Dep_Wave)/  &
                sinh(Wave_Number*Dep_Wave)**3*(2.0+cosh(2.*Wave_Number*Dep_Wave))* &
                cos(2.*(PI/2-Segma*TIME))
    Din_X0(j) = Ein_X0(j)+Hfx(Ibeg,j)     
  enddo

  do k = Kbeg,Kend
    do j = Jbeg,Jend
      Zlev = sigc(k)*Din_X0(j)
      Uin_X0(j,k) = 0.5*Amp_Wave*Segma*cosh(Wave_Number*Zlev)/  &
                    sinh(Wave_Number*Dep_Wave)*cos(PI/2-Segma*TIME)+  &
                    3./16.*Amp_Wave**2*Segma*Wave_Number*cosh(2.*Wave_Number*Zlev)/  &
                    sinh(Wave_Number*Dep_Wave)**4*cos(2.*(PI/2-Segma*TIME))
      Win_X0(j,k) = 0.5*Amp_Wave*Segma*sinh(Wave_Number*Zlev)/  &
                    sinh(Wave_Number*Dep_Wave)*sin(PI/2-Segma*TIME)+  &
                    3./16.*Amp_Wave**2*Segma*Wave_Number*sinh(2.*Wave_Number*Zlev)/  &
                    sinh(Wave_Number*Dep_Wave)**4*sin(2.*(PI/2-Segma*TIME))
      Vin_X0(j,k) = 0.0
    enddo
  enddo

end Subroutine stokes_wave_left_boundary
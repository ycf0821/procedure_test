!>Purpose:
!>  This subroutine is used to specify left and right boundary,
!>  only for large-scale domain because it may causing unreasonable 
!>  upwelling and downwelling
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


Subroutine tidal_elevation_left_right
  use global, only: SP,PI,ZERO,Ibeg,TIME,Nloc,Kloc, &
                     Ein_X0,Din_X0,Uin_X0,Vin_X0,Win_X0,Hfx,Jbeg,Jend,Kbeg,Kend, &
                     Ein_Xn,Din_Xn,Uin_Xn,Vin_Xn,Win_Xn,Iend1,Eta,D,Iend,Hc,MASK, &
                     icount_tide, NumTimeData, &
                     DataU_L,DataEta_L,DataU_R,DataEta_R,TimeData
  implicit none
  integer  :: j,k,Iter
  real(kind=SP) :: Segma,Celerity,Wave_Length,Wave_Number,Fk,Fkdif,Zlev,Stokes_Drift,Fact,Phs_Lag(Nloc)
  REAL(kind=SP) :: TMP1,TMP2
  REAL(kind=SP) :: Ztmp1,Ztmp2

!>process time series
  if(TIME>TimeData(icount_tide).AND.icount_tide<NumTimeData)then
    icount_tide=icount_tide+1
  endif


  if(icount_tide>1)then   !>tide start
    if(TIME>TimeData(icount_tide))then
      tmp2=ZERO
    else
      tmp2=(TimeData(icount_tide)-TIME) &
           /(TimeData(icount_tide)-TimeData(icount_tide-1))
    endif

    Ztmp1= DataEta_L(icount_tide)*(1.0_SP-tmp2)   &
             +DataEta_L(icount_tide-1)*tmp2
    Ztmp2= DataEta_R(icount_tide)*(1.0_SP-tmp2)   &
             +DataEta_R(icount_tide-1)*tmp2
  else

    Ztmp1=0.0
    Ztmp2=0.0

  endif  !>end tide start


  do j = 1,Nloc
    if(MASK(Ibeg,J)==1)then
      Eta(Ibeg,j) = Ztmp1
      D(Ibeg,j) = Eta(Ibeg,j)+Hc(Ibeg,j)
    endif
  enddo

  do j = 1,Nloc
    if(MASK(Iend,J)==1)then
      Eta(Iend,j) = Ztmp2
      D(Iend,j) = Eta(Iend,j)+Hc(Iend,j)
    endif
  enddo


end Subroutine tidal_elevation_left_right
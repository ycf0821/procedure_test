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

Subroutine tidal_flux_left_right
  use global, only: SP,PI,ZERO,Ibeg,TIME,Nloc,Kloc, &
                      Ein_X0,Din_X0,Uin_X0,Vin_X0,Win_X0,Hfx,Jbeg,Jend,Kbeg,Kend, &
                      Ein_Xn,Din_Xn,Uin_Xn,Vin_Xn,Win_Xn,Iend1,Eta,D,Iend,Hc,MASK,&
                      icount_tide, NumTimeData, &
                      DataU_L,DataEta_L,DataU_R,DataEta_R,TimeData, &
                      U,Nudging
 
  implicit none
  integer  :: j,k,Iter
  real(kind=SP) :: Segma,Celerity,Wave_Length,Wave_Number,Fk,Fkdif,Zlev,Stokes_Drift,Fact,Phs_Lag(Nloc)
  REAL(kind=SP) :: TMP1,TMP2
  REAL(kind=SP) :: Ztmp1,Ztmp2,Utmp1,Utmp2

!>process time series
  IF(TIME>TimeData(icount_tide).AND.icount_tide<NumTimeData)THEN
    icount_tide=icount_tide+1
  ENDIF
  IF(icount_tide>1)THEN  !>tide start
    IF(TIME>TimeData(icount_tide))THEN
      tmp2=ZERO
    ELSE
      tmp2=(TimeData(icount_tide)-TIME) &
           /(TimeData(icount_tide)-TimeData(icount_tide-1))
    ENDIF

    Ztmp1= DataEta_L(icount_tide)*(1.0_SP-tmp2)   &
             +DataEta_L(icount_tide-1)*tmp2
    Ztmp2= DataEta_R(icount_tide)*(1.0_SP-tmp2)   &
             +DataEta_R(icount_tide-1)*tmp2

    Utmp1= DataU_L(icount_tide)*(1.0_SP-tmp2)   &
             +DataU_L(icount_tide-1)*tmp2
    Utmp2= DataU_R(icount_tide)*(1.0_SP-tmp2)   &
             +DataU_R(icount_tide-1)*tmp2
  ELSE

    Ztmp1=0.0
    Ztmp2=0.0
    Utmp1=0.0
    Utmp2=0.0

  ENDIF  !>end tide start

!>NOTE always nudge surface elevation
  do j = 1,Nloc
    Ein_X0(j) = Ztmp1
    Din_X0(j) = Ein_X0(j)+Hfx(Ibeg,j)   
    Ein_Xn(j) = Ztmp2
    Din_Xn(j) = Ein_Xn(j)+Hfx(Iend1,j)  
  enddo

  do k = Kbeg,Kend
    do j = Jbeg,Jend
      IF(Nudging.OR.Utmp1>0.0)THEN
        Uin_X0(j,k)=Utmp1*MASK(Ibeg,J)
      ELSE
        Uin_X0(j,k)= U(Ibeg,j,k)
      ENDIF
      Win_X0(j,k) = 0.0
      Vin_X0(j,k) = 0.0
      IF(Nudging.OR.Utmp2<0.0)THEN
        Uin_Xn(j,k)= Utmp2*MASK(Iend,J)
      ELSE
        Uin_Xn(j,k) = U(Iend,j,k)
      ENDIF
      Win_Xn(j,k) = 0.0
      Vin_Xn(j,k) = 0.0
    enddo
  enddo  

!>force elevation at left and right boundaries
!>to avoid over-shooting 
  do j = 1,Nloc
    IF(MASK(Ibeg,J)==1)THEN
      Eta(Ibeg,j) = Ein_X0(j)
      D(Ibeg,j) = Eta(Ibeg,j)+Hc(Ibeg,j)
    ENDIF
  enddo

  do j = 1,Nloc
    IF(MASK(Iend,J)==1)THEN
      Eta(Iend,j) = Ein_Xn(j)
      D(Iend,j) = Eta(Iend,j)+Hc(Iend,j)
    ENDIF
  enddo
end Subroutine tidal_flux_left_right
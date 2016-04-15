!>Purpose:
!>  This subroutine is used to update time-varying bathymetry
!>  note: make a standard slide application 
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

Subroutine update_bathymetry
  use global
  implicit none
  integer :: i,j,m,n,iter
  real(kind=SP) :: Hd,alpha0,L0,T,bl,wl,e,kb,kw,x0,x1,x2,xr,Hb(Mloc,Nloc), &
          xt,xt1,zt,zt1,ut,t0,a0,ht,s0,st,uslide,yt,Hslide(Mloc1,Nloc1)
              
!>save old bathymetry
  Ho = Hc
  IF(ANA_BATHY)THEN
!>if use analytical slide, 1)need iteration 2) ac is time-dependent
    Hd=1.5  
    T = T_slide
    bl = b_slide
    wl = w_slide
    e = e_slide
    alpha0 = slope_slide*3.1415926/180.0
    kb = 2.0*0.8616/bl
    kw = 2.0*0.8616/wl
    x0=x0_slide
    L0 = x0/cos(alpha0)-T*tan(alpha0)
    xr = 0.0
    x1 = (L0-bl/2.)*cos(alpha0)+xr
    x2 = (L0+bl/2.)*cos(alpha0)+xr

    ut = term_v
    a0 = acceleration_lab
    t0 = ut/a0
    s0 = ut**2/a0
    st = s0*log(cosh(time/t0))
    L0 = x0/cos(alpha0)-T*tan(alpha0)+st
  ELSE
    uslide = ac_slide*time
    if (uslide > term_v) uslide = term_v
      x0_slide = x0_slide+dt*uslide*cos(alpha_slide)
      y0_slide = y0_slide+dt*uslide*sin(alpha_slide)
  ENDIF

  if(trim(adjustl(DEPTH_TYPE))=='CELL_GRID') then
    write(*,*) 'DEPTH_TYPE has to be cell_center'
  elseif(trim(adjustl(DEPTH_TYPE))=='CELL_CENTER') then
!>base bathymetry 
    IF(ANA_BATHY)THEN   !>lab analytical slide
      do j = Jbeg,Jend
        do i = Ibeg,Iend
          if(xc(i)<=xr) then
            Hb(i,j) = -(xr-xc(i))*tan(alpha0)
          elseif(xc(i)<=Hd/tan(alpha0)+xr) then
            Hb(i,j) = (xc(i)-xr)*tan(alpha0)
          else
            Hb(i,j) = Hd
          endif
          !>temporarily no runup
        enddo
      enddo

      do j = Jbeg,Jend
        do i = Ibeg,Iend
          if(xc(i)<=x1.or.xc(i)>=x2.or.yc(j)>=wl/2.) then
            Hc(i,j) = Hb(i,j)
          else
            iter = 1
            xt = (xc(i)-xr)/cos(alpha0)-L0
            zt = T/(1-e)*(1.0/cosh(kb*xt)/cosh(kw*yc(j))-e)
            do
              xt1 = ((xc(i)-xr)/cos(alpha0)-zt*tan(alpha0))-L0
              zt1 = T/(1-e)*(1.0/cosh(kb*xt1)/cosh(kw*yc(j))-e)
              if(abs(zt1-zt)/abs(zt)<=1.e-8)exit
              iter = iter+1
              zt = zt1
            enddo
            Hc(i,j) = Hb(i,j)-max(0.0,zt1)/cos(alpha0)
          endif
        enddo
      enddo

    ELSE  !>field slide
      Hb=DepC0
!>add landslide
!>to keep consistent with slide bathy at grid, assuming slide bathy at grid point
      do j = Jbeg,Jend1
        do i = Ibeg,Iend1
          xt = (x(i)-x0_slide)*cos(alpha_slide)+(y(j)-y0_slide)*sin(alpha_slide)
          yt = -(x(i)-x0_slide)*sin(alpha_slide)+(y(j)-y0_slide)*cos(alpha_slide)
          ht = T_slide/(1-e_slide)*(1./cosh(kb_slide*xt)/cosh(kw_slide*yt)-e_slide)
          Hslide(I,J) = max(0.0,ht)
        enddo
      enddo

!>interpolate into grid center
      do j=Jbeg,Jend
        do i=Ibeg,Iend
          ht=0.25*(Hslide(I,J)+Hslide(I+1,J)+Hslide(I,J+1) &
                   +Hslide(I+1,J+1))
          Hc(i,j) = Hb(i,j)-ht
        enddo
      enddo

    ENDIF  !>end analytical slide
  endif !>end cell center

!>ghost cells
  call phi_2D_coll(Hc)

!>reconstruct depth at x-y faces
  do j = 1,Nloc
    do i = 2,Mloc
      Hfx(i,j) = 0.5*(Hc(i-1,j)+Hc(i,j))
    enddo
    Hfx(1,j) = Hfx(2,j)
    Hfx(Mloc1,j) = Hfx(Mloc,j)
  enddo

  do i = 1,Mloc
    do j = 2,Nloc
      Hfy(i,j) = 0.5*(Hc(i,j-1)+Hc(i,j))
    enddo
    Hfy(i,1) = Hfy(i,2)
    Hfy(i,Nloc1) = Hfy(i,Nloc)
  enddo

!>derivatives of water depth at cell center
  do j = 1,Nloc
    do i = 1,Mloc
      DelxH(i,j) = (Hfx(i+1,j)-Hfx(i,j))/dx
      DelyH(i,j) = (Hfy(i,j+1)-Hfy(i,j))/dy
    enddo
  enddo

!>time derivative of water depth
  DeltHo = DeltH
  DeltH = ZERO
  do j = 1,Nloc
    do i = 1,Mloc
      DeltH(i,j) = (Hc(i,j)-Ho(i,j))/dt
    enddo
  enddo

!>second-order time derivative
  if(RUN_STEP>2) Delt2H = (DeltH-DeltHo)/dt

end Subroutine update_bathymetry
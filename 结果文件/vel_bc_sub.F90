!>Purpose:
!>  This subroutine is used to ensure Boundary conditions for velocity
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
Subroutine vel_bc
  use global
  implicit none
  integer :: i,j,k
  real(kind=SP) :: Wtop,Wbot,Cdrag,Phi,Kappa,Dz1

!>left and right boundary
  do k = Kbeg,Kend
    do j = Jbeg,Jend
      if(Bc_X0==1) then  !>free-slip wall
        do i = 1,NGHOST
          U(Ibeg-i,j,k) = -U(Ibeg+i-1,j,k)
          V(Ibeg-i,j,k) = V(Ibeg+i-1,j,k)
          W(Ibeg-i,j,k) = W(Ibeg+i-1,j,k)
          DU(Ibeg-i,j,k) = -DU(Ibeg+i-1,j,k)
          DV(Ibeg-i,j,k) = DV(Ibeg+i-1,j,k)
          DW(Ibeg-i,j,k) = DW(Ibeg+i-1,j,k)
        enddo
      elseif(Bc_X0==2) then !>no-slip wall
        do i =1,NGHOST
          U(Ibeg-i,j,k) = -U(Ibeg+i-1,j,k)
          V(Ibeg-i,j,k) = -V(Ibeg+i-1,j,k)
          W(Ibeg-i,j,k) = -W(Ibeg+i-1,j,k)
          DU(Ibeg-i,j,k) = -DU(Ibeg+i-1,j,k)
          DV(Ibeg-i,j,k) = -DV(Ibeg+i-1,j,k)
          DW(Ibeg-i,j,k) = -DW(Ibeg+i-1,j,k)
        enddo
      elseif(Bc_X0==3) then !>inflow
        do i = 1,NGHOST
          U(Ibeg-i,j,k) = 2.0*Uin_X0(j,k)-U(Ibeg,j,k)
          V(Ibeg-i,j,k) = 2.0*Vin_X0(j,k)-V(Ibeg,j,k)
          W(Ibeg-i,j,k) = 2.0*Win_X0(j,k)-W(Ibeg,j,k)
          DU(Ibeg-i,j,k) = 2.0*Uin_X0(j,k)*Din_X0(j)-DU(Ibeg,j,k)
          DV(Ibeg-i,j,k) = 2.0*Vin_X0(j,k)*Din_X0(j)-DV(Ibeg,j,k)
          DW(Ibeg-i,j,k) = 2.0*Win_X0(j,k)*Din_X0(j)-DW(Ibeg,j,k)
        enddo
      elseif(Bc_X0==4) then !>outflow
        do i = 1,NGHOST
          U(Ibeg-i,j,k) = U(Ibeg+i-1,j,k)
          V(Ibeg-i,j,k) = V(Ibeg+i-1,j,k)
          W(Ibeg-i,j,k) = W(Ibeg+i-1,j,k)
          DU(Ibeg-i,j,k) = DU(Ibeg+i-1,j,k)
          DV(Ibeg-i,j,k) = DV(Ibeg+i-1,j,k)
          DW(Ibeg-i,j,k) = DW(Ibeg+i-1,j,k)
        enddo
      endif
	enddo
  enddo

  do k = Kbeg,Kend
    do j = Jbeg,Jend
      if(Bc_Xn==1) then  !>free-slip wall 
        do i = 1,NGHOST
          U(Iend+i,j,k) = -U(Iend-i+1,j,k)
          V(Iend+i,j,k) = V(Iend-i+1,j,k)
          W(Iend+i,j,k) = W(Iend-i+1,j,k)
          DU(Iend+i,j,k) = -DU(Iend-i+1,j,k)
          DV(Iend+i,j,k) = DV(Iend-i+1,j,k)
          DW(Iend+i,j,k) = DW(Iend-i+1,j,k)
        enddo
      elseif(Bc_Xn==2) then !>no-slip wall
        do i = 1,NGHOST
          U(Iend+i,j,k) = -U(Iend-i+1,j,k)
          V(Iend+i,j,k) = -V(Iend-i+1,j,k)
          W(Iend+i,j,k) = -W(Iend-i+1,j,k)
          DU(Iend+i,j,k) = -DU(Iend-i+1,j,k)
          DV(Iend+i,j,k) = -DV(Iend-i+1,j,k)
          DW(Iend+i,j,k) = -DW(Iend-i+1,j,k)
        enddo
      elseif(Bc_Xn==3) then !>inflow
        do i = 1,NGHOST
          U(Iend+i,j,k) = 2.0*Uin_Xn(j,k)-U(Iend-i+1,j,k)
          V(Iend+i,j,k) = 2.0*Vin_Xn(j,k)-V(Iend-i+1,j,k)
          W(Iend+i,j,k) = 2.0*Win_Xn(j,k)-W(Iend-i+1,j,k)
          DU(Iend+i,j,k) = 2.0*Uin_Xn(j,k)*Din_Xn(j)-DU(Iend-i+1,j,k)
          DV(Iend+i,j,k) = 2.0*Vin_Xn(j,k)*Din_Xn(j)-DV(Iend-i+1,j,k)
          DW(Iend+i,j,k) = 2.0*Win_Xn(j,k)*Din_Xn(j)-DW(Iend-i+1,j,k)
        enddo
      elseif(Bc_Xn==4) then !>outflow
        do i = 1,NGHOST
          U(Iend+i,j,k) = U(Iend-i+1,j,k)
          V(Iend+i,j,k) = V(Iend-i+1,j,k)
          W(Iend+i,j,k) = W(Iend-i+1,j,k)
          DU(Iend+i,j,k) = DU(Iend-i+1,j,k)
          DV(Iend+i,j,k) = DV(Iend-i+1,j,k)
          DW(Iend+i,j,k) = DW(Iend-i+1,j,k)
        enddo
      endif
    enddo
  enddo

  do k = Kbeg,Kend
    do i = Ibeg,Iend
      if(Bc_Y0==1) then  !>free-slip wall 
        do j = 1,NGHOST
          U(i,Jbeg-j,k) = U(i,Jbeg+j-1,k)
          V(i,Jbeg-j,k) = -V(i,Jbeg+j-1,k)
          W(i,Jbeg-j,k) = W(i,Jbeg+j-1,k)
          DU(i,Jbeg-j,k) = DU(i,Jbeg+j-1,k)
          DV(i,Jbeg-j,k) = -DV(i,Jbeg+j-1,k)
          DW(i,Jbeg-j,k) = DW(i,Jbeg+j-1,k)
        enddo
      elseif(Bc_Y0==2) then !>no-slip wall 
        do j = 1,NGHOST
          U(i,Jbeg-j,k) = -U(i,Jbeg+j-1,k)
          V(i,Jbeg-j,k) = -V(i,Jbeg+j-1,k)
          W(i,Jbeg-j,k) = -W(i,Jbeg+j-1,k)
          DU(i,Jbeg-j,k) = -DU(i,Jbeg+j-1,k)
          DV(i,Jbeg-j,k) = -DV(i,Jbeg+j-1,k)
          DW(i,Jbeg-j,k) = -DW(i,Jbeg+j-1,k)
        enddo
      endif
    enddo
  enddo

  do k = Kbeg,Kend
    do i = Ibeg,Iend
      if(Bc_Yn==1) then  !>free-slip wall 
        do j = 1,NGHOST
          U(i,Jend+j,k) = U(i,Jend-j+1,k)
          V(i,Jend+j,k) = -V(i,Jend-j+1,k)
          W(i,Jend+j,k) = W(i,Jend-j+1,k)
          DU(i,Jend+j,k) = DU(i,Jend-j+1,k)
          DV(i,Jend+j,k) = -DV(i,Jend-j+1,k)
          DW(i,Jend+j,k) = DW(i,Jend-j+1,k)
        enddo
      elseif(Bc_Yn==2) then !>no-slip wall 
        do j =1,NGHOST
          U(i,Jend+j,k) = -U(i,Jend-j+1,k)
          V(i,Jend+j,k) = -V(i,Jend-j+1,k)
          W(i,Jend+j,k) = -W(i,Jend-j+1,k)
          DU(i,Jend+j,k) = -DU(i,Jend-j+1,k)
          DV(i,Jend+j,k) = -DV(i,Jend-j+1,k)
          DW(i,Jend+j,k) = -DW(i,Jend-j+1,k)
        enddo
      endif
    enddo
  enddo

!>top and bottom
  do j = Jbeg,Jend
    do i = Ibeg,Iend
      Kappa = 0.41
      Dz1 = D(i,j)*dsig(Kbeg)
      if(ibot==1) then
        Cdrag = Cd0
      else
        Cdrag = 1./(1./Kappa*log(15.0*Dz1/Zob))**2
      endif
      Phi = Dz1*Cdrag*sqrt(U(i,j,Kbeg)**2+V(i,j,Kbeg)**2)/CmuVt(i,j,Kbeg)
      Phi = dmin1(Phi,2.0)

      if(Bc_Z0==1) then  !>free-slip
        Wbot = -DeltH(i,j)-U(i,j,Kbeg)*DelxH(i,j)-V(i,j,Kbeg)*DelyH(i,j)
        do k = 1,NGHOST
          U(i,j,Kbeg-k) = U(i,j,Kbeg+k-1)
          V(i,j,Kbeg-k) = V(i,j,Kbeg+k-1)
          W(i,j,Kbeg-k) = 2.0*Wbot-W(i,j,Kbeg+k-1)
          DU(i,j,Kbeg-k) = D(i,j)*U(i,j,Kbeg-k)
          DV(i,j,Kbeg-k) = D(i,j)*V(i,j,Kbeg-k)
          DW(i,j,Kbeg-k) = D(i,j)*W(i,j,Kbeg-k)
        enddo
      elseif(Bc_Z0==2) then  !>no-slip
        Wbot = -DeltH(i,j)
        do k = 1,NGHOST
          U(i,j,Kbeg-k) = -U(i,j,Kbeg+k-1)
          V(i,j,Kbeg-k) = -V(i,j,Kbeg+k-1)
          W(i,j,Kbeg-k) = 2.0*Wbot-W(i,j,Kbeg+k-1)
          DU(i,j,Kbeg-k) = D(i,j)*U(i,j,Kbeg-k)
          DV(i,j,Kbeg-k) = D(i,j)*V(i,j,Kbeg-k)
          DW(i,j,Kbeg-k) = D(i,j)*W(i,j,Kbeg-k)
        enddo
      elseif(Bc_Z0==5) then
        do k = 1,NGHOST
          U(i,j,Kbeg-k) = (1.0-Phi)*U(i,j,Kbeg+k-1)
          V(i,j,Kbeg-k) = (1.0-Phi)*V(i,j,Kbeg+k-1)
          Wbot = -DeltH(i,j)-0.5*(U(i,j,Kbeg)+U(i,j,Kbeg-1))*DelxH(i,j)-  &
                  0.5*(V(i,j,Kbeg)+V(i,j,Kbeg-1))*DelyH(i,j)
          W(i,j,Kbeg-k) = 2.0*Wbot-W(i,j,Kbeg+k-1)
          DU(i,j,Kbeg-k) = D(i,j)*U(i,j,Kbeg-k)
          DV(i,j,Kbeg-k) = D(i,j)*V(i,j,Kbeg-k)
          DW(i,j,Kbeg-k) = D(i,j)*W(i,j,Kbeg-k)
        enddo
      endif

       !>at the surface (no stress)
      Wtop = (Eta(i,j)-Eta0(i,j))/dt+U(i,j,Kend)*DelxEta(i,j)+V(i,j,Kend)*DelyEta(i,j)
      do k = 1,NGHOST
        U(i,j,Kend+k) = U(i,j,Kend-k+1)
        V(i,j,Kend+k) = V(i,j,Kend-k+1)
        W(i,j,Kend+k) = 2.0*Wtop-W(i,j,Kend-k+1)
        DU(i,j,Kend+k) = D(i,j)*U(i,j,Kend+k)
        DV(i,j,Kend+k) = D(i,j)*V(i,j,Kend+k)
        DW(i,j,Kend+k) = D(i,j)*W(i,j,Kend+k)
      enddo
	enddo
  enddo

end subroutine vel_bc
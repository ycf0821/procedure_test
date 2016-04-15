!>Purpose:
!>  This subroutine is used to calculate wave speeds
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

Subroutine wave_speed
  use global, only: SP,Ibeg,Iend,Iend1,Jbeg,Jend,Jend1,Kbeg,Kend, &
                     DxL,DxR,DyL,DyR,UxL,UxR,VyL,VyR, &
                     SxL,SxR,SyL,SyR,GRAV,Mask
  implicit none
  integer  :: i,j,k
  real(kind=SP) :: SQR_PHI_L,SQR_PHI_R,SQR_PHI_S,U_S
     
!>x-faces
  do k = Kbeg,Kend
    do j = Jbeg,Jend
      do i = Ibeg,Iend1
        if(Mask(i-1,j)==1.and.Mask(i,j)==1) then
          SQR_PHI_L = sqrt(GRAV*abs(DxL(i,j)))
          SQR_PHI_R = sqrt(GRAV*abs(DxR(i,j)))
          SQR_PHI_S = 0.5*(SQR_PHI_L+SQR_PHI_R)+0.25*(UxL(i,j,k)-UxR(i,j,k))
          U_S = 0.5*(UxL(i,j,k)+UxR(i,j,k))+SQR_PHI_L-SQR_PHI_R
          SxL(i,j,k) = min(UxL(i,j,k)-SQR_PHI_L,U_S-SQR_PHI_S)
          SxR(i,j,k) = max(UxR(i,j,k)+SQR_PHI_R,U_S+SQR_PHI_S)
        elseif(Mask(i-1,j)==0.and.Mask(i,j)==1) then
          !>left-side dry case
          SQR_PHI_R = sqrt(GRAV*abs(DxR(i,j)))
          SxL(i,j,k) = UxR(i,j,k)-2.0*SQR_PHI_R
          SxR(i,j,k) = UxR(i,j,k)+SQR_PHI_R
        elseif(Mask(i-1,j)==1.and.Mask(i,j)==0) then
          !>right-side dry case
          SQR_PHI_L = sqrt(GRAV*abs(DxL(i,j)))
          SxL(i,j,k) = UxL(i,j,k)-SQR_PHI_L
          SxR(i,j,k) = UxL(i,j,k)+2.0*SQR_PHI_L
        endif
      enddo
    enddo
  enddo

!>y-faces
  do k = Kbeg,Kend
    do j = Jbeg,Jend1
      do i = Ibeg,Iend
        if(Mask(i,j-1)==1.and.Mask(i,j)==1) then
          SQR_PHI_L = sqrt(GRAV*abs(DyL(i,j)))
          SQR_PHI_R = sqrt(GRAV*abs(DyR(i,j)))
          SQR_PHI_S = 0.5*(SQR_PHI_L+SQR_PHI_R)+0.25*(VyL(i,j,k)-VyR(i,j,k))
          U_S = 0.5*(VyL(i,j,k)+VyR(i,j,k))+SQR_PHI_L-SQR_PHI_R
          SyL(i,j,k) = min(VyL(i,j,k)-SQR_PHI_L,U_S-SQR_PHI_S)
          SyR(i,j,k) = max(VyR(i,j,k)+SQR_PHI_R,U_S+SQR_PHI_S)
        elseif(Mask(i,j-1)==0.and.Mask(i,j)==1) then
          !>left-side dry case
          SQR_PHI_R = sqrt(GRAV*abs(DyR(i,j)))
          SyL(i,j,k) = VyR(i,j,k)-2.0*SQR_PHI_R
          SyR(i,j,k) = VyR(i,j,k)+SQR_PHI_R
        elseif(Mask(i,j-1)==1.and.Mask(i,j)==0) then
          !>right-side dry case
          SQR_PHI_L = sqrt(GRAV*abs(DyL(i,j)))
          SyL(i,j,k) = VyL(i,j,k)-SQR_PHI_L
          SyR(i,j,k) = VyL(i,j,k)+2.0*SQR_PHI_L
        endif
      enddo
    enddo
  enddo

end Subroutine wave_speed
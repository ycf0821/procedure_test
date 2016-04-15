!>Purpose:
!>  This subroutine is used to update mask for wetting-drying
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

Subroutine update_mask
  use global, only: Ibeg,Iend,Jbeg,Jend,Eta,Hc,D,MinDep,  &
                     Mask,Mask_Struct,Mask9
  implicit none
  integer :: i,j

!>Mask at ghost cells keeps no change
  do j = Jbeg,Jend
    do i = Ibeg,Iend
	  if(Mask_Struct(i,j)==0) cycle
       
!>flooding (dry->wet)
	  if(Mask(i,j)==0) then
	    if(Mask(i-1,j)==1.and.Eta(i-1,j)>Eta(i,j)) Mask(i,j)=1
		if(Mask(i+1,j)==1.and.Eta(i+1,j)>Eta(i,j)) Mask(i,j)=1
		if(Mask(i,j-1)==1.and.Eta(i,j-1)>Eta(i,j)) Mask(i,j)=1
		if(Mask(i,j+1)==1.and.Eta(i,j+1)>Eta(i,j)) Mask(i,j)=1
	  else
!>drying (wet->dry)
	    if(abs(D(i,j)-MinDep)<=1.e-6) then
		  Mask(i,j) = 0
		  Eta(i,j) = MinDep-Hc(i,j)
	      D(i,j) = Eta(i,j)+Hc(i,j)           
		endif
	  endif
	enddo
  enddo
  Mask = Mask*Mask_Struct
  do j = Jbeg,Jend
    do i = Ibeg,Iend
	  Mask9(i,j) = Mask(i,j)*Mask(i-1,j)*Mask(i+1,j)  &
				   *Mask(i+1,j+1)*Mask(i,j+1)*Mask(i-1,j+1) &
				   *Mask(i+1,j-1)*Mask(i,j-1)*Mask(i-1,j-1)
	enddo
  enddo
end Subroutine update_mask
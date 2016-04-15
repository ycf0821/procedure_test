!>Purpose:
!>  This subroutine is used to save variables
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

Subroutine update_vars
  use global        
  implicit none
  D0 = D
  Eta0 = Eta
  DU0 = DU
  DV0 = DV
  DW0 = DW
  DTke0 = DTke
  DEps0 = DEps
end Subroutine update_vars
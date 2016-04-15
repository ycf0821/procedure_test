!>Purpose:
!>  This subroutine is used to update boundary conditions
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

Subroutine update_wave_bc
  use global
  implicit none

  if(WaveMaker(1:7)=='LEF_SOL') then
    call solitary_wave_left_boundary
  elseif(WaveMaker(1:7)=='LEF_LIN') then
	call linear_wave_left_boundary
  elseif(WaveMaker(1:7)=='LEF_CON') then
	call cnoidal_wave_left_boundary
  elseif(WaveMaker(1:7)=='LEF_STK') then
	call stokes_wave_left_boundary
  elseif(WaveMaker(1:7)=='LEF_SPC') then
!>  not work yet
	call random_wave_left_boundary
  elseif(WaveMaker(1:7)=='LEF_TID') then
	call tidal_wave_left_boundary
  elseif(WaveMaker(1:10)=='TID_FLX_LR') then
	call tidal_flux_left_right
  elseif(WaveMaker(1:10)=='TID_ELE_LR') then
	call tidal_elevation_left_right
  endif

end Subroutine update_wave_bc 
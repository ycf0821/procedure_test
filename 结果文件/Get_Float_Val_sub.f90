!>Purpose:
!>  This subroutine is used to return float 
!>
!>Interface:
!>
!>Input:
!>  filename:
!>  argname:
!>  echo:
!>  start:
!>  finish:
!>
!>
!>Output:
!>  fval:
!>  line:
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

Subroutine Get_Float_Val(fval,filename,argname,line,echo,start,finish)  
  implicit none
  real(kind=SP),         intent(out) :: fval
  character(len=*), intent(in ) :: filename
  character(len=*), intent(in ) :: argname
  integer,          intent(out) :: line
  logical, optional,intent(in)  :: echo
  integer, optional,intent(in)  :: start
  integer, optional,intent(in)  :: finish 

  character(len=80) :: argval
  character(len=80) :: argtype
  integer           :: ierr


!>make sure file exists
  if(.not.check_exist(filename))then
    call error('Get_Val','halt','error reading: '//trim(argname), &
              'file: '//trim(filename)//' does not exist') 
  endif

!>check start and finish lines
  if(present(start) .and. present(finish))then
    if(finish < start)then
    call error('Get_Val','halt','finish must be greater than or equal to start')
    endif
  endif

!>parse file for argument line 
  call Extract_Val_String(filename,argname,argval,ierr,line,start,finish)
  if(ierr == 1)then
    call error('Get_Val','halt','error reading variable: '//trim(argname), &
              'does not exist in file: '//trim(filename)) 
  endif

!>check argument type
  call Check_Arg_Type(argval,argtype)
  if(trim(argtype) /= 'float')then
    call error('Get_Val','halt','error reading variable: '//trim(argname), &
              'in file: '//trim(filename), &        
              'should be float but is: '//trim(argtype))
  endif

!>set return value
  read(argval,*)fval  

!>echo to screen 
  if(present(echo))then
    if(echo)then 
      write(*,'(A20,F10.4)')trim(argname)//': ',fval
    endif
  endif
    
  
end Subroutine Get_Float_Val
!>Purpose:
!>  This subroutine is used to return integer 
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
!>  ival:
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

Subroutine Get_Integer_Val(ival,filename,argname,line,echo,start,finish)  
  implicit none
  integer,          intent(out) :: ival 
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
    write(*,*) trim(argname),'is not specified.' 
  endif

!>check argument type
  call Check_Arg_Type(argval,argtype)
  if(trim(argtype) /= 'integer')then
    call error('Get_Val','halt','error reading variable: '//trim(argname), &
               'in file: '//trim(filename), &        
               'should be integer but is: '//trim(argtype))
  endif

!>set return value
  read(argval,*)ival   

!>echo to screen
  if(present(echo))then
    if(echo)then 
      write(*,'(A20,I10)')trim(argname)//': ',ival
    endif
  endif
  
end Subroutine Get_Integer_Val
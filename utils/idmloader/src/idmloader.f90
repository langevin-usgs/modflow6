program idmloader
  use KindModule
  use ConstantsModule, only: LINELENGTH, LENHUGELINE
  use VersionModule, only: VERSION
  use SimVariablesModule, only: iout, errmsg
  use SimModule, only: store_error
  use GenericUtilitiesModule, only: sim_message, write_centered
  use InputOutputModule,  only: openfile
  
  implicit none
  
  character(len=10), parameter :: mfvnam=' Version 6'
  character(len=LINELENGTH) :: line
  character(len=LENHUGELINE) :: flst
  integer(I4B) :: iunit_lst = 20
  
  ! -- Write title to screen
  call write_centered('IDMLOADER'//mfvnam, 80)
  call write_centered('U.S. GEOLOGICAL SURVEY', 80)
  call write_centered('VERSION '//VERSION, 80)
  !  
  ! -- Open list file and write title
  iout = iunit_lst
  flst = 'out.lst'
  call openfile(iunit_lst, 0, flst, 'LIST', filstat_opt='REPLACE')
  call write_centered('IDMLOADER'//mfvnam, 80, iunit=iout)
  call write_centered('U.S. GEOLOGICAL SURVEY', 80, iunit=iout)
  call write_centered('VERSION '//VERSION, 80, iunit=iout)
  !
  ! -- use load_from_file to load input file into memory manager
  call test_load_from_file()
  !
  ! -- load data into memory
  call finalize_data()
  !
  ! -- close output files
  write(iunit_lst, '(/, a)') 'Normal Termination'
  close(iunit_lst)
  write(line,'(a)') 'Normal Termination'
  call sim_message(line, skipbefore=1)
  !
  ! -- end of program
  end program idmloader
  
  ! todo: modify program to load all data in a gwf name file?
  ! todo: a short term goal should be to load a minimum number
  !       of files necessary to run a model.  This would include:
  !       TDIS, IMS, DIS, NPF, IC, CHD
  subroutine test_load_from_file()
    use LoadFromFileModule, only: input_from_file
    implicit none
    call input_from_file('TDIS6', 'mf6.tdis', 'SIM', 'TDIS', 'SIM', 'TDIS')
    call input_from_file('DIS6', 'mf6.dis', 'GWF', 'DIS', 'MYMODEL', 'DIS')
    call input_from_file('NPF6', 'mf6.npf', 'GWF', 'NPF', 'MYMODEL', 'NPF')
  end subroutine test_load_from_file
  
  subroutine finalize_data()
    use SimVariablesModule, only: iout, errmsg
    use MemoryManagerModule,  only: mem_set_print_option, mem_write_usage, mem_da
    implicit none
    call mem_set_print_option(iout, 'ALL', errmsg)
    call mem_write_usage(iout)
    call mem_da()
  end subroutine finalize_data
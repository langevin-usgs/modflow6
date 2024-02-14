!> @brief Stream Network Flow (SWF) Muskingum Routing (MMR) Module
!!
!! This module solves one-dimensional flow routing using a linear Muskingum
!! approach.
!!
!<
module SwfMmrModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LENMEMPATH, LENVARNAME, LINELENGTH, &
                             DZERO, DHALF, DONE, DTWO, DTHREE, &
                             LENBUDTXT
  use MemoryHelperModule, only: create_mem_path
  use MemoryManagerModule, only: mem_allocate, mem_setptr, get_isize
  use SimVariablesModule, only: errmsg, warnmsg
  use SimModule, only: count_errors, store_error, store_error_unit, &
                       store_error_filename
  use NumericalPackageModule, only: NumericalPackageType
  use BaseDisModule, only: DisBaseType
  use SwfDislModule, only: SwfDislType
  use ObsModule, only: ObsType, obs_cr
  use ObsModule, only: DefaultObsIdProcessor
  use ObserveModule, only: ObserveType

  implicit none
  private
  public :: SwfMmrType, mmr_cr

  character(len=LENBUDTXT), dimension(2) :: budtxt = & !< text labels for budget terms
    &['         STORAGE', '     EXT-OUTFLOW']

  type, extends(NumericalPackageType) :: SwfMmrType

    ! -- user-provided input
    integer(I4B), dimension(:), pointer, contiguous :: iseg_order => null() !< routing calculation order
    real(DP), dimension(:), pointer, contiguous :: qoutflow0 => null() !< initial outflow for each reach
    real(DP), dimension(:), pointer, contiguous :: k_coef => null() !< manning K coefficient
    real(DP), dimension(:), pointer, contiguous :: x_coef => null() !< routing weighting factor

    ! -- input arguments to calc_muskingum_mann routine
    real(DP), dimension(:), pointer, contiguous :: qinflow_old => null() !< inflow to each reach for last time step
    real(DP), dimension(:), pointer, contiguous :: qinflow => null() !< inflow to each reach for current time step
    real(DP), dimension(:), pointer, contiguous :: qoutflow_old => null() !< outflow from each reach for last time step
    real(DP), dimension(:), pointer, contiguous :: qoutflow => null() !< outflow from each reach for current time step
    real(DP), dimension(:), pointer, contiguous :: c0 => null() !< Muskingum c0 variable
    real(DP), dimension(:), pointer, contiguous :: c1 => null() !< Muskingum c1 variable
    real(DP), dimension(:), pointer, contiguous :: c2 => null() !< Muskingum c2 variable

    ! -- budget vectors
    real(DP), dimension(:), pointer, contiguous :: qextoutflow => null() !< flows leaving model (for toreach = 0)
    real(DP), dimension(:), pointer, contiguous :: qsto => null() !< storage rates

    ! -- pointer to concrete disl subclass of DisBaseType
    type(SwfDislType), pointer :: disl

    ! -- observation data
    integer(I4B), pointer :: inobspkg => null() !< unit number for obs package
    type(ObsType), pointer :: obs => null() !< observation package

  contains

    procedure :: allocate_scalars
    procedure :: allocate_arrays
    procedure :: mmr_load
    procedure :: source_options
    procedure :: log_options
    procedure :: source_griddata
    procedure :: log_griddata
    procedure :: mmr_ar
    procedure :: mmr_rp
    procedure :: mmr_ad
    procedure :: mmr_init_data
    procedure :: mmr_solve
    procedure :: mmr_cq
    procedure :: mmr_bd
    procedure :: mmr_save_model_flows
    procedure :: mmr_print_model_flows
    procedure :: mmr_da
    procedure :: mmr_df_obs
    procedure :: mmr_rp_obs
    procedure :: mmr_bd_obs

  end type SwfMmrType

contains

  !> @brief create package
  !<
  subroutine mmr_cr(mmrobj, name_model, input_mempath, inunit, iout, dis)
    ! -- modules
    use MemoryManagerExtModule, only: mem_set_value
    ! -- dummy
    type(SwfMmrType), pointer :: mmrobj
    character(len=*), intent(in) :: name_model
    character(len=*), intent(in) :: input_mempath
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    class(DisBaseType), pointer, intent(inout) :: dis !< the pointer to the discretization
    ! -- locals
    logical(LGP) :: found_fname
    ! -- formats
    character(len=*), parameter :: fmtheader = &
      "(1x, /1x, 'MMR -- MUSKINGUM MANNINGS ROUTING PACKAGE, VERSION 1, 5/22/2023', &
       &' INPUT READ FROM MEMPATH: ', A, /)"
    !
    ! -- Create the object
    allocate (mmrobj)

    ! -- create name and memory path
    call mmrobj%set_names(1, name_model, 'MMR', 'MMR')

    ! -- Allocate scalars
    call mmrobj%allocate_scalars()

    ! -- Set variables
    mmrobj%input_mempath = input_mempath
    mmrobj%inunit = inunit
    mmrobj%iout = iout

    ! -- set name of input file
    call mem_set_value(mmrobj%input_fname, 'INPUT_FNAME', mmrobj%input_mempath, &
                       found_fname)

    ! -- store pointer to disl
    !    Not normally good practice, but since SWF only works with DISL
    !    may be okay
    mmrobj%dis => dis
    select type (dis)
    type is (SwfDislType)
      mmrobj%disl => dis
    end select

    ! -- create obs package
    call obs_cr(mmrobj%obs, mmrobj%inobspkg)

    ! -- check if mmr is enabled
    if (inunit > 0) then

      ! -- Print a message identifying the package.
      write (iout, fmtheader) input_mempath

      ! -- allocate arrays
      call mmrobj%allocate_arrays()

      ! -- load mmr
      call mmrobj%mmr_load()

    end if

    ! -- Return
    return
  end subroutine mmr_cr

  !> @ brief Allocate scalars
  !!
  !! Allocate and initialize scalars for the package. The base model
  !! allocate scalars method is also called.
  !!
  !<
  subroutine allocate_scalars(this)
    ! -- modules
    ! -- dummy
    class(SwfMmrtype) :: this
    !
    ! -- allocate scalars in NumericalPackageType
    call this%NumericalPackageType%allocate_scalars()
    !
    ! -- Allocate scalars
    call mem_allocate(this%inobspkg, 'INOBSPKG', this%memoryPath)

    this%inobspkg = 0

    return
  end subroutine allocate_scalars

  !> @brief allocate memory for arrays
  !<
  subroutine allocate_arrays(this)
    ! -- dummy
    class(SwfMmrType) :: this
    ! -- locals
    integer(I4B) :: n
    !
    ! -- user-provided input
    call mem_allocate(this%iseg_order, this%dis%nodes, &
                      'ISEG_ORDER', this%memoryPath)
    call mem_allocate(this%qoutflow0, this%dis%nodes, &
                      'QOUTFLOW0', this%memoryPath)
    call mem_allocate(this%k_coef, this%dis%nodes, &
                      'K_COEF', this%memoryPath)
    call mem_allocate(this%x_coef, this%dis%nodes, &
                      'X_COEF', this%memoryPath)

    ! -- input arguments to calc_muskingum_mann routine
    call mem_allocate(this%qinflow_old, this%dis%nodes, &
                      'QINFLOW_OLD', this%memoryPath)
    call mem_allocate(this%qinflow, this%dis%nodes, &
                      'QINFLOW', this%memoryPath)
    call mem_allocate(this%qoutflow_old, this%dis%nodes, &
                      'QOUTFLOW_OLD', this%memoryPath)
    call mem_allocate(this%qoutflow, this%dis%nodes, &
                      'QOUTFLOW', this%memoryPath)
    call mem_allocate(this%c0, this%dis%nodes, &
                      'C0', this%memoryPath)
    call mem_allocate(this%c1, this%dis%nodes, &
                      'C1', this%memoryPath)
    call mem_allocate(this%c2, this%dis%nodes, &
                      'C2', this%memoryPath)

    ! -- budgeting variables
    call mem_allocate(this%qextoutflow, this%dis%nodes, &
                      'QEXTOUTFLOW', this%memoryPath)
    call mem_allocate(this%qsto, this%dis%nodes, &
                      'QSTO', this%memoryPath)

    do n = 1, this%dis%nodes

      this%iseg_order(n) = 0
      this%qoutflow0(n) = DZERO
      this%k_coef(n) = DZERO
      this%x_coef(n) = DZERO

      this%qinflow_old(n) = DZERO
      this%qinflow(n) = DZERO
      this%qoutflow_old(n) = DZERO
      this%qoutflow(n) = DZERO
      this%c0(n) = DZERO
      this%c1(n) = DZERO
      this%c2(n) = DZERO

      this%qextoutflow(n) = DZERO
      this%qsto(n) = DZERO

    end do

    ! -- Return
    return
  end subroutine allocate_arrays

  !> @brief load data from IDM to package
  !<
  subroutine mmr_load(this)
    ! -- dummy
    class(SwfMmrType) :: this
    ! -- locals
    !
    ! -- source input data
    call this%source_options()
    call this%source_griddata()
    !
    ! -- Return
    return
  end subroutine mmr_load

  !> @brief Copy options from IDM into package
  !<
  subroutine source_options(this)
    ! -- modules
    use KindModule, only: LGP
    use InputOutputModule, only: getunit, openfile
    use MemoryManagerExtModule, only: mem_set_value
    use CharacterStringModule, only: CharacterStringType
    use SwfMmrInputModule, only: SwfMmrParamFoundType
    ! -- dummy
    class(SwfMmrType) :: this
    ! -- locals
    integer(I4B) :: isize
    type(SwfMmrParamFoundType) :: found
    type(CharacterStringType), dimension(:), pointer, &
      contiguous :: obs6_fnames
    !
    ! -- update defaults with idm sourced values
    call mem_set_value(this%iprflow, 'IPRFLOW', this%input_mempath, found%iprflow)
    call mem_set_value(this%ipakcb, 'IPAKCB', this%input_mempath, found%ipakcb)
    !
    ! -- save flows option active
    if (found%ipakcb) this%ipakcb = -1
    !
    ! -- check for obs6_filename
    call get_isize('OBS6_FILENAME', this%input_mempath, isize)
    if (isize > 0) then
      !
      if (isize /= 1) then
        errmsg = 'Multiple OBS6 keywords detected in OPTIONS block.'// &
                 ' Only one OBS6 entry allowed.'
        call store_error(errmsg)
        call store_error_filename(this%input_fname)
      end if
      !
      call mem_setptr(obs6_fnames, 'OBS6_FILENAME', this%input_mempath)
      !
      this%obs%inputFilename = obs6_fnames(1)
      this%obs%active = .true.
      this%inobspkg = GetUnit()
      this%obs%inUnitObs = this%inobspkg
      call openfile(this%inobspkg, this%iout, this%obs%inputFilename, 'OBS')
      call this%obs%obs_df(this%iout, this%packName, this%filtyp, this%dis)
      call this%mmr_df_obs()
    end if
    !
    ! -- log values to list file
    if (this%iout > 0) then
      call this%log_options(found)
    end if
    !
    ! -- Return
    return
  end subroutine source_options

  !> @brief Write user options to list file
  !<
  subroutine log_options(this, found)
    use SwfMmrInputModule, only: SwfMmrParamFoundType
    class(SwfMmrType) :: this
    type(SwfMmrParamFoundType), intent(in) :: found

    write (this%iout, '(1x,a)') 'Setting MMR Options'

    if (found%iprflow) then
      write (this%iout, '(4x,a)') 'Cell-by-cell flow information will be printed &
                                  &to listing file whenever ICBCFL is not zero.'
    end if

    if (found%ipakcb) then
      write (this%iout, '(4x,a)') 'Cell-by-cell flow information will be printed &
                                  &to listing file whenever ICBCFL is not zero.'
    end if

    write (this%iout, '(1x,a,/)') 'End Setting MMR Options'

  end subroutine log_options

  !> @brief copy griddata from IDM to package
  !<
  subroutine source_griddata(this)
    ! -- modules
    use SimModule, only: count_errors, store_error
    use MemoryHelperModule, only: create_mem_path
    use MemoryManagerModule, only: mem_reallocate
    use MemoryManagerExtModule, only: mem_set_value
    use SimVariablesModule, only: idm_context
    use SwfMmrInputModule, only: SwfMmrParamFoundType
    ! -- dummy
    class(SwfMmrType) :: this
    ! -- locals
    character(len=LENMEMPATH) :: idmMemoryPath
    type(SwfMmrParamFoundType) :: found
    integer(I4B), dimension(:), pointer, contiguous :: map
    ! -- formats
    !
    ! -- set memory path
    idmMemoryPath = create_mem_path(this%name_model, 'MMR', idm_context)
    !
    ! -- set map to convert user input data into reduced data
    map => null()
    if (this%dis%nodes < this%dis%nodesuser) map => this%dis%nodeuser
    !
    ! -- update defaults with idm sourced values
    call mem_set_value(this%iseg_order, 'ISEG_ORDER', &
                       idmMemoryPath, map, found%iseg_order)
    call mem_set_value(this%qoutflow0, 'QOUTFLOW0', &
                       idmMemoryPath, map, found%qoutflow0)
    call mem_set_value(this%k_coef, 'K_COEF', &
                       idmMemoryPath, map, found%k_coef)
    call mem_set_value(this%x_coef, 'X_COEF', &
                       idmMemoryPath, map, found%x_coef)
    !
    ! -- ensure ISEG_ORDER was found
    if (.not. found%iseg_order) then
      write (errmsg, '(a)') 'Error in GRIDDATA block: ISEG_ORDER not found.'
      call store_error(errmsg)
    end if
    !
    ! -- ensure QOUTFLOW0 was found
    if (.not. found%qoutflow0) then
      write (errmsg, '(a)') 'Error in GRIDDATA block: QOUTFLOW0 not found.'
      call store_error(errmsg)
    end if
    !
    ! -- ensure MANN_N was found
    if (.not. found%k_coef) then
      write (errmsg, '(a)') 'Error in GRIDDATA block: K_COEF not found.'
      call store_error(errmsg)
    end if
    !
    ! -- ensure X_COEF was found
    if (.not. found%x_coef) then
      write (errmsg, '(a)') 'Error in GRIDDATA block: X_COEF not found.'
      call store_error(errmsg)
    end if
    !
    ! -- log griddata
    if (this%iout > 0) then
      call this%log_griddata(found)
    end if
    !
    ! -- Return
    return
  end subroutine source_griddata

  !> @brief log griddata to list file
  !<
  subroutine log_griddata(this, found)
    use SwfMmrInputModule, only: SwfMmrParamFoundType
    class(SwfMmrType) :: this
    type(SwfMmrParamFoundType), intent(in) :: found

    write (this%iout, '(1x,a)') 'Setting MMR Griddata'

    if (found%iseg_order) then
      write (this%iout, '(4x,a)') 'ISEG_ORDER set from input file'
    end if

    if (found%qoutflow0) then
      write (this%iout, '(4x,a)') 'QOUTFLOW0 set from input file'
    end if

    if (found%k_coef) then
      write (this%iout, '(4x,a)') 'K_COEF set from input file'
    end if

    if (found%x_coef) then
      write (this%iout, '(4x,a)') 'X_COEF set from input file'
    end if

    write (this%iout, '(1x,a,/)') 'End Setting MMR Griddata'

  end subroutine log_griddata

  !> @brief allocate memory
  !<
  subroutine mmr_ar(this)
    ! -- modules
    ! -- dummy
    class(SwfMmrType) :: this !< this instance
    !

    ! - observation data
    call this%obs%obs_ar()

    ! -- initialize routing variables
    call this%mmr_init_data()

    return
  end subroutine mmr_ar

  !> @brief allocate memory
  !<
  subroutine mmr_rp(this)
    ! -- modules
    ! -- dummy
    class(SwfMmrType) :: this !< this instance
    !
    ! -- read observations
    call this%mmr_rp_obs()
    return
  end subroutine mmr_rp

  subroutine mmr_ad(this, irestore)
    !
    class(SwfMmrType) :: this
    integer(I4B), intent(in) :: irestore
    integer(I4B) :: n
    !
    ! Advance forward or backward depending on irestore
    if (irestore == 0) then
      do n = 1, this%disl%nodes
        this%qinflow_old(n) = this%qinflow(n)
        this%qoutflow_old(n) = this%qoutflow(n)
      end do
    else
      do n = 1, this%disl%nodes
        this%qinflow(n) = this%qinflow_old(n)
        this%qoutflow(n) = this%qoutflow_old(n)
      end do
    end if

    ! -- Push simulated values to preceding time/subtime step
    call this%obs%obs_ad()

    !
    ! -- Return
    return
  end subroutine mmr_ad

  !> @brief solve
  !<
  subroutine mmr_solve(this, rhs)
    ! -- modules
    use TdisModule, only: delt
    ! -- dummy
    class(SwfMmrType) :: this !< this instance
    real(DP), dimension(:), intent(in) :: rhs !< right-hand-side vector of boundary package inflows
    ! -- local

    call calc_muskingum_coefficients(delt, this%k_coef, this%x_coef, this%c0, &
                                     this%c1, this%c2)

    call calc_muskingum(this%disl%toreach, this%iseg_order, this%qinflow_old, &
                        this%qoutflow_old, this%qinflow, this%qoutflow, &
                        this%c0, this%c1, this%c2, -rhs)

    ! -- return
    return
  end subroutine mmr_solve

  subroutine mmr_cq(this, flowja)
    ! -- dummy
    class(SwfMmrType) :: this
    real(DP), intent(inout), dimension(:) :: flowja
    ! -- local
    integer(I4B) :: n, ipos, m
    real(DP) :: qnm, q
    !
    ! -- Transfer seg_outflow into flowja
    do n = 1, this%dis%nodes
      m = this%disl%toreach(n)
      ! TODO: may be a faster way than lookup
      if (m > 0) then
        do ipos = this%dis%con%ia(n) + 1, this%dis%con%ia(n + 1) - 1
          if (this%dis%con%ja(ipos) == m) exit
        end do
        qnm = this%qoutflow(n)
        flowja(ipos) = -qnm
        flowja(this%dis%con%isym(ipos)) = qnm
      end if
    end do

    ! Transfer any flows leaving toreach 0 into qextoutflow
    do n = 1, this%dis%nodes
      q = DZERO
      if (this%disl%toreach(n) == 0) then
        q = -this%qoutflow(n)
      end if
      this%qextoutflow(n) = q
      !
      ! -- add to diagonal of flowja
      ipos = this%dis%con%ia(n)
      flowja(ipos) = flowja(ipos) + q
    end do

    ! Transfer storage terms into qsto
    do n = 1, this%dis%nodes
      q = this%qoutflow(n) - this%qinflow(n)
      this%qsto(n) = q
      !
      ! -- add to diagonal
      ipos = this%dis%con%ia(n)
      flowja(ipos) = flowja(ipos) + q
    end do

    !
    ! -- Return
    return
  end subroutine mmr_cq

  !> @ brief Model budget calculation for package
  !!
  !!  Budget calculation for the MMR package components. Components include
  !!  external outflow
  !!
  !<
  subroutine mmr_bd(this, isuppress_output, model_budget)
    ! -- modules
    use TdisModule, only: delt
    use BudgetModule, only: BudgetType, rate_accumulator
    ! -- dummy variables
    class(SwfMmrType) :: this !< SwfMmrType object
    integer(I4B), intent(in) :: isuppress_output !< flag to suppress model output
    type(BudgetType), intent(inout) :: model_budget !< model budget object
    ! -- local variables
    real(DP) :: rin
    real(DP) :: rout
    !
    ! -- Add external outflow rates to model budget
    call rate_accumulator(this%qextoutflow, rin, rout)
    call model_budget%addentry(rin, rout, delt, '             MMR', &
                               isuppress_output, '     EXT-OUTFLOW')
    !
    ! -- Add storage rates to model budget
    call rate_accumulator(this%qsto, rin, rout)
    call model_budget%addentry(rin, rout, delt, '             MMR', &
                               isuppress_output, '         STORAGE')
    !
    ! -- return
    return
  end subroutine mmr_bd

  !> @ brief save flows for package
  !<
  subroutine mmr_save_model_flows(this, flowja, icbcfl, icbcun)
    ! -- dummy
    class(SwfMmrType) :: this
    real(DP), dimension(:), intent(in) :: flowja
    integer(I4B), intent(in) :: icbcfl
    integer(I4B), intent(in) :: icbcun
    ! -- local
    real(DP) :: dinact
    integer(I4B) :: ibinun
    integer(I4B) :: iprint, nvaluesp, nwidthp
    character(len=1) :: cdatafmp = ' ', editdesc = ' '
    ! -- formats
    !
    ! -- Set unit number for binary output
    iprint = 0
    dinact = DZERO
    if (this%ipakcb < 0) then
      ibinun = icbcun
    elseif (this%ipakcb == 0) then
      ibinun = 0
    else
      ibinun = this%ipakcb
    end if
    if (icbcfl == 0) ibinun = 0
    !
    ! -- Write the face flows if requested
    if (ibinun /= 0) then
      !
      ! -- flowja
      call this%dis%record_connection_array(flowja, ibinun, this%iout)
      !
      !
      ! -- storage
      call this%dis%record_array(this%qsto, this%iout, iprint, -ibinun, &
                                 budtxt(1), cdatafmp, nvaluesp, &
                                 nwidthp, editdesc, dinact)

      ! -- external outflow
      ! TODO: should this be written as a list instead?
      call this%dis%record_array(this%qextoutflow, this%iout, iprint, -ibinun, &
                                 budtxt(2), cdatafmp, nvaluesp, &
                                 nwidthp, editdesc, dinact)

    end if
    !
    ! -- Return
    return
  end subroutine mmr_save_model_flows

  !> @ brief print flows for package
  !<
  subroutine mmr_print_model_flows(this, ibudfl, flowja)
    ! -- modules
    use TdisModule, only: kper, kstp
    use ConstantsModule, only: LENBIGLINE
    ! -- dummy
    class(SwfMmrType) :: this
    integer(I4B), intent(in) :: ibudfl
    real(DP), intent(inout), dimension(:) :: flowja
    ! -- local
    character(len=LENBIGLINE) :: line
    character(len=30) :: tempstr
    integer(I4B) :: n, ipos, m
    real(DP) :: qnm
    ! -- formats
    character(len=*), parameter :: fmtiprflow = &
      &"(/,4x,'CALCULATED INTERCELL FLOW FOR PERIOD ', i0, ' STEP ', i0)"
! ------------------------------------------------------------------------------
    !
    ! -- Write flowja to list file if requested
    if (ibudfl /= 0 .and. this%iprflow > 0) then
      write (this%iout, fmtiprflow) kper, kstp
      do n = 1, this%dis%nodes
        line = ''
        call this%dis%noder_to_string(n, tempstr)
        line = trim(tempstr)//':'
        do ipos = this%dis%con%ia(n) + 1, this%dis%con%ia(n + 1) - 1
          m = this%dis%con%ja(ipos)
          call this%dis%noder_to_string(m, tempstr)
          line = trim(line)//' '//trim(tempstr)
          qnm = flowja(ipos)
          write (tempstr, '(1pg15.6)') qnm
          line = trim(line)//' '//trim(adjustl(tempstr))
        end do
        write (this%iout, '(a)') trim(line)
      end do
    end if
    !
    ! -- Return
    return
  end subroutine mmr_print_model_flows

  !> @brief deallocate memory
  !<
  subroutine mmr_da(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    use MemoryManagerExtModule, only: memorylist_remove
    use SimVariablesModule, only: idm_context
    ! -- dummy
    class(SwfMmrType) :: this
    !
    ! -- Deallocate input memory
    call memorylist_remove(this%name_model, 'MMR', idm_context)
    !
    ! -- Scalars
    !
    ! -- Deallocate arrays
    call mem_deallocate(this%iseg_order)
    call mem_deallocate(this%qoutflow0)
    call mem_deallocate(this%k_coef)
    call mem_deallocate(this%x_coef)

    ! -- input arguments to calc_muskingum_mann routine
    call mem_deallocate(this%qinflow_old)
    call mem_deallocate(this%qinflow)
    call mem_deallocate(this%qoutflow_old)
    call mem_deallocate(this%qoutflow)
    call mem_deallocate(this%c0)
    call mem_deallocate(this%c1)
    call mem_deallocate(this%c2)

    ! -- budget variables
    call mem_deallocate(this%qextoutflow)
    call mem_deallocate(this%qsto)

    ! -- obs package
    call mem_deallocate(this%inobspkg)
    call this%obs%obs_da()
    deallocate (this%obs)
    nullify (this%obs)

    ! -- deallocate parent
    call this%NumericalPackageType%da()
    !
    ! -- Return
    return
  end subroutine mmr_da

  !> @brief initialize mmr data
  !!
  !! This routine is only called once at the beginning of
  !! the simulation from mmr_ar()
  !!
  !<
  subroutine mmr_init_data(this)
    ! -- modules
    ! -- dummy
    class(SwfMmrType) :: this !< this instance
    ! -- local
    integer(I4B) :: n
    integer(I4B) :: ito_seg
    integer(I4B), dimension(:), allocatable :: nupreaches

    ! -- Set qinflow and qoutflow from qoutflow0
    !    These will be copied into qinflow_old and qoutflow_old
    !    as part of the first advance
    do n = 1, this%dis%nodes
      this%qoutflow(n) = this%qoutflow0(n)
      ito_seg = this%disl%toreach(n)
      if (ito_seg > 0) then
        this%qinflow(ito_seg) = &
          this%qinflow(ito_seg) + this%qoutflow0(n)
      end if
    end do

    ! -- Calculate the number of upstream reaches connected to each reach,
    !    which is then used in the subsequent section to initialize the
    !    flow conditions
    allocate (nupreaches(this%dis%nodes))
    do n = 1, this%dis%nodes
      nupreaches(n) = 0
    end do
    do n = 1, this%dis%nodes
      ito_seg = this%disl%toreach(n)
      if (ito_seg > 0) then
        nupreaches(ito_seg) = nupreaches(ito_seg) + 1
      end if
    end do

    ! -- For any cells that do not have inflow via upstream reaches,
    !    the qinflow term should be set to the initial outflow
    !    for the reach, qoutflow0
    do n = 1, this%dis%nodes
      if (nupreaches(n) == 0) then
        this%qinflow(n) = this%qoutflow0(n)
      end if
    end do

    ! -- return
    return
  end subroutine mmr_init_data

  !> @brief calculate muskingum c0, c1, c2 coefficients
  !<
  subroutine calc_muskingum_coefficients(delt, k_coef, x_coef, c0, c1, c2)
    ! -- modules
    ! -- dummy
    real(DP), intent(in) :: delt !< Length of time step
    real(DP), dimension(:), intent(in) :: k_coef !< Muskingum K coefficient
    real(DP), dimension(:), intent(in) :: x_coef !< Muskingum K coefficient
    real(DP), dimension(:), intent(inout) :: c0 !< Muskingum coefficient on inflow_new
    real(DP), dimension(:), intent(inout) :: c1 !< Muskingum coefficient on inflow_old
    real(DP), dimension(:), intent(inout) :: c2 !< Muskingum coefficient on outflow_old
    ! -- local
    integer(I4B) :: n
    real(DP) :: dtoverk
    real(DP) :: twox
    real(DP) :: two_oneminusx
    real(DP) :: denom

    ! -- Calculate muskingum C coefficients, using Ponce equations
    do n = 1, size(k_coef)
      dtoverk = delt / k_coef(n)
      twox = DTWO * x_coef(n)
      two_oneminusx = DTWO * (DONE - x_coef(n))
      denom = two_oneminusx + dtoverk
      c0(n) = (dtoverk - twox) / denom
      c1(n) = (dtoverk + twox) / denom
      c2(n) = (two_oneminusx - dtoverk) / denom
    end do

    ! -- Clean up negative C coefficients
    do n = 1, size(k_coef)

      ! Short travel time
      if (c2(n) < DZERO) then
        c1(n) = c1(n) + c2(n)
        c2(n) = DZERO
      end if

      ! Long travel time
      if (c0(n) < DZERO) then
        c1(n) = c1(n) + c0(n)
        c0(n) = DZERO
      end if

    end do

    ! -- return
    return
  end subroutine calc_muskingum_coefficients

  subroutine calc_muskingum(itoreach, iseg_order, qinflow_old, qoutflow_old, &
                            qinflow, qoutflow, c0, c1, c2, qsource)
    ! -- dummy
    integer(I4B), dimension(:), intent(in) :: itoreach
    integer(I4B), dimension(:), intent(in) :: iseg_order
    real(DP), dimension(:), intent(in) :: qinflow_old
    real(DP), dimension(:), intent(in) :: qoutflow_old
    real(DP), dimension(:), intent(inout) :: qinflow
    real(DP), dimension(:), intent(inout) :: qoutflow
    real(DP), dimension(:), intent(in) :: c0
    real(DP), dimension(:), intent(in) :: c1
    real(DP), dimension(:), intent(in) :: c2
    real(DP), dimension(:), intent(in) :: qsource
    ! -- dummy
    integer(I4B) :: n
    integer(I4B) :: j
    integer(I4B) :: i
    real(DP) :: qout
    real(DP) :: qin

    ! -- Initialize qinflow with any sources, such as lateral inflow
    do n = 1, size(itoreach)
      j = iseg_order(n)
      qinflow(j) = qsource(j)
    end do

    ! -- Use Muskingum method to calculate outflows and accumulate
    !    outflows in downstream reaches
    do n = 1, size(itoreach)
      j = iseg_order(n)
      qin = qinflow(j) !+ qsource(j)
      qout = c0(j) * qin + c1(j) * qinflow_old(j) + c2(j) * qoutflow_old(j)
      qoutflow(j) = qout
      i = itoreach(j)
      if (i > 0) then
        qinflow(i) = qinflow(i) + qout
      end if
    end do

  end subroutine calc_muskingum

  !> @brief Define the observation types available in the package
  !!
  !! Method to define the observation types available in the package.
  !!
  !<
  subroutine mmr_df_obs(this)
    ! -- dummy variables
    class(SwfMmrType) :: this !< SwfMmrType object
    ! -- local variables
    integer(I4B) :: indx
    !
    ! -- Store obs type and assign procedure pointer
    !    for ext-outflow observation type.
    call this%obs%StoreObsType('ext-outflow', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => mmrobsidprocessor
    !
    ! -- return
    return
  end subroutine mmr_df_obs

  subroutine mmrobsidprocessor(obsrv, dis, inunitobs, iout)
    ! -- dummy
    type(ObserveType), intent(inout) :: obsrv
    class(DisBaseType), intent(in) :: dis
    integer(I4B), intent(in) :: inunitobs
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B) :: n
    character(len=LINELENGTH) :: strng
    !
    ! -- Initialize variables
    strng = obsrv%IDstring
    read (strng, *) n
    !
    if (n > 0) then
      obsrv%NodeNumber = n
    else
      errmsg = 'Error reading data from ID string'
      call store_error(errmsg)
      call store_error_unit(inunitobs)
    end if
    !
    return
  end subroutine mmrobsidprocessor

  !> @brief Save observations for the package
  !!
  !! Method to save simulated values for the package.
  !!
  !<
  subroutine mmr_bd_obs(this)
    ! -- dummy variables
    class(SwfMmrType) :: this !< SwfMmrType object
    ! -- local variables
    integer(I4B) :: i
    integer(I4B) :: j
    integer(I4B) :: n
    real(DP) :: v
    character(len=100) :: msg
    type(ObserveType), pointer :: obsrv => null()
    !
    ! Write simulated values for all observations
    if (this%obs%npakobs > 0) then
      call this%obs%obs_bd_clear()
      do i = 1, this%obs%npakobs
        obsrv => this%obs%pakobs(i)%obsrv
        do j = 1, obsrv%indxbnds_count
          n = obsrv%indxbnds(j)
          v = DZERO
          select case (obsrv%ObsTypeId)
          case ('EXT-OUTFLOW')
            v = this%qextoutflow(n)
          case default
            msg = 'Unrecognized observation type: '//trim(obsrv%ObsTypeId)
            call store_error(msg)
          end select
          call this%obs%SaveOneSimval(obsrv, v)
        end do
      end do
      !
      ! -- write summary of package error messages
      if (count_errors() > 0) then
        call this%parser%StoreErrorUnit()
      end if
    end if
    !
    ! -- return
    return
  end subroutine mmr_bd_obs

  !> @brief Read and prepare observations for a package
  !!
  !! Method to read and prepare observations for a package.
  !!
  !<
  subroutine mmr_rp_obs(this)
    ! -- modules
    use TdisModule, only: kper
    ! -- dummy variables
    class(SwfMmrType), intent(inout) :: this !< SwfMmrType object
    ! -- local variables
    integer(I4B) :: i
    integer(I4B) :: j
    integer(I4B) :: nn1
    class(ObserveType), pointer :: obsrv => null()
    ! -- formats
    !
    ! -- process each package observation
    !    only done the first stress period since boundaries are fixed
    !    for the simulation
    if (kper == 1) then
      do i = 1, this%obs%npakobs
        obsrv => this%obs%pakobs(i)%obsrv
        !
        ! -- get node number 1
        nn1 = obsrv%NodeNumber
        if (nn1 < 1 .or. nn1 > this%disl%nodes) then
          write (errmsg, '(a,1x,a,1x,i0,1x,a,1x,i0,a)') &
            trim(adjustl(obsrv%ObsTypeId)), &
            'reach must be greater than 0 and less than or equal to', &
            this%disl%nodes, '(specified value is ', nn1, ')'
          call store_error(errmsg)
        else
          if (obsrv%indxbnds_count == 0) then
            call obsrv%AddObsIndex(nn1)
          else
            errmsg = 'Programming error in mmr_rp_obs'
            call store_error(errmsg)
          end if
        end if
        !
        ! -- check that node number 1 is valid; call store_error if not
        do j = 1, obsrv%indxbnds_count
          nn1 = obsrv%indxbnds(j)
          if (nn1 < 1 .or. nn1 > this%disl%nodes) then
            write (errmsg, '(a,1x,a,1x,i0,1x,a,1x,i0,a)') &
              trim(adjustl(obsrv%ObsTypeId)), &
              'reach must be greater than 0 and less than or equal to', &
              this%disl%nodes, '(specified value is ', nn1, ')'
            call store_error(errmsg)
          end if
        end do
      end do
      !
      ! -- evaluate if there are any observation errors
      if (count_errors() > 0) then
        call this%parser%StoreErrorUnit()
      end if
    end if
    !
    ! -- return
    return
  end subroutine mmr_rp_obs

end module SwfMmrModule

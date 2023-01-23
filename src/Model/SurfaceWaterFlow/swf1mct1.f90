!> @brief Stream Network Flow (SWF) Muskingum-Cunge-Todini (MCT) Module
!!
!! This module solves one-dimensional flow routing using a linear Muskingum
!! approach.
!! 
!<
module SwfMctModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LENMEMPATH, LENVARNAME, LINELENGTH, &
                             DZERO, DHALF, DONE, DTWO, DTHREE, &
                             LENBUDTXT, DNODATA, DEM5
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
  public :: SwfMctType, mct_cr

  ! todo: get elevation from disl, do not include it here

  character(len=LENBUDTXT), dimension(2) :: budtxt = & !< text labels for budget terms
    &['         STORAGE', '     EXT-OUTFLOW']

  type, extends(NumericalPackageType) :: SwfMctType

    ! -- user-provided input
    real(DP), pointer :: unitconv !< numerator used in mannings equation to convert between units (1.0 for metric)
    integer(I4B), dimension(:), pointer, contiguous :: icalc_order => null() !< routing calculation order
    real(DP), dimension(:), pointer, contiguous :: qoutflow0 => null() !< initial outflow for each reach
    real(DP), dimension(:), pointer, contiguous :: width => null() !< reach width
    real(DP), dimension(:), pointer, contiguous :: manningsn => null() !< mannings roughness for each reach
    real(DP), dimension(:), pointer, contiguous :: elevation => null() !< elevation for each reach
    real(DP), dimension(:), pointer, contiguous :: slope => null() !< slope for each reach
    integer(I4B), dimension(:), pointer, contiguous :: idcxs => null() !< cross section id for each reach

    ! -- input arguments to calc_mct routine
    real(DP), dimension(:), pointer, contiguous :: qinflow_old => null() !< inflow to each reach for last time step
    real(DP), dimension(:), pointer, contiguous :: qinflow => null() !< inflow to each reach for current time step
    real(DP), dimension(:), pointer, contiguous :: qoutflow_old => null() !< outflow from each reach for last time step
    real(DP), dimension(:), pointer, contiguous :: qoutflow => null() !< outflow from each reach for current time step
    real(DP), dimension(:), pointer, contiguous :: cstar_old => null() !< mct cstar_old variable
    real(DP), dimension(:), pointer, contiguous :: dstar_old => null() !< mct dstar_old variable

    ! -- budget vectors
    real(DP), dimension(:), pointer, contiguous :: qextoutflow => null() !< flows leaving model (for toreach = 0)
    real(DP), dimension(:), pointer, contiguous :: qsto => null() !< storage rates

    ! -- pointer to concrete disl subclass of DisBaseType
    type(SwfDislType), pointer :: disl

    ! -- observation data
    integer(I4B), pointer :: inobspkg => null() !< unit number for obs package
    type(ObsType), pointer :: obs => null() !< observation package

    ! -- pointers to cross section data
    integer(I4B), pointer :: incxs => null() !< flag to indicate if cross section package is available
    real(DP), dimension(:), pointer, contiguous :: xfraction => null() !< cross-section relative x distance, of size npoints
    real(DP), dimension(:), pointer, contiguous :: height => null() !< cross-section heights, of size npoints
    real(DP), dimension(:), pointer, contiguous :: manfraction => null() !< cross-section roughness data, of size npoints
    integer(I4B), dimension(:), pointer, contiguous :: iacross => null() !< pointers to cross-section data for each section, of size nsections + 1

  contains

    procedure :: allocate_scalars
    procedure :: allocate_arrays
    procedure :: mct_load
    procedure :: source_options
    procedure :: log_options
    procedure :: source_griddata
    procedure :: log_griddata
    procedure :: mct_ar
    procedure :: set_cxs_pointers
    procedure :: mct_rp
    procedure :: mct_ad
    procedure :: mct_init_data
    procedure :: mct_solve
    procedure :: mct_cq
    procedure :: mct_bd
    procedure :: mct_save_model_flows
    procedure :: mct_print_model_flows
    procedure :: mct_da
    procedure :: mct_df_obs
    procedure :: mct_rp_obs
    procedure :: mct_bd_obs

  end type SwfMctType

  contains

  !> @brief create package
  !<
  subroutine mct_cr(mctobj, name_model, input_mempath, inunit, iout, dis, incxs)
    ! -- modules
    use MemoryManagerExtModule, only: mem_set_value
    ! -- dummy
    type(SwfMctType), pointer :: mctobj
    character(len=*), intent(in) :: name_model
    character(len=*), intent(in) :: input_mempath
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    class(DisBaseType), pointer, intent(inout) :: dis !< the pointer to the discretization
    integer(I4B), intent(in) :: incxs
    ! -- locals
    logical(LGP) :: found_fname
    ! -- formats
    character(len=*), parameter :: fmtheader = &
      "(1x, /1x, 'MCT --  MUSKINGUM-CUNGE-TODINI (MCT) PACKAGE, VERSION 1, 5/22/2023', &
       &' INPUT READ FROM MEMPATH: ', A, /)"
    !
    ! -- Create the object
    allocate (mctobj)

    ! -- create name and memory path
    call mctobj%set_names(1, name_model, 'MCT', 'MCT')

    ! -- Allocate scalars
    call mctobj%allocate_scalars()

    ! -- Set variables
    mctobj%input_mempath = input_mempath
    mctobj%inunit = inunit
    mctobj%iout = iout
    mctobj%incxs = incxs

    ! -- set name of input file
    call mem_set_value(mctobj%input_fname, 'INPUT_FNAME', mctobj%input_mempath, &
                       found_fname)

    ! -- store pointer to disl
    !    Not normally good practice, but since SWF only works with DISL
    !    may be okay
    mctobj%dis => dis
    select type (dis)
    type is (SwfDislType)
      mctobj%disl => dis
    end select

    ! -- create obs package
    call obs_cr(mctobj%obs, mctobj%inobspkg)

    ! -- check if mct is enabled
    if (inunit > 0) then
      
      ! -- Print a message identifying the package.
      write (iout, fmtheader) input_mempath

      ! -- allocate arrays
      call mctobj%allocate_arrays()

      ! -- load mct
      call mctobj%mct_load()

    end if

    ! -- Return
    return
  end subroutine mct_cr

  !> @ brief Allocate scalars
  !!
  !! Allocate and initialize scalars for the package. The base model
  !! allocate scalars method is also called.
  !!
  !<
  subroutine allocate_scalars(this)
    ! -- modules
    ! -- dummy
    class(SwfMcttype) :: this
    !
    ! -- allocate scalars in NumericalPackageType
    call this%NumericalPackageType%allocate_scalars()
    !
    ! -- Allocate scalars
    call mem_allocate(this%unitconv, 'UNITCONV', this%memoryPath)
    call mem_allocate(this%inobspkg, 'INOBSPKG', this%memoryPath)
    call mem_allocate(this%incxs, 'INCXS', this%memoryPath)

    this%unitconv = DONE
    this%inobspkg = 0
    this%incxs = 0

    return
  end subroutine allocate_scalars

  !> @brief allocate memory for arrays
  !<
  subroutine allocate_arrays(this)
    ! -- dummy
    class(SwfMctType) :: this
    ! -- locals
    integer(I4B) :: n
    !
    ! -- user-provided input
    call mem_allocate(this%icalc_order, this%dis%nodes, 'ICALC_ORDER', this%memoryPath)
    call mem_allocate(this%qoutflow0, this%dis%nodes, 'QOUTFLOW0', this%memoryPath)
    call mem_allocate(this%width, this%dis%nodes, 'WIDTH', this%memoryPath)
    call mem_allocate(this%manningsn, this%dis%nodes, 'MANNINGSN', this%memoryPath)
    call mem_allocate(this%elevation, this%dis%nodes, 'ELEVATION', this%memoryPath)
    call mem_allocate(this%slope, this%dis%nodes, 'SLOPE', this%memoryPath)
    call mem_allocate(this%idcxs, this%dis%nodes, 'IDCXS', this%memoryPath)

    ! -- input arguments to calc_mct routine
    call mem_allocate(this%qinflow_old, this%dis%nodes, 'QINFLOW_OLD', this%memoryPath)
    call mem_allocate(this%qinflow, this%dis%nodes, 'QINFLOW', this%memoryPath)
    call mem_allocate(this%qoutflow_old, this%dis%nodes, 'QOUTFLOW_OLD', this%memoryPath)
    call mem_allocate(this%qoutflow, this%dis%nodes, 'QOUTFLOW', this%memoryPath)
    call mem_allocate(this%cstar_old, this%dis%nodes, 'CSTAR_OLD', this%memoryPath)
    call mem_allocate(this%dstar_old, this%dis%nodes, 'DSTAR_OLD', this%memoryPath)

    ! -- budgeting variables
    call mem_allocate(this%qextoutflow, this%dis%nodes, 'QEXTOUTFLOW', this%memoryPath)
    call mem_allocate(this%qsto, this%dis%nodes, 'QSTO', this%memoryPath)

    do n = 1, this%dis%nodes

      this%icalc_order(n) = 0
      this%qoutflow0(n) = DZERO
      this%width(n) = DZERO
      this%manningsn(n) = DZERO
      this%elevation(n) = DZERO
      this%slope(n) = DZERO
      this%idcxs(n) = DZERO
      
      this%qinflow_old(n) = DZERO
      this%qinflow(n) = DZERO
      this%qoutflow_old(n) = DZERO
      this%qoutflow(n) = DZERO
      this%cstar_old(n) = DZERO
      this%dstar_old(n) = DZERO

      this%qextoutflow(n) = DZERO
      this%qsto(n) = DZERO

    end do

    ! -- Return
    return
  end subroutine allocate_arrays
    
  !> @brief load data from IDM to package
  !<
  subroutine mct_load(this)
    ! -- dummy
    class(SwfMctType) :: this
    ! -- locals
    !
    ! -- source input data
    call this%source_options()
    call this%source_griddata()
    !
    ! -- Return
    return
  end subroutine mct_load

  !> @brief Copy options from IDM into package
  !<
  subroutine source_options(this)
    ! -- modules
    use KindModule, only: LGP
    use InputOutputModule, only: getunit, openfile
    use MemoryManagerExtModule, only: mem_set_value
    use CharacterStringModule, only: CharacterStringType
    use SwfMctInputModule, only: SwfMctParamFoundType
    ! -- dummy
    class(SwfMctType) :: this
    ! -- locals
    integer(I4B) :: isize
    type(SwfMctParamFoundType) :: found
    type(CharacterStringType), dimension(:), pointer, &
      contiguous :: obs6_fnames
    !
    ! -- update defaults with idm sourced values
    call mem_set_value(this%unitconv, 'UNITCONV', this%input_mempath, found%unitconv)
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
      found%obs6_filename = .true.
      this%obs%inputFilename = obs6_fnames(1)
      this%obs%active = .true.
      this%inobspkg = GetUnit()
      this%obs%inUnitObs = this%inobspkg
      call openfile(this%inobspkg, this%iout, this%obs%inputFilename, 'OBS')
      call this%obs%obs_df(this%iout, this%packName, this%filtyp, this%dis)
      call this%mct_df_obs()
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
    use SwfMctInputModule, only: SwfMctParamFoundType
    class(SwfMctType) :: this
    type(SwfMctParamFoundType), intent(in) :: found

    write (this%iout, '(1x,a)') 'Setting MCT Options'

    if (found%unitconv) then
      write (this%iout, '(4x,a, G0)') 'Mannings unit conversion value &
                                  &specified as ', this%unitconv
    end if

    if (found%iprflow) then
      write (this%iout, '(4x,a)') 'Cell-by-cell flow information will be printed &
                                  &to listing file whenever ICBCFL is not zero.'
    end if

    if (found%ipakcb) then
      write (this%iout, '(4x,a)') 'Cell-by-cell flow information will be printed &
                                  &to listing file whenever ICBCFL is not zero.'
    end if

    if (found%obs6_filename) then
      write (this%iout, '(4x,a)') 'Observation package is active.'
    end if

    write (this%iout, '(1x,a,/)') 'End Setting MCT Options'

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
    use SwfMctInputModule, only: SwfMctParamFoundType
    ! -- dummy
    class(SwfMctType) :: this
    ! -- locals
    character(len=LENMEMPATH) :: idmMemoryPath
    type(SwfMctParamFoundType) :: found
    integer(I4B), dimension(:), pointer, contiguous :: map
    ! -- formats
    !
    ! -- set memory path
    idmMemoryPath = create_mem_path(this%name_model, 'MCT', idm_context)
    !
    ! -- set map to convert user input data into reduced data
    map => null()
    if (this%dis%nodes < this%dis%nodesuser) map => this%dis%nodeuser
    !
    ! -- update defaults with idm sourced values
    call mem_set_value(this%icalc_order, 'ICALC_ORDER', idmMemoryPath, map, found%icalc_order)
    call mem_set_value(this%qoutflow0, 'QOUTFLOW0', idmMemoryPath, map, found%qoutflow0)
    call mem_set_value(this%width, 'WIDTH', idmMemoryPath, map, found%width)
    call mem_set_value(this%manningsn, 'MANNINGSN', idmMemoryPath, map, found%manningsn)
    call mem_set_value(this%elevation, 'ELEVATION', idmMemoryPath, map, found%elevation)
    call mem_set_value(this%slope, 'SLOPE', idmMemoryPath, map, found%slope)
    call mem_set_value(this%idcxs, 'IDCXS', idmMemoryPath, map, found%idcxs)
    !
    ! -- ensure ICALC_ORDER was found
    if (.not. found%icalc_order) then
      write (errmsg, '(a)') 'Error in GRIDDATA block: ICALC_ORDER not found.'
      call store_error(errmsg)
    end if
    !
    ! -- ensure QOUTFLOW0 was found
    if (.not. found%qoutflow0) then
      write (errmsg, '(a)') 'Error in GRIDDATA block: QOUTFLOW0 not found.'
      call store_error(errmsg)
    end if
    !
    ! -- ensure WIDTH was found
    if (.not. found%width) then
      write (errmsg, '(a)') 'Error in GRIDDATA block: WIDTH not found.'
      call store_error(errmsg)
    end if
    !
    ! -- ensure MANNINGSN was found
    if (.not. found%manningsn) then
      write (errmsg, '(a)') 'Error in GRIDDATA block: MANNINGSN not found.'
      call store_error(errmsg)
    end if
    !
    ! -- ensure ELEVATION was found
    if (.not. found%elevation) then
      write (errmsg, '(a)') 'Error in GRIDDATA block: ELEVATION not found.'
      call store_error(errmsg)
    end if
    !
    ! -- ensure SLOPE was found
    if (.not. found%slope) then
      write (errmsg, '(a)') 'Error in GRIDDATA block: SLOPE not found.'
      call store_error(errmsg)
    end if
    !
    ! -- ensure IDCXS was found
    if (.not. found%idcxs) then
      write (errmsg, '(a)') 'Error in GRIDDATA block: IDCXS not found.'
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
    use SwfMctInputModule, only: SwfMctParamFoundType
    class(SwfMctType) :: this
    type(SwfMctParamFoundType), intent(in) :: found

    write (this%iout, '(1x,a)') 'Setting MCT Griddata'

    if (found%icalc_order) then
      write (this%iout, '(4x,a)') 'ICALC_ORDER set from input file'
    end if

    if (found%qoutflow0) then
      write (this%iout, '(4x,a)') 'QOUTFLOW0 set from input file'
    end if

    if (found%width) then
      write (this%iout, '(4x,a)') 'WIDTH set from input file'
    end if

    if (found%manningsn) then
      write (this%iout, '(4x,a)') 'MANNINGSN set from input file'
    end if

    if (found%elevation) then
      write (this%iout, '(4x,a)') 'ELEVATION set from input file'
    end if

    if (found%slope) then
      write (this%iout, '(4x,a)') 'SLOPE set from input file'
    end if

    if (found%idcxs) then
      write (this%iout, '(4x,a)') 'IDCXS set from input file'
    end if

    write (this%iout, '(1x,a,/)') 'End Setting MCT Griddata'

  end subroutine log_griddata

  !> @brief allocate memory
  !<
  subroutine mct_ar(this)
    ! -- modules
    ! -- dummy
    class(SwfMctType) :: this !< this instance

    ! -- set pointers to cxs arrays
    call this%set_cxs_pointers()

    ! - observation data
    call this%obs%obs_ar()

    ! -- initialize routing variables
    call this%mct_init_data()

    return
  end subroutine mct_ar

  !> @brief Set pointers to cross section arrays
  !!
  !!  If cross section package is not active, then create
  !!  small arrays that can be passed to mct routines even
  !!  though they won't be used.
  !!
  !<
  subroutine set_cxs_pointers(this)
    ! -- modules
    use MemoryManagerModule, only: mem_setptr, mem_allocate
    ! -- dummy
    class(SwfMctType) :: this !< this instance
    ! -- local
    character(len=LENMEMPATH) :: cxs_mem_path

    if (this%incxs > 0) then
      ! set pointers to cxs arrays
      cxs_mem_path = create_mem_path(this%name_model, 'CXS')
      call mem_setptr(this%xfraction, 'XFRACTION', cxs_mem_path)
      call mem_setptr(this%height, 'HEIGHT', cxs_mem_path)
      call mem_setptr(this%manfraction, 'MANFRACTION', cxs_mem_path)
      call mem_setptr(this%iacross, 'IACROSS', cxs_mem_path)
    else
      ! allocate arrays with size 1 so they can be passed around
      call mem_allocate(this%xfraction, 1, 'XFRACTION', this%memoryPath)
      call mem_allocate(this%height, 1, 'HEIGHT', this%memoryPath)
      call mem_allocate(this%manfraction, 1, 'MANFRACTION', this%memoryPath)
      call mem_allocate(this%iacross, 2, 'IACROSS', this%memoryPath)
      this%xfraction(1) = DNODATA
      this%height(1) = DNODATA
      this%manfraction(1) = DNODATA
      this%iacross(1) = 1
      this%iacross(2) = 1
    end if

  end subroutine set_cxs_pointers

  !> @brief allocate memory
  !<
  subroutine mct_rp(this)
    ! -- modules
    ! -- dummy
    class(SwfMctType) :: this !< this instance
    !
    ! -- read observations
    call this%mct_rp_obs()
    return
  end subroutine mct_rp

  subroutine mct_ad(this, irestore)
    !
    class(SwfMctType) :: this
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
  end subroutine mct_ad

  !> @brief solve
  !<
  subroutine mct_solve(this, rhs)
    ! -- modules
    use TdisModule, only: delt
    ! -- dummy
    class(SwfMctType) :: this !< this instance
    real(DP), dimension(:), intent(in) :: rhs !< right-hand-side vector of boundary package inflows
    ! -- local
    integer(I4B) :: icalcmeth = 1 !< 0 is composite linear, 1 is by section, 2 is composite nonlinear
   
    !call calc_muskingum(this%disl%toreach, this%icalc_order, this%qinflow_old, &
    !                    this%qoutflow_old, this%qinflow, this%qoutflow, &
    !                    this%c0, this%c1, this%c2, -rhs)

    ! todo: replace with
    ! call calc_mct(...)
    call calc_mct(this%disl%toreach, &
                  this%icalc_order, &
                  this%qinflow_old, &
                  this%qoutflow_old, &
                  this%qinflow, &
                  this%qoutflow, &
                  this%cstar_old, &
                  this%dstar_old, &
                  -rhs, & !qsource
                  this%width, &
                  this%manningsn, &
                  this%slope, &
                  this%unitconv, &
                  this%disl%reach_length, &
                  this%iacross, &
                  this%xfraction, &
                  this%height, &
                  this%manfraction, &
                  this%idcxs, &
                  icalcmeth, &
                  delt)

    ! -- return
    return
  end subroutine mct_solve

  subroutine mct_cq(this, flowja)
    ! -- dummy
    class(SwfMctType) :: this
    real(DP), intent(inout), dimension(:) :: flowja
    ! -- local
    integer(I4B) :: n, ipos, m
    real(DP) :: qnm, q
    !
    ! -- Transfer qoutflow into flowja
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
  end subroutine mct_cq

  !> @ brief Model budget calculation for package
  !!
  !!  Budget calculation for the MCT package components. Components include
  !!  external outflow
  !!
  !<
  subroutine mct_bd(this, isuppress_output, model_budget)
    ! -- modules
    use TdisModule, only: delt
    use BudgetModule, only: BudgetType, rate_accumulator
    ! -- dummy variables
    class(SwfMctType) :: this !< SwfMctType object
    integer(I4B), intent(in) :: isuppress_output !< flag to suppress model output
    type(BudgetType), intent(inout) :: model_budget !< model budget object
    ! -- local variables
    real(DP) :: rin
    real(DP) :: rout
    !
    ! -- Add external outflow rates to model budget
    call rate_accumulator(this%qextoutflow, rin, rout)
    call model_budget%addentry(rin, rout, delt, '             MCT', &
                               isuppress_output, '     EXT-OUTFLOW')
    !
    ! -- Add storage rates to model budget
    call rate_accumulator(this%qsto, rin, rout)
    call model_budget%addentry(rin, rout, delt, '             MCT', &
                               isuppress_output, '         STORAGE')
    !
    ! -- return
    return
  end subroutine mct_bd

  !> @ brief save flows for package
  !<
  subroutine mct_save_model_flows(this, flowja, icbcfl, icbcun)
    ! -- dummy
    class(SwfMctType) :: this
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
  end subroutine mct_save_model_flows

  !> @ brief print flows for package
  !<
  subroutine mct_print_model_flows(this, ibudfl, flowja)
    ! -- modules
    use TdisModule, only: kper, kstp
    use ConstantsModule, only: LENBIGLINE
    ! -- dummy
    class(SwfMctType) :: this
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
  end subroutine mct_print_model_flows

  !> @brief deallocate memory
  !<
  subroutine mct_da(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    use MemoryManagerExtModule, only: memorylist_remove
    use SimVariablesModule, only: idm_context
    ! -- dummy
    class(SwfMctType) :: this
    !
    ! -- Deallocate input memory
    call memorylist_remove(this%name_model, 'MCT', idm_context)
    !
    ! -- Scalars
    call mem_deallocate(this%unitconv)
    !
    ! -- Deallocate arrays
    call mem_deallocate(this%icalc_order)
    call mem_deallocate(this%qoutflow0)
    call mem_deallocate(this%width)
    call mem_deallocate(this%manningsn)
    call mem_deallocate(this%elevation)
    call mem_deallocate(this%slope)
    call mem_deallocate(this%idcxs)

    ! -- input arguments to calc_mct routine
    call mem_deallocate(this%qinflow_old)
    call mem_deallocate(this%qinflow)
    call mem_deallocate(this%qoutflow_old)
    call mem_deallocate(this%qoutflow)
    call mem_deallocate(this%cstar_old)
    call mem_deallocate(this%dstar_old)

    ! -- budget variables
    call mem_deallocate(this%qextoutflow)
    call mem_deallocate(this%qsto)

    ! -- obs package
    call mem_deallocate(this%inobspkg)
    call this%obs%obs_da()
    deallocate (this%obs)
    nullify (this%obs)

    ! -- cross sections
    if (this%incxs > 0) then
      this%xfraction => null()
      this%height => null()
      this%manfraction => null()
      this%iacross => null()
    else
      call mem_deallocate(this%xfraction)
      call mem_deallocate(this%height)
      call mem_deallocate(this%manfraction)
      call mem_deallocate(this%iacross)
    end if
    call mem_deallocate(this%incxs)

    ! -- deallocate parent
    call this%NumericalPackageType%da()
    !
    ! -- Return
    return
  end subroutine mct_da

  !> @brief initialize mct data
  !!
  !! This routine is only called once at the beginning of
  !! the simulation from mct_ar()
  !!
  !<
  subroutine mct_init_data(this)
    ! -- modules
    ! -- dummy
    class(SwfMctType) :: this !< this instance
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
    allocate(nupreaches(this%dis%nodes))
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
  end subroutine mct_init_data

  subroutine calc_mct(itoreach, icalc_order, qinflow_old, qoutflow_old, &
                      qinflow, qoutflow, cstar_old, dstar_old, qsource, &
                      width, rough, slope, unitconv, reach_length, &
                      iacross, xfraction, height, manfraction, idcxs, &
                      icalcmeth, delt)
    ! -- dummy
    integer(I4B), dimension(:), intent(in) :: itoreach
    integer(I4B), dimension(:), intent(in) :: icalc_order
    real(DP), dimension(:), intent(in) :: qinflow_old
    real(DP), dimension(:), intent(in) :: qoutflow_old
    real(DP), dimension(:), intent(inout) :: qinflow
    real(DP), dimension(:), intent(inout) :: qoutflow
    real(DP), dimension(:), intent(inout) :: cstar_old
    real(DP), dimension(:), intent(inout) :: dstar_old
    real(DP), dimension(:), intent(in) :: qsource
    real(DP), dimension(:), intent(in) :: width
    real(DP), dimension(:), intent(in) :: rough
    real(DP), dimension(:), intent(in) :: slope
    real(DP), intent(in) :: unitconv
    real(DP), dimension(:), intent(in) :: reach_length
    integer(I4B), dimension(:), intent(in) :: iacross
    real(DP), dimension(:), intent(in) :: xfraction
    real(DP), dimension(:), intent(in) :: height
    real(DP), dimension(:), intent(in) :: manfraction
    integer(I4B), dimension(:), intent(in) :: idcxs
    integer(I4B), intent(inout) :: icalcmeth
    real(DP), intent(in) :: delt
    ! -- local
    integer(I4B) :: npts
    integer(I4B) :: id
    integer(I4B) :: n
    integer(I4B) :: j
    integer(I4B) :: i
    integer(I4B) :: niter = 2
    integer(I4B) :: i0
    integer(I4B) :: i1
    real(DP) :: qout_new
    real(DP) :: qin_new
    real(DP) :: cstar_new
    real(DP) :: dstar_new

    ! -- Initialize qinflow with any sources, such as lateral inflow
    do n = 1, size(itoreach)
      j = icalc_order(n)
      qinflow(j) = qsource(j)
    end do

    ! -- Use MCT method to calculate outflows and accumulate
    !    outflows in downstream reaches
    do n = 1, size(itoreach)
      j = icalc_order(n)
      qin_new = qinflow(j) !+ qsource(j)

      !qout_new = c0(j) * qin_new + c1(j) * qinflow_old(j) + c2(j) * qoutflow_old(j)
      
      ! -- If the cross section id is 0, then it is a hydraulically wide channel,
      !    and only width and rough are needed (not xfraction, height, and manfraction)
      id = idcxs(j)
      if (id > 0) then
        i0 = iacross(id)
        i1 = iacross(id + 1) - 1
      else
        i0 = 1
        i1 = 1
      end if

      ! set icalcmeth based on number of cross section points
      npts = i1 - i0 + 1
      icalcmeth = 0 ! linear composite mannings resistance
      if (npts > 4) then
        icalcmeth = 1 ! sum q by cross section segments
      end if

      call mct_solve_qout(qoutflow_old(j), qin_new, qinflow_old(j), &
                          cstar_old(j), dstar_old(j), niter, &
                          xfraction(i0:i1), height(i0:i1), manfraction(i0:i1), &
                          width(j), rough(j), slope(j), unitconv, &
                          reach_length(j), delt, icalcmeth, &
                          qout_new, cstar_new, dstar_new)
      
      qoutflow(j) = qout_new
      i = itoreach(j)
      if (i > 0) then
          qinflow(i) = qinflow(i) + qout_new
      end if

      cstar_old(j) = cstar_new
      dstar_old(j) = dstar_new

    end do

  end subroutine calc_mct

  !> @brief Solve qout for a single reach
  !<
  subroutine mct_solve_qout(qout_old, qin_new, qin_old, cstar_old, dstar_old, &
                            niter, cxs_xf, cxs_h, cxs_rf, width, rough, slope, &
                            unitconv, dx, dt, icalcmeth, qout_new, &
                            cstar_new, dstar_new)
    real(DP), intent(in) :: qout_old !< outflow from last time step
    real(DP), intent(in) :: qin_new !< inflow from current time step
    real(DP), intent(in) :: qin_old !< inflow from last time step
    real(DP), intent(inout) :: cstar_old !< cstar from last time step
    real(DP), intent(inout) :: dstar_old !< dstar from last time step
    integer(I4B), intent(in) :: niter !< number of iterations to perform
    real(DP), intent(in), dimension(:) :: cxs_xf !< cross section x-fraction values
    real(DP), intent(in), dimension(:) :: cxs_h !< cross section height values
    real(DP), intent(in), dimension(:) :: cxs_rf !< cross section manning roughness fraction values
    real(DP), intent(in) :: width !< reach width
    real(DP), intent(in) :: rough !< mannings n
    real(DP), intent(in) :: slope !< bottom slope
    real(DP), intent(in) :: unitconv !< numerator in mannings equation for unit conversion
    real(DP), intent(in) :: dx !< reach length
    real(DP), intent(in) :: dt !< time step length
    integer(I4B), intent(in) :: icalcmeth !< method for calculating manning flow
    real(DP), intent(out) :: qout_new !< calculated reach outflow for this time step
    real(DP), intent(out) :: cstar_new
    real(DP), intent(out) :: dstar_new
    ! -- local
    integer(I4B) :: kiter
    real(DP) :: qout_iter
    real(DP) :: depth

    ! TODO: MAKE THIS MORE EFFICIENT
    ! calculate cstar_old and dstar_old
    call calc_mct_parameters(qin_old, qout_old, cxs_xf, cxs_h, cxs_rf, & 
                              width, rough, slope, unitconv, dx, dt, &
                              icalcmeth, cstar_old, dstar_old, depth)

    ! estimate qout_iter from known ins and outs
    qout_iter = qout_old + (qin_new - qin_old)

    ! iterate to resolve nonlinearities
    do kiter = 1, niter

      ! calculate cstar and dstar
      call calc_mct_parameters(qin_new, qout_iter, cxs_xf, cxs_h, cxs_rf, & 
                               width, rough, slope, unitconv, dx, dt, &
                               icalcmeth, cstar_new, dstar_new, depth)

      ! calculate new estimate of qout_iter
      call mct_calc_qout(qin_new, qin_old, qout_old, cstar_new, dstar_new, &
                         cstar_old, dstar_old, qout_iter)

    end do

    ! assign qout_new and ensure it is positive
    if (qout_iter > 0) then
      qout_new = qout_iter
    else
      qout_new = qout_old
    end if

    ! recalculate cstar_new and dstar_new so they can be saved for next time step
    call calc_mct_parameters(qin_new, qout_iter, cxs_xf, cxs_h, cxs_rf, &
                             width, rough, slope, unitconv, dx, dt, icalcmeth, &
                             cstar_new, dstar_new, depth)

  end subroutine mct_solve_qout

  !> @brief Calculate MCT cstar and dstar parameters
  !<
  subroutine calc_mct_parameters(qin, qout, cxs_xf, cxs_h, cxs_rf, &
                                 width, rough, slope, unitconv, &
                                 dx, dt, icalcmeth, cstar, dstar, depth)
    use SwfCxsUtilsModule, only: calc_depth_from_q, &
                                 get_cross_section_area, &
                                 get_wetted_topwidth
    real(DP), intent(in) :: qin !< inflow
    real(DP), intent(in) :: qout !< outflow
    real(DP), intent(in), dimension(:) :: cxs_xf !< cross section x fraction values for current reach
    real(DP), intent(in), dimension(:) :: cxs_h !< cross section height values for current reach
    real(DP), intent(in), dimension(:) :: cxs_rf !< cross section manning roughness fraction values for current reach
    real(DP), intent(in) :: width !< reach width
    real(DP), intent(in) :: rough !< mannings n
    real(DP), intent(in) :: slope !< bottom slope
    real(DP), intent(in) :: unitconv !< numerator in mannings equation for unit conversion
    real(DP), intent(in) :: dx !< reach length
    real(DP), intent(in) :: dt !< time step length
    integer(I4B), intent(in) :: icalcmeth !< method for calculating manning flow
    real(DP), intent(out) :: cstar !< calculated mct cstar parameter
    real(DP), intent(out) :: dstar !< calculated mct dstar parameter
    real(DP), intent(out) :: depth !< calculated depth
    ! -- local
    integer(I4B) :: npts
    real(DP) :: fa
    real(DP) :: beta
    real(DP) :: celerity
    real(DP) :: wa
    real(DP) :: tw
    
    ! flow average
    fa = DHALF * (qin + qout)

    ! ! depth
    ! TODO: THIS ROUTINE IS ONLY FOR N-POINT CROSS SECTIONS
    ! TODO: WILL NEED TO SUPPORT ICALCMETHOD
    depth = calc_depth_from_q(fa, width, rough, slope, &
                              cxs_xf, cxs_h, cxs_rf, unitconv, &
                              icalcmeth)
    ! call qtodepth(fa, cxs_x, csx_h, cxs_r, slope, icalcmeth, depth)

    ! ! celerity
    celerity = calc_celerity(fa, depth, width, rough, slope, &
                             cxs_xf, cxs_h, cxs_rf, unitconv, &
                             icalcmeth)
    ! call calc_celerity(fa, cxs_x, csx_h, cxs_r, slope, icalcmeth, depth, &
    !                    celerity)

    ! ! wetted cross sectional area
    ! call wetted_area(depth, cxs_x, cxs_h, wa)
    npts = size(cxs_xf)
    if (npts > 1) then
      wa = get_cross_section_area(npts, cxs_xf, cxs_h, width, depth)
    else
      wa = depth * width
    end if

    ! ! beta
    beta = celerity * wa / fa

    ! ! cstar
    cstar = celerity / beta * dt / dx

    ! ! top width
    !tw = wetted_topwidth(depth, cxs_x, cxs_h, cxs_r)
    if (npts > 1) then
      tw = get_wetted_topwidth(npts, cxs_xf, cxs_h, width, depth)
    else
      tw = width
    end if

    ! ! dstar
    dstar = fa / beta / tw / slope / celerity / dx

    !print*, fa, depth, celerity, wa, beta, cstar, tw, dstar
    !stop

    return
  end subroutine calc_mct_parameters

  function calc_celerity(flow, depth, width, rough, slope, &
                         cxs_xf, cxs_h, cxs_rf, unitconv, &
                         icalcmeth) result(celerity)
    ! -- modules
    use SwfCxsUtilsModule, only: get_wetted_topwidth, calc_qman
    ! -- dummy
    real(DP), intent(in) :: flow !< reach flow
    real(DP), intent(in) :: depth !< reach depth
    real(DP), intent(in) :: width !< reach width
    real(DP), intent(in) :: rough !< mannings n
    real(DP), intent(in) :: slope !< bottom slope
    real(DP), intent(in), dimension(:) :: cxs_xf !< cross section x fraction values for current reach
    real(DP), intent(in), dimension(:) :: cxs_h !< cross section height values for current reach
    real(DP), intent(in), dimension(:) :: cxs_rf !< cross section manning roughness fraction values for current reach
    real(DP), intent(in) :: unitconv !< numerator in mannings equation for unit conversion
    integer(I4B), intent(in) :: icalcmeth !< manning calculation method
    ! -- return
    real(DP) :: celerity
    ! -- local
    real(DP) :: deps = DEM5
    real(DP) :: top_width
    real(DP) :: flow_plus
    real(DP) :: flow_minus
    integer(I4B) :: npts

    npts = size(cxs_xf)
    if (npts > 1) then
      top_width = get_wetted_topwidth(npts, cxs_xf, cxs_h, width, depth)
    else
      top_width = width
    end if
    flow_plus = calc_qman(depth + deps, width, rough, slope, &
                          cxs_xf, cxs_h, cxs_rf, unitconv, icalcmeth)
    flow_minus = calc_qman(depth - deps, width, rough, slope, &
                          cxs_xf, cxs_h, cxs_rf, unitconv, icalcmeth)
    celerity = (flow_plus - flow_minus) / deps / DTWO / top_width
    
    return
  end function calc_celerity

  !> @brief Calculate reach outflow using MCT equation
  !<
  subroutine mct_calc_qout(qin_new, qin_old, qout_old, cstar_new, dstar_new, &
                           cstar_old, dstar_old, qout_new)
    real(DP), intent(in) :: qin_new !< inflow from current time step
    real(DP), intent(in) :: qin_old !< inflow from last time step
    real(DP), intent(in) :: qout_old !< outflow from last time step
    real(DP), intent(in) :: cstar_old !< cstar from last time step
    real(DP), intent(in) :: dstar_old !< dstar from last time step
    real(DP), intent(in) :: cstar_new !< cstar from this time step
    real(DP), intent(in) :: dstar_new !< dstar from this time step
    real(DP), intent(out) :: qout_new !< calculated reach outflow for this time step
    ! local
    real(DP) :: c1
    real(DP) :: c2
    real(DP) :: c3
    real(DP) :: d
    real(DP) :: cfact

    d = (DONE + cstar_new + dstar_new)
    cfact = cstar_new / cstar_old
    c1 = (-DONE + cstar_new + dstar_new) / d
    c2 = (DONE + cstar_old - dstar_old) / d * cfact
    c3 = (DONE - cstar_old + dstar_old) / d * cfact
    qout_new = c1 * qin_new + c2 * qin_old + c3 * qout_old

  end subroutine mct_calc_qout

  !> @brief Define the observation types available in the package
  !!
  !! Method to define the observation types available in the package.
  !!
  !<
  subroutine mct_df_obs(this)
    ! -- dummy variables
    class(SwfMctType) :: this !< SwfMctType object
    ! -- local variables
    integer(I4B) :: indx
    !
    ! -- Store obs type and assign procedure pointer
    !    for ext-outflow observation type.
    call this%obs%StoreObsType('ext-outflow', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => mctobsidprocessor
    !
    ! -- return
    return
  end subroutine mct_df_obs


  subroutine mctobsidprocessor(obsrv, dis, inunitobs, iout)
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
    read(strng, *) n
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
  end subroutine mctobsidprocessor


  !> @brief Save observations for the package
  !!
  !! Method to save simulated values for the package.
  !!
  !<
  subroutine mct_bd_obs(this)
    ! -- dummy variables
    class(SwfMctType) :: this !< SwfMctType object
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
  end subroutine mct_bd_obs

  !> @brief Read and prepare observations for a package
  !!
  !! Method to read and prepare observations for a package.
  !!
  !<
  subroutine mct_rp_obs(this)
    ! -- modules
    use TdisModule, only: kper
    ! -- dummy variables
    class(SwfMctType), intent(inout) :: this !< SwfMctType object
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
            errmsg = 'Programming error in mct_rp_obs'
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
  end subroutine mct_rp_obs

end module SwfMctModule
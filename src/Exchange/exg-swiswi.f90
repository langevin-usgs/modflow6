!> @brief This module contains the SwiExchangeModule Module
!!
!! This module contains the code for connecting freshwater and
!! saltwater GWF models.
!!
!<
module SwiExchangeModule

  use KindModule, only: DP, I4B, LGP
  use SimVariablesModule, only: errmsg
  use SimModule, only: count_errors, store_error, store_error_filename, &
                       store_error_unit
  use BaseModelModule, only: BaseModelType, GetBaseModelFromList
  use BaseExchangeModule, only: BaseExchangeType, AddBaseExchangeToList
  use ConstantsModule, only: LENBOUNDNAME, NAMEDBOUNDFLAG, LINELENGTH, &
                             TABCENTER, TABLEFT, LENAUXNAME, DNODATA
  use ListModule, only: ListType
  use ListsModule, only: basemodellist
  use NumericalExchangeModule, only: NumericalExchangeType
  use GwfModule, only: GwfModelType
  use SimVariablesModule, only: errmsg, model_loc_idx
  use MatrixBaseModule

  implicit none

  private
  public :: SwiExchangeType
  public :: swiexchange_create
  public :: GetSwiExchangeFromList
  public :: CastAsSwiExchange

  !> @brief Derived type for SwiExchangeType
  !!
  !! This derived type contains information and methods for
  !! connecting freshwater and saltwater GWF models.
  !<
  type, extends(NumericalExchangeType) :: SwiExchangeType
    class(GwfModelType), pointer :: gwfmodel1 => null() !< pointer to GWF Model 1
    class(GwfModelType), pointer :: gwfmodel2 => null() !< pointer to GWF Model 2
    !
    ! -- options
    character(len=LINELENGTH), pointer :: filename => null() !< name of the input file
    integer(I4B), pointer :: ipr_input => null() !< flag to print input
    integer(I4B), pointer :: ipr_flow => null() !< print flag for cell by cell flows
    !
    ! -- dimensions
    integer(I4B), pointer :: nexg => null() !< number of exchanges
    !
    ! -- data
    integer(I4B), dimension(:), pointer, contiguous :: nodem1 => null() !< node numbers in model 1
    integer(I4B), dimension(:), pointer, contiguous :: nodem2 => null() !< node numbers in model 2
    real(DP), dimension(:), pointer, contiguous :: amat_fresh => null() !< newton diagonal terms for freshwater model
    real(DP), dimension(:), pointer, contiguous :: rhs_fresh => null() !< newton rhs terms for freshwater model
    real(DP), dimension(:), pointer, contiguous :: amat_salt => null() !< newton diagonal terms for saltwater model
    real(DP), dimension(:), pointer, contiguous :: rhs_salt => null() !< newton rhs terms for saltwater model
    integer(I4B), dimension(:), pointer, contiguous :: idxglo => null() !< mapping to global (solution) amat
    integer(I4B), dimension(:), pointer, contiguous :: idxsymglo => null() !< mapping to global (solution) symmetric amat
    real(DP), dimension(:), pointer, contiguous :: simvals => null() !< simulated flow rate for each exchange

  contains

    procedure :: exg_df => swi_swi_df
    procedure :: exg_ac => swi_swi_ac
    procedure :: exg_mc => swi_swi_mc
    procedure :: exg_ar => swi_swi_ar
    procedure :: exg_rp => swi_swi_rp
    procedure :: exg_ad => swi_swi_ad
    procedure :: exg_cf => swi_swi_cf
    procedure :: exg_fc => swi_swi_fc
    procedure :: exg_fn => swi_swi_fn
    ! procedure :: exg_cq => swi_swi_cq
    ! procedure :: exg_bd => swi_swi_bd
    procedure :: exg_ot => swi_swi_ot
    procedure :: exg_da => swi_swi_da
    procedure :: exg_fp => swi_swi_fp
    procedure :: get_iasym => swi_swi_get_iasym
    procedure :: allocate_scalars
    procedure :: allocate_arrays
    procedure :: source_options
    procedure :: source_dimensions
    procedure :: source_data
    procedure :: noder
    procedure :: cellstr
  end type SwiExchangeType

contains

  !> @ brief Create SWISWI exchange
  !!
  !! Create a new SWI to SWI exchange object.
  !<
  subroutine swiexchange_create(filename, name, id, m1_id, m2_id, input_mempath)
    ! -- modules
    use BaseModelModule, only: BaseModelType
    ! use VirtualModelModule, only: get_virtual_model
    use ListsModule, only: baseexchangelist
    use ObsModule, only: obs_cr
    use MemoryHelperModule, only: create_mem_path
    ! -- dummy
    character(len=*), intent(in) :: filename !< filename for reading
    character(len=*) :: name !< exchange name
    integer(I4B), intent(in) :: id !< id for the exchange
    integer(I4B), intent(in) :: m1_id !< id for model 1
    integer(I4B), intent(in) :: m2_id !< id for model 2
    character(len=*), intent(in) :: input_mempath
    ! -- local
    type(SwiExchangeType), pointer :: exchange
    class(BaseModelType), pointer :: mb
    class(BaseExchangeType), pointer :: baseexchange
    integer(I4B) :: m1_index, m2_index
    !
    ! -- Create a new exchange and add it to the baseexchangelist container
    allocate (exchange)
    baseexchange => exchange
    call AddBaseExchangeToList(baseexchangelist, baseexchange)
    !
    ! -- Assign id and name
    exchange%id = id
    exchange%name = name
    exchange%memoryPath = create_mem_path(exchange%name)
    exchange%input_mempath = input_mempath
    !
    ! -- allocate scalars and set defaults
    call exchange%allocate_scalars()
    exchange%filename = filename
    exchange%typename = 'SWI-SWI'
    !
    ! -- set gwfmodel1
    m1_index = model_loc_idx(m1_id)
    if (m1_index > 0) then
      mb => GetBaseModelFromList(basemodellist, m1_index)
      select type (mb)
      type is (GwfModelType)
        ! exchange%model1 => mb
        exchange%gwfmodel1 => mb
      end select
    end if
    ! exchange%v_model1 => get_virtual_model(m1_id)
    ! exchange%is_datacopy = .not. exchange%v_model1%is_local
    !
    ! -- set gwfmodel2
    m2_index = model_loc_idx(m2_id)
    if (m2_index > 0) then
      mb => GetBaseModelFromList(basemodellist, m2_index)
      select type (mb)
      type is (GwfModelType)
        ! exchange%model2 => mb
        exchange%gwfmodel2 => mb
      end select
    end if
    ! exchange%v_model2 => get_virtual_model(m2_id)
    !
    ! -- Verify that gwf model1 is of the correct type
    if (.not. associated(exchange%gwfmodel1) .and. m1_index > 0) then
      write (errmsg, '(3a)') 'Problem with SWI-SWI exchange ', &
        trim(exchange%name), &
        '.  First specified GWF Model does not appear to be of the correct type.'
      call store_error(errmsg, terminate=.true.)
    end if
    !
    ! -- Verify that gwf model2 is of the correct type
    if (.not. associated(exchange%gwfmodel2) .and. m2_index > 0) then
      write (errmsg, '(3a)') 'Problem with SWI-SWI exchange ', &
        trim(exchange%name), &
        '.  Second specified GWF Model does not appear to be of the correct type.'
      call store_error(errmsg, terminate=.true.)
    end if
    !
    ! -- Return
    return
  end subroutine swiexchange_create

  !> @ brief Define SWI SWI exchange
  !!
  !! Define SWI to SWI exchange object.
  !<
  subroutine swi_swi_df(this)
    ! -- modules
    use SimVariablesModule, only: iout
    use InputOutputModule, only: getunit, openfile
    use GhostNodeModule, only: gnc_cr
    ! -- dummy
    class(SwiExchangeType) :: this !<  SwiExchangeType
    ! -- local
    !
    ! -- log the exchange
    write (iout, '(/a,a)') ' Creating exchange: ', this%name
    !
    ! -- Ensure models are in same solution
    if (associated(this%gwfmodel1) .and. associated(this%gwfmodel2)) then
      if (this%gwfmodel1%idsoln /= this%gwfmodel2%idsoln) then
        call store_error('Two models are connected in a GWF '// &
                         'exchange but they are in different solutions. '// &
                         'GWF models must be in same solution: '// &
                         trim(this%gwfmodel1%name)//' '// &
                         trim(this%gwfmodel2%name))
        call store_error_filename(this%filename)
      end if
    end if
    !
    ! -- source options
    call this%source_options(iout)
    !
    ! -- source dimensions
    call this%source_dimensions(iout)
    !
    ! -- allocate arrays
    call this%allocate_arrays()
    !
    ! -- source exchange data
    call this%source_data(iout)
    !
    ! -- Return
    return
  end subroutine swi_swi_df

  !> @ brief Add connections
  !!
  !! Override parent exg_ac so that connections can be added here.
  !<
  subroutine swi_swi_ac(this, sparse)
    ! -- modules
    use SparseModule, only: sparsematrix
    ! -- dummy
    class(SwiExchangeType) :: this !<  SwiExchangeType
    type(sparsematrix), intent(inout) :: sparse
    ! -- local
    integer(I4B) :: n, iglo, jglo
    !
    ! -- add exchange connections
    do n = 1, this%nexg
      iglo = this%nodem1(n) + this%gwfmodel1%moffset
      jglo = this%nodem2(n) + this%gwfmodel2%moffset
      call sparse%addconnection(iglo, jglo, 1)
      call sparse%addconnection(jglo, iglo, 1)
    end do
    !
    ! -- Return
    return
  end subroutine swi_swi_ac

  !> @ brief Map connections
  !!
  !! Map the connections in the global matrix
  !<
  subroutine swi_swi_mc(this, matrix_sln)
    ! -- modules
    use SparseModule, only: sparsematrix
    ! -- dummy
    class(SwiExchangeType) :: this !<  SwiExchangeType
    class(MatrixBaseType), pointer :: matrix_sln !< the system matrix
    ! -- local
    integer(I4B) :: n, iglo, jglo
    !
    ! -- map exchange connections
    do n = 1, this%nexg
      iglo = this%nodem1(n) + this%gwfmodel1%moffset
      jglo = this%nodem2(n) + this%gwfmodel2%moffset
      this%idxglo(n) = matrix_sln%get_position(iglo, jglo)
      this%idxsymglo(n) = matrix_sln%get_position(jglo, iglo)
    end do
    !
    ! -- Return
    return
  end subroutine swi_swi_mc

  !> @ brief Allocate and read
  !!
  !! Allocated and read and calculate saturated conductance
  !<
  subroutine swi_swi_ar(this)
    ! -- dummy
    class(SwiExchangeType) :: this !<  SwiExchangeType
    !
    ! -- Return
    return
  end subroutine swi_swi_ar

  !> @ brief Read and prepare
  !!
  !! Read new data for mover and obs
  !<
  subroutine swi_swi_rp(this)
    ! -- modules
    use TdisModule, only: readnewdata
    ! -- dummy
    class(SwiExchangeType) :: this !<  SwiExchangeType
    !
    ! -- Check with TDIS on whether or not it is time to RP
    if (.not. readnewdata) return
    !
    ! -- Return
    return
  end subroutine swi_swi_rp

  !> @ brief Advance
  !!
  !! Advance mover and obs
  !<
  subroutine swi_swi_ad(this)
    ! -- dummy
    class(SwiExchangeType) :: this !<  SwiExchangeType
    !
    ! -- Return
    return
  end subroutine swi_swi_ad

  !> @ brief Calculate coefficients
  !!
  !! 
  !<
  subroutine swi_swi_cf(this, kiter)
    ! -- dummy
    class(SwiExchangeType) :: this !<  SwiExchangeType
    integer(I4B), intent(in) :: kiter
    !
    ! -- Return
    return
  end subroutine swi_swi_cf

  !> @ brief Fill coefficients
  !!
  !! Fill coefficient matrix with cross exchange terms
  !<
  subroutine swi_swi_fc(this, kiter, matrix_sln, rhs_sln, inwtflag)
    ! -- modules
    ! -- dummy
    class(SwiExchangeType) :: this !<  SwiExchangeType
    integer(I4B), intent(in) :: kiter
    class(MatrixBaseType), pointer :: matrix_sln
    real(DP), dimension(:), intent(inout) :: rhs_sln
    integer(I4B), optional, intent(in) :: inwtflag
    ! -- local
    ! integer(I4B) :: inwt, iexg
    ! integer(I4B) :: i, nodem1sln, nodem2sln
    !

    ! -- TODO: Since these terms are all Newton, do they go on the
    !    here in _fc or should we put in _fn
    !  ** this is not true.  they are not all newton terms.  The
    !     storage term for the freshwater model has a saltwater model
    !     dependency and vice versa.

    ! ! -- Put terms into amatsln and rhs
    ! do i = 1, this%nexg
    !   call matrix_sln%set_value_pos(this%idxglo(i), this%cond(i))
    !   call matrix_sln%set_value_pos(this%idxsymglo(i), this%cond(i))

    !   nodem1sln = this%nodem1(i) + this%gwfmodel1%moffset
    !   nodem2sln = this%nodem2(i) + this%gwfmodel2%moffset
    !   call matrix_sln%add_diag_value(nodem1sln, -this%cond(i))
    !   call matrix_sln%add_diag_value(nodem2sln, -this%cond(i))
    ! end do
    ! !
    ! ! -- Set inwt to exchange newton, but shut off if requested by caller
    ! inwt = this%inewton
    ! if (present(inwtflag)) then
    !   if (inwtflag == 0) inwt = 0
    ! end if
    ! if (inwt /= 0) then
    !   call this%exg_fn(kiter, matrix_sln)
    ! end if
    !
    ! -- Return
    return
  end subroutine swi_swi_fc

  !> @ brief Fill Newton
  !!
  !! Fill amatsln with Newton terms
  !<
  subroutine swi_swi_fn(this, kiter, matrix_sln)
    ! -- modules
    use SmoothingModule, only: sQuadraticSaturationDerivative
    ! -- dummy
    class(SwiExchangeType) :: this !<  SwiExchangeType
    integer(I4B), intent(in) :: kiter
    class(MatrixBaseType), pointer :: matrix_sln
    ! -- local
    !
    ! -- Return
    return
  end subroutine swi_swi_fn

  !> @ brief Output
  !!
  !! Write output
  !<
  subroutine swi_swi_ot(this)
    ! -- modules
    ! -- dummy
    class(SwiExchangeType) :: this !<  SwiExchangeType
    ! -- local
    !
    ! -- Return
    return
  end subroutine swi_swi_ot

  !> @ brief Source options
  !!
  !! Source the options block
  !<
  subroutine source_options(this, iout)
    ! -- modules
    use ConstantsModule, only: LENVARNAME, DEM6
    use InputOutputModule, only: getunit, openfile
    use MemoryManagerExtModule, only: mem_set_value
    use CharacterStringModule, only: CharacterStringType
    use ExgSwiswiInputModule, only: ExgSwiswiParamFoundType
    use SourceCommonModule, only: filein_fname
    ! -- dummy
    class(SwiExchangeType) :: this !<  SwiExchangeType
    integer(I4B), intent(in) :: iout
    ! -- local
    type(ExgSwiswiParamFoundType) :: found
    !
    ! -- update defaults with idm sourced values
    call mem_set_value(this%ipr_input, 'IPR_INPUT', this%input_mempath, found%ipr_input)
    call mem_set_value(this%ipr_flow, 'IPR_FLOW', this%input_mempath, found%ipr_flow)
    !
    write (iout, '(1x,a)') 'PROCESSING SWI-SWI EXCHANGE OPTIONS'
    !
    if (found%ipr_input) then
      write (iout, '(4x,a)') &
        'THE LIST OF EXCHANGES WILL BE PRINTED.'
    end if
    !
    if (found%ipr_flow) then
      write (iout, '(4x,a)') &
        'EXCHANGE FLOWS WILL BE PRINTED TO LIST FILES.'
    end if
    !
    write (iout, '(1x,a)') 'END OF SWI-SWI EXCHANGE OPTIONS'
    !
    ! -- Return
    return
  end subroutine source_options

  !> @ brief Source dimensions
  !!
  !! Source the dimensions block
  !<
  subroutine source_dimensions(this, iout)
    ! -- modules
    use ConstantsModule, only: LENVARNAME, DEM6
    use InputOutputModule, only: getunit, openfile
    use MemoryManagerExtModule, only: mem_set_value
    use CharacterStringModule, only: CharacterStringType
    use ExgSwiswiInputModule, only: ExgSwiswiParamFoundType
    use SourceCommonModule, only: filein_fname
    ! -- dummy
    class(SwiExchangeType) :: this !<  SwiExchangeType
    integer(I4B), intent(in) :: iout
    ! -- local
    type(ExgSwiswiParamFoundType) :: found
    !
    ! -- update defaults with idm sourced values
    call mem_set_value(this%nexg, 'NEXG', this%input_mempath, found%nexg)
    !
    write (iout, '(1x,a)') 'PROCESSING SWI-SWI EXCHANGE DIMENSIONS'
    !
    if (found%nexg) then
      write (iout, '(4x,a,i0)') &
        'Number of exchanges (NEXG) set to', this%nexg
    else
      write (errmsg, '(a)') 'Error in DIMENSIONS block: NEXG not found.'
      call store_error(errmsg, terminate=.false.)
      call store_error_filename(this%filename)
    end if
    !
    write (iout, '(1x,a)') 'END OF SWI-SWI EXCHANGE DIMENSIONS'
    !
    ! -- Return
    return
  end subroutine source_dimensions

  !> @brief Source exchange data from input context
  !<
  subroutine source_data(this, iout)
    ! -- modules
    use MemoryManagerModule, only: mem_setptr
    ! -- dummy
    class(SwiExchangeType) :: this !< instance of exchange object
    integer(I4B), intent(in) :: iout !< the output file unit
    ! -- local
    integer(I4B), dimension(:, :), contiguous, pointer :: cellidm1
    integer(I4B), dimension(:, :), contiguous, pointer :: cellidm2
    real(DP), dimension(:), contiguous, pointer :: amat_fresh
    real(DP), dimension(:), contiguous, pointer :: rhs_fresh
    real(DP), dimension(:), contiguous, pointer :: amat_salt
    real(DP), dimension(:), contiguous, pointer :: rhs_salt
    character(len=20) :: cellstr1, cellstr2
    integer(I4B) :: nerr
    integer(I4B) :: iexg, nodem1, nodem2
    ! -- format
    character(len=*), parameter :: fmtexglabel = "(1x, 3a10, 50(a16))"
    character(len=*), parameter :: fmtexgdata = &
                                   "(5x, a, 1x, a ,I10, 50(1pg16.6))"
    !
    call mem_setptr(cellidm1, 'CELLIDM1', this%input_mempath)
    call mem_setptr(cellidm2, 'CELLIDM2', this%input_mempath)
    call mem_setptr(amat_fresh, 'AMAT_FRESH', this%input_mempath)
    call mem_setptr(rhs_fresh, 'RHS_FRESH', this%input_mempath)
    call mem_setptr(amat_salt, 'AMAT_SALT', this%input_mempath)
    call mem_setptr(rhs_salt, 'RHS_SALT', this%input_mempath)
    !
    write (iout, '(1x,a)') 'PROCESSING EXCHANGEDATA'
    !
    if (this%ipr_input /= 0) then
      write (iout, fmtexglabel) 'NODEM1', 'NODEM2', 'AMAT_FRESH', &
        'RHS_FRESH', 'AMAT_SALT', 'RHS_SALT'
    end if
    !
    do iexg = 1, this%nexg
      !
      if (associated(this%gwfmodel1)) then
        !
        ! -- Determine user node number
        nodem1 = this%noder(this%gwfmodel1, cellidm1(:, iexg), iout)
        this%nodem1(iexg) = nodem1
        !
      else
        this%nodem1(iexg) = -1
      end if
      !
      if (associated(this%gwfmodel2)) then
        !
        ! -- Determine user node number
        nodem2 = this%noder(this%gwfmodel2, cellidm2(:, iexg), iout)
        this%nodem2(iexg) = nodem2
        !
      else
        this%nodem2(iexg) = -1
      end if
      !
      ! -- Read rest of input line
      this%amat_fresh(iexg) = amat_fresh(iexg)
      this%rhs_fresh(iexg) = rhs_fresh(iexg)
      this%amat_salt(iexg) = amat_salt(iexg)
      this%rhs_salt(iexg) = rhs_salt(iexg)
      !
      ! -- Write the data to listing file if requested
      if (this%ipr_input /= 0) then
        cellstr1 = this%cellstr(this%gwfmodel1, cellidm1(:, iexg), iout)
        cellstr2 = this%cellstr(this%gwfmodel2, cellidm2(:, iexg), iout)
        write (iout, fmtexgdata) trim(cellstr1), trim(cellstr2), &
          this%amat_fresh(iexg), this%rhs_fresh(iexg), &
          this%amat_salt(iexg), this%rhs_salt(iexg)
      end if
      !
      ! -- Check to see if nodem1 is outside of active domain
      if (associated(this%gwfmodel1)) then
        if (nodem1 <= 0) then
          cellstr1 = this%cellstr(this%gwfmodel1, cellidm1(:, iexg), iout)
          write (errmsg, *) &
            trim(adjustl(this%gwfmodel1%name))// &
            ' Cell is outside active grid domain ('// &
            trim(adjustl(cellstr1))//').'
          call store_error(errmsg)
        end if
      end if
      !
      ! -- Check to see if nodem2 is outside of active domain
      if (associated(this%gwfmodel2)) then
        if (nodem2 <= 0) then
          cellstr2 = this%cellstr(this%gwfmodel2, cellidm2(:, iexg), iout)
          write (errmsg, *) &
            trim(adjustl(this%gwfmodel2%name))// &
            ' Cell is outside active grid domain ('// &
            trim(adjustl(cellstr2))//').'
          call store_error(errmsg)
        end if
      end if
    end do
    !
    write (iout, '(1x,a)') 'END OF EXCHANGEDATA'
    !
    ! -- Stop if errors
    nerr = count_errors()
    if (nerr > 0) then
      call store_error('Errors encountered in exchange input file.')
      call store_error_filename(this%filename)
    end if
    !
    ! -- Return
    return
  end subroutine source_data

  !> @ brief Allocate scalars
  !!
  !! Allocate scalar variables
  !<
  subroutine allocate_scalars(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(SwiExchangeType) :: this !<  SwiExchangeType
    !
    allocate (this%filename)
    this%filename = ''
    !
    call mem_allocate(this%ipr_input, 'IPR_INPUT', this%memoryPath)
    call mem_allocate(this%ipr_flow, 'IPR_FLOW', this%memoryPath)
    call mem_allocate(this%nexg, 'NEXG', this%memoryPath)
    !
    this%ipr_input = 0
    this%ipr_flow = 0
    this%nexg = 0
    !
    ! -- Return
    return
  end subroutine allocate_scalars

  !> @ brief Deallocate
  !!
  !! Deallocate memory associated with this object
  !<
  subroutine swi_swi_da(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(SwiExchangeType) :: this !<  SwiExchangeType
    !
    ! -- arrays
    call mem_deallocate(this%amat_fresh)
    call mem_deallocate(this%rhs_fresh)
    call mem_deallocate(this%amat_salt)
    call mem_deallocate(this%rhs_salt)
    call mem_deallocate(this%idxglo)
    call mem_deallocate(this%idxsymglo)
    call mem_deallocate(this%simvals)
    !
    ! -- scalars
    deallocate (this%filename)
    !
    call mem_deallocate(this%ipr_input)
    call mem_deallocate(this%ipr_flow)
    call mem_deallocate(this%nexg)
    !
    ! -- Return
    return
  end subroutine swi_swi_da

  !> @ brief Allocate arrays
  !!
  !! Allocate arrays
  !<
  subroutine allocate_arrays(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(SwiExchangeType) :: this !<  SwiExchangeType
    ! -- local
    integer(I4B) :: i
    !
    call mem_allocate(this%nodem1, this%nexg, 'NODEM1', this%memoryPath)
    call mem_allocate(this%nodem2, this%nexg, 'NODEM2', this%memoryPath)
    call mem_allocate(this%amat_fresh, this%nexg, 'AMAT_FRESH', this%memoryPath)
    call mem_allocate(this%rhs_fresh, this%nexg, 'RHS_FRESH', this%memoryPath)
    call mem_allocate(this%amat_salt, this%nexg, 'AMAT_SALT', this%memoryPath)
    call mem_allocate(this%rhs_salt, this%nexg, 'RHS_SALT', this%memoryPath)
    call mem_allocate(this%idxglo, this%nexg, 'IDXGLO', this%memoryPath)
    call mem_allocate(this%idxsymglo, this%nexg, 'IDXSYMGLO', this%memoryPath)
    call mem_allocate(this%simvals, this%nexg, 'SIMVALS', this%memoryPath)
    !
    ! -- Initialize
    do i = 1, this%nexg
      this%amat_fresh(i) = DNODATA
      this%rhs_fresh(i) = DNODATA
      this%amat_salt(i) = DNODATA
      this%rhs_salt(i) = DNODATA
    end do
    !
    ! -- Return
    return
  end subroutine allocate_arrays

  !> @ brief Final processing
  !!
  !! Conduct any final processing
  !<
  subroutine swi_swi_fp(this)
    ! -- dummy
    class(SwiExchangeType) :: this !<  SwiExchangeType
    !
    ! -- Return
    return
  end subroutine swi_swi_fp

  !> @ brief Set symmetric flag
  !!
  !! Return flag indicating whether or not this exchange will cause the
  !! coefficient matrix to be asymmetric.
  !<
  function swi_swi_get_iasym(this) result(iasym)
    ! -- dummy
    class(SwiExchangeType) :: this !<  SwiExchangeType
    ! -- local
    integer(I4B) :: iasym
    !
    ! -- Newton formulation results in asymmetric matrix
    iasym = 1
    !
    ! -- Return
    return
  end function swi_swi_get_iasym

  !> @brief
  !<
  function noder(this, model, cellid, iout)
    ! -- modules
    use GeomUtilModule, only: get_node
    ! -- dummy
    class(SwiExchangeType) :: this !< instance of exchange object
    class(GwfModelType), pointer, intent(in) :: model
    integer(I4B), dimension(:), pointer, intent(in) :: cellid
    integer(I4B), intent(in) :: iout !< the output file unit
    integer(I4B) :: noder, node
    !
    if (model%dis%ndim == 1) then
      node = cellid(1)
    elseif (model%dis%ndim == 2) then
      node = get_node(cellid(1), 1, cellid(2), &
                      model%dis%mshape(1), 1, &
                      model%dis%mshape(2))
    else
      node = get_node(cellid(1), cellid(2), cellid(3), &
                      model%dis%mshape(1), &
                      model%dis%mshape(2), &
                      model%dis%mshape(3))
    end if
    noder = model%dis%get_nodenumber(node, 0)
    !
    ! -- return
    return
  end function noder

  !> @brief
  !<
  function cellstr(this, model, cellid, iout)
    ! -- modules
    ! -- dummy
    class(SwiExchangeType) :: this !< instance of exchange object
    class(GwfModelType), pointer, intent(in) :: model
    integer(I4B), dimension(:), pointer, intent(in) :: cellid
    integer(I4B), intent(in) :: iout !< the output file unit
    character(len=20) :: cellstr
    character(len=*), parameter :: fmtndim1 = &
                                   "('(',i0,')')"
    character(len=*), parameter :: fmtndim2 = &
                                   "('(',i0,',',i0,')')"
    character(len=*), parameter :: fmtndim3 = &
                                   "('(',i0,',',i0,',',i0,')')"
    !
    cellstr = ''
    !
    select case (model%dis%ndim)
    case (1)
      write (cellstr, fmtndim1) cellid(1)
    case (2)
      write (cellstr, fmtndim2) cellid(1), cellid(2)
    case (3)
      write (cellstr, fmtndim3) cellid(1), cellid(2), cellid(3)
    case default
    end select
    !
    ! -- return
    return
  end function cellstr

  !> @ brief Cast polymorphic object as exchange
  !!
  !! Cast polymorphic object as exchange
  !<
  function CastAsSwiExchange(obj) result(res)
    implicit none
    ! -- dummy
    class(*), pointer, intent(inout) :: obj
    ! -- return
    class(SwiExchangeType), pointer :: res
    !
    res => null()
    if (.not. associated(obj)) return
    !
    select type (obj)
    class is (SwiExchangeType)
      res => obj
    end select
    !
    ! -- Return
    return
  end function CastAsSwiExchange

  !> @ brief Get exchange from list
  !!
  !! Return an exchange from the list for specified index
  !<
  function GetSwiExchangeFromList(list, idx) result(res)
    implicit none
    ! -- dummy
    type(ListType), intent(inout) :: list
    integer(I4B), intent(in) :: idx
    ! -- return
    class(SwiExchangeType), pointer :: res
    ! -- local
    class(*), pointer :: obj
    !
    obj => list%GetItem(idx)
    res => CastAsSwiExchange(obj)
    !
    ! -- Return
    return
  end function GetSwiExchangeFromList

end module SwiExchangeModule


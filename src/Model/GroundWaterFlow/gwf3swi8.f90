module GwfSwiModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH, DONE, DZERO, LENBUDTXT
  use SimVariablesModule, only: errmsg
  use SimModule, only: store_error
  use NumericalPackageModule, only: NumericalPackageType
  use BlockParserModule, only: BlockParserType
  use BaseDisModule, only: DisBaseType
  use MemoryManagerModule, only: mem_setptr
  use MemoryHelperModule, only: create_mem_path
  use MatrixBaseModule

  implicit none
  private
  public :: GwfSwiType
  public :: swi_cr

  character(len=LENBUDTXT), dimension(1) :: budtxt = & !< text labels for budget terms
    &['         STORAGE']

  type, extends(NumericalPackageType) :: GwfSwiType

    real(DP), dimension(:), pointer, contiguous :: zeta => null() ! starting head
    real(DP), dimension(:), pointer, contiguous :: hcof => null() !< hcof contribution to amat
    real(DP), dimension(:), pointer, contiguous :: rhs => null() !< rhs contribution
    real(DP), dimension(:), pointer, contiguous :: storage => null() !< calculated swi storage

  contains

    procedure :: swi_df
    procedure :: swi_ar
    procedure :: swi_fc
    procedure :: swi_cq
    procedure :: swi_bd
    procedure :: swi_save_model_flows
    procedure :: swi_da
    procedure, private :: swi_load
    procedure, private :: allocate_arrays
    procedure, private :: source_griddata

  end type GwfSwiType

contains

  !> @brief Create a new swi package object
  !<
  subroutine swi_cr(swi, name_model, input_mempath, inunit, iout, dis)
    ! -- modules
    use MemoryManagerExtModule, only: mem_set_value
    ! -- dummy
    type(GwfSwiType), pointer :: swi
    character(len=*), intent(in) :: name_model
    character(len=*), intent(in) :: input_mempath
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    class(DisBaseType), pointer, intent(in) :: dis
    ! -- formats
    character(len=*), parameter :: fmtswi = &
      "(1x, /1x, 'SWI -- Seawater Intrusion Package, Version 8, 1/8/2024', &
      &' input read from mempath: ', A, //)"
    !
    ! -- create SWI object
    allocate (swi)
    !
    ! -- create name and memory path
    call swi%set_names(1, name_model, 'SWI', 'SWI', input_mempath)
    !
    ! -- allocate scalars
    call swi%allocate_scalars()
    !
    ! -- set variables
    swi%inunit = inunit
    swi%iout = iout
    !
    ! -- set pointers
    swi%dis => dis
    !
    ! -- check if pkg is enabled,
    if (inunit > 0) then
      ! print message identifying pkg
      write (swi%iout, fmtswi) input_mempath
    end if
    !
    ! -- return
    return
  end subroutine swi_cr

  !> @brief Allocate arrays, load from IDM, and assign head
  !<
  subroutine swi_df(this)
    ! -- dummy
    class(GwfSwiType) :: this
    ! -- local
    !
    ! -- allocate arrays
    call this%allocate_arrays(this%dis%nodes)
    !
    ! -- load from IDM
    call this%swi_load()
    !
    ! -- return
    return
  end subroutine swi_df

  !> @brief Allocate arrays, load from IDM, and assign head
  !<
  subroutine swi_ar(this)
    ! -- dummy
    class(GwfSwiType) :: this
    ! -- local
    !
    ! -- return
    return
  end subroutine swi_ar

  !> @ brief Fill A and right-hand side for the package
  !!
  !!  Fill the coefficient matrix and right-hand side
  !!
  !<
  subroutine swi_fc(this, kiter, hold, hnew, matrix_sln, idxglo, rhs)
    ! -- modules
    ! -- dummy variables
    class(GwfSwiType) :: this !< GwfSwiType object
    integer(I4B), intent(in) :: kiter !< outer iteration number
    real(DP), intent(in), dimension(:) :: hold !< previous heads
    real(DP), intent(in), dimension(:) :: hnew !< current heads
    class(MatrixBaseType), pointer :: matrix_sln !< A matrix
    integer(I4B), intent(in), dimension(:) :: idxglo !< global index model to solution
    real(DP), intent(inout), dimension(:) :: rhs !< right-hand side
    ! -- local variables
    integer(I4B) :: n
    integer(I4B) :: idiag
    ! -- formats
    !
    ! -- Add hcof and rhs terms
    do n = 1, this%dis%nodes
      idiag = this%dis%con%ia(n)
      call matrix_sln%add_value_pos(idxglo(idiag), this%hcof(n))
      rhs(n) = rhs(n) + this%rhs(n)
    end do
    !
    ! -- return
    return
  end subroutine swi_fc

  !> @ brief Calculate flows for package
  !!
  !!  Flow calculation for the STO package components. Components include
  !!  specific storage and specific yield storage.
  !!
  !<
  subroutine swi_cq(this, flowja, hnew, hold)
    ! -- modules
    ! -- dummy variables
    class(GwfSwiType) :: this !< GwfStoType object
    real(DP), dimension(:), contiguous, intent(inout) :: flowja !< connection flows
    real(DP), dimension(:), contiguous, intent(in) :: hnew !< current head
    real(DP), dimension(:), contiguous, intent(in) :: hold !< previous head
    ! -- local variables
    integer(I4B) :: n
    integer(I4B) :: idiag
    real(DP) :: rate
    !
    ! -- initialize strg arrays
    do n = 1, this%dis%nodes
      this%storage(n) = DZERO
    end do
    !
    ! Loop through cells
    do n = 1, this%dis%nodes
      !
      ! -- Calculate change in freshwater storage
      rate = this%hcof(n) * hnew(n) - this%rhs(n)
      this%storage(n) = rate
      !
      ! -- Add storage term to flowja
      idiag = this%dis%con%ia(n)
      flowja(idiag) = flowja(idiag) + rate
    end do
    !
    ! -- return
    return
  end subroutine swi_cq

  !> @ brief Model budget calculation for package
  !!
  !!  Budget calculation for the STO package components. Components include
  !!  specific storage and specific yield storage.
  !!
  !<
  subroutine swi_bd(this, isuppress_output, model_budget)
    ! -- modules
    use TdisModule, only: delt
    use BudgetModule, only: BudgetType, rate_accumulator
    ! -- dummy variables
    class(GwfSwiType) :: this !< GwfSwiType object
    integer(I4B), intent(in) :: isuppress_output !< flag to suppress model output
    type(BudgetType), intent(inout) :: model_budget !< model budget object
    ! -- local variables
    real(DP) :: rin
    real(DP) :: rout
    !
    ! -- Add swi storage rates to model budget
    call rate_accumulator(this%storage, rin, rout)
    call model_budget%addentry(rin, rout, delt, budtxt(1), &
                               isuppress_output, '     SWI')
    !
    ! -- return
    return
  end subroutine swi_bd

  !> @ brief Save model flows for package
  !!
  !!  Save cell-by-cell budget terms for the STO package.
  !!
  !<
  subroutine swi_save_model_flows(this, icbcfl, icbcun)
    ! -- dummy variables
    class(GwfSwiType) :: this !< GwfSwiType object
    integer(I4B), intent(in) :: icbcfl !< flag to output budget data
    integer(I4B), intent(in) :: icbcun !< cell-by-cell file unit number
    ! -- local variables
    integer(I4B) :: ibinun
    integer(I4B) :: iprint, nvaluesp, nwidthp
    character(len=1) :: cdatafmp = ' ', editdesc = ' '
    real(DP) :: dinact
    !
    ! -- Set unit number for binary output
    if (this%ipakcb < 0) then
      ibinun = icbcun
    elseif (this%ipakcb == 0) then
      ibinun = 0
    else
      ibinun = this%ipakcb
    end if
    if (icbcfl == 0) ibinun = 0
    !
    ! -- Record the storage rates if requested
    if (ibinun /= 0) then
      iprint = 0
      dinact = DZERO
      !
      ! -- swi storage
      call this%dis%record_array(this%storage, this%iout, iprint, -ibinun, &
                                 budtxt(1), cdatafmp, nvaluesp, &
                                 nwidthp, editdesc, dinact)
    end if
    !
    ! -- return
    return
  end subroutine swi_save_model_flows

  !> @brief Deallocate
  !<
  subroutine swi_da(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    use MemoryManagerExtModule, only: memorylist_remove
    use SimVariablesModule, only: idm_context
    ! -- dummy
    class(GwfSwiType) :: this
    !
    ! -- deallocate IDM memory
    call memorylist_remove(this%name_model, 'SWI', idm_context)
    !
    ! -- deallocate arrays
    call mem_deallocate(this%zeta)
    call mem_deallocate(this%hcof)
    call mem_deallocate(this%rhs)
    call mem_deallocate(this%storage)
    !
    ! -- deallocate parent
    call this%NumericalPackageType%da()
    !
    ! -- return
    return
  end subroutine swi_da

  !> @brief Load data from IDM into package
  !<
  subroutine swi_load(this)
    ! -- modules
    use BaseDisModule, only: DisBaseType
    ! -- dummy
    class(GwfSwiType) :: this
    !
    call this%source_griddata()
    !
    ! -- return
    return
  end subroutine swi_load

  !> @brief Allocate arrays
  !<
  subroutine allocate_arrays(this, nodes)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwfSwiType) :: this
    integer(I4B), intent(in) :: nodes
    ! -- local
    integer(I4B) :: n
    !
    ! -- Allocate
    call mem_allocate(this%zeta, nodes, 'ZETA', this%memoryPath)
    call mem_allocate(this%hcof, nodes, 'HCOF', this%memoryPath)
    call mem_allocate(this%rhs, nodes, 'RHS', this%memoryPath)
    call mem_allocate(this%storage, nodes, 'STORAGE', this%memoryPath)
    !
    ! -- initialize
    do n = 1, nodes
      this%zeta(n) = DZERO
      this%hcof(n) = DZERO
      this%rhs(n) = DZERO
      this%storage(n) = DZERO
    end do
    !
    ! -- return
    return
  end subroutine allocate_arrays

  !> @brief Copy grid data from IDM into package
  !<
  subroutine source_griddata(this)
    ! -- modules
    use SimModule, only: store_error, store_error_filename
    use MemoryManagerExtModule, only: mem_set_value
    use GwfSwiInputModule, only: GwfSwiParamFoundType
    ! -- dummy
    class(GwfSwiType) :: this
    ! -- local
    type(GwfSwiParamFoundType) :: found
    integer(I4B), dimension(:), pointer, contiguous :: map
    !
    ! -- set map to convert user to reduced node data
    map => null()
    if (this%dis%nodes < this%dis%nodesuser) map => this%dis%nodeuser
    !
    ! -- set values
    call mem_set_value(this%zeta, 'ZETA', this%input_mempath, map, found%zeta)
    !
    ! -- ensure ZETA was found
    if (.not. found%zeta) then
      write (errmsg, '(a)') 'Error in GRIDDATA block: ZETA not found.'
      call store_error(errmsg, terminate=.false.)
      call store_error_filename(this%input_fname)
    else if (this%iout > 0) then
      write (this%iout, '(4x,a)') 'ZETA set from input file'
    end if
    !
    ! -- return
    return
  end subroutine source_griddata

end module GwfSwiModule

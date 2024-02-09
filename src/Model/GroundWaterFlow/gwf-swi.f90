module GwfSwiModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH, DONE, DZERO, LENBUDTXT, &
                             MNORMAL, DHNOFLO
  use SimVariablesModule, only: errmsg
  use SimModule, only: store_error
  use NumericalPackageModule, only: NumericalPackageType
  use BlockParserModule, only: BlockParserType
  use BaseDisModule, only: DisBaseType
  use MemoryManagerModule, only: mem_setptr, mem_allocate
  use MemoryHelperModule, only: create_mem_path
  use MatrixBaseModule

  implicit none
  private
  public :: GwfSwiType
  public :: swi_cr

  character(len=LENBUDTXT), dimension(1) :: budtxt = & !< text labels for budget terms
    &['         STORAGE']

  type, extends(NumericalPackageType) :: GwfSwiType

    integer(I4B), pointer :: iuseapi => null() !< 1 is flag to indicate terms will be filled by api
    integer(I4B), pointer :: isaltwater => null() !< 0 is freshwater, 1 is saltwater
    integer(I4B), pointer :: izetaout => null() !< unit number for binary zeta output file
    real(DP), dimension(:), pointer, contiguous :: zeta => null() !< starting zeta
    real(DP), dimension(:), pointer, contiguous :: hcof => null() !< hcof contribution to amat
    real(DP), dimension(:), pointer, contiguous :: rhs => null() !< rhs contribution
    real(DP), dimension(:), pointer, contiguous :: storage => null() !< calculated swi storage

    ! information needed for full implementation
    real(DP), pointer :: alphaf => null() !< rhof / (rhos - rhof), default is 40
    real(DP), pointer :: alphas => null() !< rhos / (rhos - rhof), default is 41
    real(DP), dimension(:), pointer, contiguous :: hfresh => null() !< head of freshwater
    real(DP), dimension(:), pointer, contiguous :: hsalt => null() !< head of saltwater
    integer(I4B), dimension(:), pointer, contiguous :: ibound => null() !< pointer to model ibound

    ! pointers for transient simulations
    integer(I4B), pointer :: insto => null() !< pointer to check of storage package is on
    integer(I4B), pointer :: iss => null() !< pointer to gwf steady state flag
    real(DP), dimension(:), pointer, contiguous :: sy => null() !< pointer to storage package specific yield

  contains

    procedure :: swi_df
    procedure :: swi_ar
    procedure :: swi_fc
    procedure :: swi_cc
    procedure :: swi_cq
    procedure :: swi_bd
    procedure :: swi_save_model_flows
    procedure :: swi_ot_dv
    procedure :: swi_da
    procedure, private :: swi_load
    procedure, private :: source_options
    procedure, private :: log_options
    procedure :: allocate_scalars
    procedure, private :: allocate_arrays
    procedure, private :: source_griddata
    procedure, private :: update_zeta
    procedure, private :: swi_fc_freshstorage

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

  !> @brief Setup pointers
  !<
  subroutine swi_ar(this, ibound)
    ! -- dummy
    class(GwfSwiType) :: this
    integer(I4B), dimension(:), pointer, contiguous :: ibound !< model ibound
    ! -- local
    !
    ! set pointer to ibound
    this%ibound => ibound
    !
    ! -- set pointer to gwf steady state flag
    call mem_setptr(this%insto, 'INSTO', &
                    create_mem_path(this%name_model))
    call mem_setptr(this%iss, 'ISS', &
                    create_mem_path(this%name_model))
    if (this%insto > 0) then
      call mem_setptr(this%sy, 'SY', &
                      create_mem_path(this%name_model, 'STO'))
    end if
    !
    ! -- If freshwater model, point effective_bot to zeta; otherwise
    !    if a saltwater model, then point effective_top to zeta
    if (this%isaltwater == 0) then
      ! -- set hfresh to model x
      call mem_setptr(this%hfresh, 'X', &
                      create_mem_path(this%name_model))
    else
      ! -- set hsalt to model x
      call mem_setptr(this%hsalt, 'X', &
                      create_mem_path(this%name_model))
    end if
    !
    ! -- update zeta
    if (this%iuseapi == 0) then
      call this%update_zeta()
    end if
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
    ! -- test if steady-state stress period
    if (this%iss /= 0) return
    !
    ! -- Calculate fresh storage terms and put in hcof/rhs
    if (this%iuseapi == 0) then
      if (this%isaltwater == 0) then
        call this%swi_fc_freshstorage(kiter, hold, hnew, matrix_sln, idxglo, rhs)
      end if
    end if
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

  !> @ brief Calculate fresh storage terms
  !<
  subroutine swi_fc_freshstorage(this, kiter, hold, hnew, matrix_sln, idxglo, rhs)
    ! -- modules
    use TdisModule, only: delt
    !use GwfStorageUtilsModule, only: SsCapacity, SyCapacity, SsTerms, SyTerms
    use GwfStorageUtilsModule, only: SyCapacity
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
    real(DP) :: tled
    real(DP) :: sc2
    real(DP) :: rho2
    real(DP) :: zetanew
    real(DP) :: zetaold
    !
    ! -- set variables
    tled = DONE / delt
    !
    ! -- Calculate storage terms for freshwater model
    do n = 1, this%dis%nodes
 
      ! initialize
      this%hcof(n) = DZERO
      this%rhs(n) = DZERO
  
      if (this%ibound(n) < 1) cycle

      ! -- calculate zetanew and zetaold
      zetanew = calc_zeta(this%alphaf, hnew(n))
      zetaold = calc_zeta(this%alphaf, hold(n))
      sc2 = SyCapacity(this%dis%area(n), this%sy(n))
      rho2 = - this%alphaf * sc2 * tled

      if (zetanew > this%dis%bot(n) .and. zetaold > this%dis%bot(n)) then
        ! new and old zeta above bottom
        this%hcof(n) = rho2
        this%rhs(n) = rho2 * hold(n)
      else if (zetanew > this%dis%bot(n) .and. zetaold < this%dis%bot(n)) then
        ! zetanew above bottom but zetaold is not
        this%hcof(n) = rho2
        this%rhs(n) = - rho2 * this%dis%bot(n) / this%alphaf
      else if (zetanew < this%dis%bot(n) .and. zetaold > this%dis%bot(n)) then
        ! zetanew is below bottom, zetaold above bottom
        this%hcof(n) = DZERO
        this%rhs(n) = rho2 * (this%dis%bot(n) / this%alphaf + hold(n))
      end if

    end do
    !
    ! -- return
    return
  end subroutine swi_fc_freshstorage

  !> @brief convergence check
  !<
  subroutine swi_cc(this)
    ! -- dummy
    class(GwfSwiType) :: this
    ! -- local
    !
    ! -- recalculate zeta
    if (this%iuseapi == 0) then
      call this%update_zeta()
    end if
    !
    ! -- return
    return
  end subroutine swi_cc

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

  !> @brief Save density array to binary file
  !<
  subroutine swi_ot_dv(this, idvfl)
    ! -- dummy
    class(GwfSwiType) :: this
    integer(I4B), intent(in) :: idvfl
    ! -- local
    character(len=1) :: cdatafmp = ' ', editdesc = ' '
    integer(I4B) :: ibinun
    integer(I4B) :: iprint
    integer(I4B) :: nvaluesp
    integer(I4B) :: nwidthp
    real(DP) :: dinact
    !
    ! -- Set unit number for density output
    if (this%izetaout /= 0) then
      ibinun = 1
    else
      ibinun = 0
    end if
    if (idvfl == 0) ibinun = 0
    !
    ! -- save density array
    if (ibinun /= 0) then
      iprint = 0
      dinact = DHNOFLO
      !
      ! -- write density to binary file
      if (this%izetaout /= 0) then
        ibinun = this%izetaout
        call this%dis%record_array(this%zeta, this%iout, iprint, ibinun, &
                                   '            ZETA', cdatafmp, nvaluesp, &
                                   nwidthp, editdesc, dinact)
      end if
    end if
    !
    ! -- Return
    return
  end subroutine swi_ot_dv

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
    ! -- deallocate scalars
    call mem_deallocate(this%iuseapi)
    call mem_deallocate(this%isaltwater)
    call mem_deallocate(this%izetaout)
    call mem_deallocate(this%alphaf)
    call mem_deallocate(this%alphas)
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
    ! -- dummy
    class(GwfSwiType) :: this
    !
    call this%source_options()
    call this%source_griddata()
    !
    ! -- return
    return
  end subroutine swi_load

  !> @ brief Allocate scalars
  !!
  !! Allocate and initialize scalars for the VSC package. The base model
  !! allocate scalars method is also called.
  !<
  subroutine allocate_scalars(this)
    ! -- modules
    ! -- dummy
    class(GwfSwiType) :: this
    !
    ! -- allocate scalars in NumericalPackageType
    call this%NumericalPackageType%allocate_scalars()
    !
    ! -- Allocate scalars
    call mem_allocate(this%iuseapi, 'IUSEAPI', this%memoryPath)
    call mem_allocate(this%isaltwater, 'ISALTWATER', this%memoryPath)
    call mem_allocate(this%izetaout, 'IZETAOUT', this%memoryPath)
    call mem_allocate(this%alphaf, 'ALPHAF', this%memoryPath)
    call mem_allocate(this%alphas, 'ALPHAS', this%memoryPath)
    !
    ! -- Initialize value
    this%iuseapi = 0
    this%isaltwater = 0
    this%izetaout = 0
    this%alphaf = 40.D0
    this%alphas = 41.D0
    !
    ! -- Return
    return
  end subroutine allocate_scalars

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

  !> @brief Update simulation options from input mempath
  !<
  subroutine source_options(this)
    ! -- modules
    use SimModule, only: store_error, store_error_filename
    use OpenSpecModule, only: access, form
    use InputOutputModule, only: getunit, openfile
    use MemoryManagerModule, only: mem_setptr, get_isize
    use MemoryManagerExtModule, only: mem_set_value
    use CharacterStringModule, only: CharacterStringType
    use GwfSwiInputModule, only: GwfSwiParamFoundType
    ! -- dummy
    class(GwfSwiType) :: this
    character(len=LINELENGTH) :: zeta_fname
    ! -- locals
    type(GwfSwiParamFoundType) :: found
    !
    ! -- update defaults with idm sourced values
    zeta_fname = ''
    call mem_set_value(this%isaltwater, 'ISALTWATER', this%input_mempath, found%isaltwater)
    call mem_set_value(zeta_fname, 'ZETAFILE', this%input_mempath, found%zetafile)
    !
    ! -- open zeta file
    if (zeta_fname /= '') then
      this%izetaout = getunit()
      call openfile(this%izetaout, this%iout, zeta_fname, 'DATA(BINARY)', &
                    form, access, 'REPLACE', MNORMAL)
      write (this%iout, '(4x,a)') &
        'ZETA information will be written to ', trim(zeta_fname)
    end if
    !
    ! -- log options
    if (this%iout > 0) then
      call this%log_options(found)
    end if
    !
    ! -- Return
    return
  end subroutine source_options

  !> @brief Log options sourced from the input mempath
  !<
  subroutine log_options(this, found)
    ! -- modules
    use KindModule, only: LGP
    use GwfSwiInputModule, only: GwfSwiParamFoundType
    ! -- dummy
    class(GwfSwiType) :: this
    ! -- locals
    type(GwfSwiParamFoundType), intent(in) :: found
    !
    write (this%iout, '(1x,a)') 'Setting SWI Options'
    if (found%isaltwater) &
      write (this%iout, '(4x,a)') 'This model has been designated as a &
                                  &saltwater model.'
    write (this%iout, '(1x,a,/)') 'End Setting SWI Options'
    !
    ! -- Return
    return
  end subroutine log_options

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
    call mem_set_value(this%zeta, 'ZETASTRT', this%input_mempath, map, &
                       found%zetastrt)
    !
    ! -- ensure ZETASTRT was found
    if (.not. found%zetastrt) then
      write (errmsg, '(a)') 'Error in GRIDDATA block: ZETASTRT not found.'
      call store_error(errmsg, terminate=.false.)
      call store_error_filename(this%input_fname)
    else if (this%iout > 0) then
      write (this%iout, '(4x,a)') 'ZETASTRT set from input file'
    end if
    !
    ! -- return
    return
  end subroutine source_griddata

  !> @brief Calculate zeta surface
  !<
  subroutine update_zeta(this)
    ! -- modules
    ! -- dummy
    class(GwfSwiType) :: this
    ! -- locals
    integer(I4B) :: n
    logical :: hs_avail
    !
    ! -- Check if there is a saltwater model, and use hsalt
    !    otherwise, assume hsalt is zero.
    hs_avail = associated(this%hsalt)
    !
    ! -- Loop through each node and calculate zeta
    do n = 1, this%dis%nodes
      ! -- skip if inactive
      if (this%ibound(n) == 0) cycle
      !
      ! -- Calculate zeta
      if (hs_avail) then
        this%zeta(n) = calc_zeta(this%alphaf, this%hfresh(n), this%alphas, this%hsalt(n))
      else
        this%zeta(n) = calc_zeta(this%alphaf, this%hfresh(n))
      end if
      !
      ! -- todo: Do we want to constrain zeta to top and
      !    bot of cell?
      if (this%zeta(n) > this%dis%top(n)) then
        this%zeta(n) = this%dis%top(n)
      end if
      if (this%zeta(n) < this%dis%bot(n)) then
        this%zeta(n) = this%dis%bot(n)
      end if
      !
    end do
    !
    ! -- Return
    return
  end subroutine update_zeta

  function calc_zeta(alphaf, hf, alphas, hs) result(zeta)
    real(DP), intent(in) :: alphaf
    real(DP), intent(in) :: hf
    real(DP), intent(in), optional :: alphas
    real(DP), intent(in), optional :: hs
    real(DP) :: zeta
    zeta = - alphaf * hf
    if (present(alphas) .and. present(hs)) then
      zeta = zeta + alphas * hs
    end if
    return
  end function calc_zeta

end module GwfSwiModule

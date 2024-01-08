module GwfSwiModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH, DONE, DZERO
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

  type, extends(NumericalPackageType) :: GwfSwiType

    integer(I4B), pointer :: iss => null() !< steady state flag: 1 = steady, 0 = transient
    integer(I4B), pointer :: insto => null() !< flag to indicate storage package is active
    real(DP), dimension(:), pointer, contiguous :: zeta => null() ! starting head

  contains

    procedure :: swi_df
    procedure :: swi_ar
    procedure :: swi_fc
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
    ! -- set pointer to gwf insto
    call mem_setptr(swi%insto, 'INSTO', create_mem_path(swi%name_model))
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
  end subroutine swi_df

  !> @brief Allocate arrays, load from IDM, and assign head
  !<
  subroutine swi_ar(this)
    ! -- dummy
    class(GwfSwiType) :: this
    ! -- local
    !
    ! -- set pointer to gwf iss
    call mem_setptr(this%iss, 'ISS', create_mem_path(this%name_model))
  end subroutine swi_ar

  !> @ brief Fill A and right-hand side for the package
  !!
  !!  Fill the coefficient matrix and right-hand side
  !!
  !<
  subroutine swi_fc(this, kiter, hold, hnew, matrix_sln, idxglo, rhs)
    ! -- modules
    use TdisModule, only: delt
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
    real(DP) :: tled
    real(DP) :: sc1
    real(DP) :: sc2
    real(DP) :: rho1
    real(DP) :: rho2
    real(DP) :: sc1old
    real(DP) :: sc2old
    real(DP) :: rho1old
    real(DP) :: rho2old
    real(DP) :: tp
    real(DP) :: bt
    real(DP) :: snold
    real(DP) :: snnew
    real(DP) :: aterm
    real(DP) :: rhsterm
    ! -- formats
    character(len=*), parameter :: fmtsperror = &
      &"('Detected time-step length of zero.  GWF SWI package cannot be ', &
      &'used unless time-step length is non-zero.')"
    !
    ! -- test if steady-state stress period
    if (this%iss /= 0) return
    !
    ! -- Ensure time step length is not zero
    if (delt == DZERO) then
      write (errmsg, fmtsperror)
      call store_error(errmsg, terminate=.TRUE.)
    end if
    !
    ! -- set variables
    tled = DONE / delt
    !
    ! -- loop through and calculate storage contribution to hcof and rhs
!     do n = 1, this%dis%nodes
!       idiag = this%dis%con%ia(n)
!       if (this%ibound(n) < 1) cycle
!       !
!       ! -- aquifer elevations and thickness
!       tp = this%dis%top(n)
!       bt = this%dis%bot(n)
!       !
!       ! -- aquifer saturation
!       if (this%iconvert(n) == 0) then
!         snold = DONE
!         snnew = DONE
!       else
!         snold = sQuadraticSaturation(tp, bt, hold(n), this%satomega)
!         snnew = sQuadraticSaturation(tp, bt, hnew(n), this%satomega)
!       end if
!       !
!       ! -- storage coefficients
!       sc1 = SsCapacity(this%istor_coef, tp, bt, this%dis%area(n), this%ss(n))
!       rho1 = sc1 * tled
!       !
!       if (this%integratechanges /= 0) then
!         ! -- Integration of storage changes (e.g. when using TVS):
!         !    separate the old (start of time step) and new (end of time step)
!         !    primary storage capacities
!         sc1old = SsCapacity(this%istor_coef, tp, bt, this%dis%area(n), &
!                             this%oldss(n))
!         rho1old = sc1old * tled
!       else
!         ! -- No integration of storage changes: old and new values are
!         !    identical => normal MF6 storage formulation
!         rho1old = rho1
!       end if
!       !
!       ! -- calculate specific storage terms
!       call SsTerms(this%iconvert(n), this%iorig_ss, this%iconf_ss, tp, bt, &
!                    rho1, rho1old, snnew, snold, hnew(n), hold(n), &
!                    aterm, rhsterm)
!       !
!       ! -- add specific storage terms to amat and rhs
!       call matrix_sln%add_value_pos(idxglo(idiag), aterm)
!       rhs(n) = rhs(n) + rhsterm
!       !
!       ! -- specific yield
!       if (this%iconvert(n) /= 0) then
!         rhsterm = DZERO
!         !
!         ! -- secondary storage coefficient
!         sc2 = SyCapacity(this%dis%area(n), this%sy(n))
!         rho2 = sc2 * tled
!         !
!         if (this%integratechanges /= 0) then
!           ! -- Integration of storage changes (e.g. when using TVS):
!           !    separate the old (start of time step) and new (end of time step)
!           !    secondary storage capacities
!           sc2old = SyCapacity(this%dis%area(n), this%oldsy(n))
!           rho2old = sc2old * tled
!         else
!           ! -- No integration of storage changes: old and new values are
!           !    identical => normal MF6 storage formulation
!           rho2old = rho2
!         end if
!         !
!         ! -- calculate specific storage terms
!         call SyTerms(tp, bt, rho2, rho2old, snnew, snold, &
!                      aterm, rhsterm)
! !
!         ! -- add specific yield terms to amat and rhs
!         call matrix_sln%add_value_pos(idxglo(idiag), aterm)
!         rhs(n) = rhs(n) + rhsterm
!       end if
!     end do
    !
    ! -- return
    return
  end subroutine swi_fc

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
    !
    ! -- deallocate parent
    call this%NumericalPackageType%da()
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
  end subroutine swi_load

  !> @brief Allocate arrays
  !<
  subroutine allocate_arrays(this, nodes)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwfSwiType) :: this
    integer(I4B), intent(in) :: nodes
    !
    ! -- Allocate
    call mem_allocate(this%zeta, nodes, 'ZETA', this%memoryPath)
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
    character(len=LINELENGTH) :: errmsg
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
  end subroutine source_griddata

end module GwfSwiModule

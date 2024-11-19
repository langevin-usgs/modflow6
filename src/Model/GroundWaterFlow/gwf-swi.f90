module GwfSwiModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH, DONE, DZERO, LENBUDTXT, &
                             MNORMAL, DHNOFLO, C3D_VERTICAL
  use SimVariablesModule, only: errmsg
  use SimModule, only: store_error
  use NumericalPackageModule, only: NumericalPackageType
  use BlockParserModule, only: BlockParserType
  use BaseDisModule, only: DisBaseType
  use GwfNpfModule, only: GwfNpfType
  use GwfStoModule, only: GwfStoType
  use SmoothingModule, only: sQuadraticSaturation, &
                             sQuadraticSaturationDerivative
  use MemoryManagerModule, only: mem_setptr, mem_allocate
  use MemoryHelperModule, only: create_mem_path
  use MatrixBaseModule
  use GwfConductanceUtilsModule, only: hcond

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
    real(DP), dimension(:), pointer, contiguous :: oldsy => null() !< pointer to storage package specific yield
    real(DP), dimension(:), pointer, contiguous :: oldss => null() !< pointer to storage package specific yield
  contains

    procedure :: swi_df
    procedure :: swi_ar
    procedure :: swi_fc
    procedure :: swi_fn
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
    procedure, private :: swi_fn_freshstorage

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
      !csp*** need to point to first model to get hfresh
      call mem_setptr(this%hfresh, 'X', &
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
  subroutine swi_fc(this, kiter, hold, hnew, matrix_sln, idxglo, rhs, npf, sto)
    ! -- modules
    ! -- dummy variables
    class(GwfSwiType) :: this !< GwfSwiType object
    integer(I4B), intent(in) :: kiter !< outer iteration number
    real(DP), intent(in), dimension(:) :: hold !< previous heads
    real(DP), intent(in), dimension(:) :: hnew !< current heads
    class(MatrixBaseType), pointer :: matrix_sln !< A matrix
    integer(I4B), intent(in), dimension(:) :: idxglo !< global index model to solution
    real(DP), intent(inout), dimension(:) :: rhs !< right-hand side
    type(GwfNpfType), intent(in) :: npf
    type(GwfStoType), intent(in) :: sto
    ! -- local variables
    integer(I4B) :: n
    integer(I4B) :: idiag
    ! -- formats
    !
    call npf_fc_swi(npf, kiter, matrix_sln, idxglo, &
                    rhs, hnew, this%zeta)
    !
    ! -- test if steady-state stress period
    if (this%iss /= 0) return
    !
    ! -- Calculate fresh storage terms and put in hcof/rhs
    if (this%iuseapi == 0) then
      if (this%isaltwater == 0) then
        call this%swi_fc_freshstorage(kiter, hold, hnew, matrix_sln, idxglo, rhs,sto)
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
  subroutine swi_fc_freshstorage(this, kiter, hold, hnew, matrix_sln, idxglo, rhs,sto)
    ! -- modules
    use TdisModule, only: delt
    use GwfStorageUtilsModule, only: SsCapacity, SyCapacity, SsTerms, SyTerms
    ! 
    class(GwfStoType) :: sto    
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
    real(DP) :: zetanew
    real(DP) :: zetaold
    !
    ! -- set variables
    tled = DONE / delt
    !
    ! -- Calculate storage terms for freshwater model by subtracting out saltwater side
    do n = 1, this%dis%nodes

      ! initialize
      this%hcof(n) = DZERO
      this%rhs(n) = DZERO

      if (this%ibound(n) < 1) cycle

      ! -- calculate zetanew and zetaold
      zetanew = calc_zeta(this%alphaf, hnew(n))
      zetaold = calc_zeta(this%alphaf, hold(n))
      ! -- aquifer elevations and thickness
      tp = this%dis%top(n)
      bt = this%dis%bot(n)
      !
      ! -- aquifer saturation
      if (sto%iconvert(n) == 0) then
        snold = DONE
        snnew = DONE
      else
        snold = sQuadraticSaturation(tp, bt, zetaold, sto%satomega)
        snnew = sQuadraticSaturation(tp, bt, zetanew, sto%satomega)
      end if
      !
      ! -- storage coefficients
      sc1 = SsCapacity(sto%istor_coef, tp, bt, this%dis%area(n), sto%ss(n))
      rho1 = sc1 * tled
      !
      if (sto%integratechanges /= 0) then
        ! -- Integration of storage changes (e.g. when using TVS):
        !    separate the old (start of time step) and new (end of time step)
        !    primary storage capacities
        sc1old = SsCapacity(sto%istor_coef, tp, bt, this%dis%area(n), &
                            this%oldss(n))
        rho1old = sc1old * tled
      else
        ! -- No integration of storage changes: old and new values are
        !    identical => normal MF6 storage formulation
        rho1old = rho1
      end if
      !
      ! -- calculate specific storage terms
      call SsTerms(sto%iconvert(n), sto%iorig_ss, sto%iconf_ss, tp, bt, &
                   rho1, rho1old, snnew, snold, zetanew, zetaold, &
                   aterm, rhsterm)
      !
      ! -- add specific storage terms to amat and rhs - subtract out aterm and rhsterm
      idiag = this%dis%con%ia(n)      
      call matrix_sln%add_value_pos(idxglo(idiag), -aterm)
      rhs(n) = rhs(n) - rhsterm
      !
      ! -- specific yield
      if (sto%iconvert(n) /= 0) then
        rhsterm = DZERO
        !
        ! -- secondary storage coefficient
        sc2 = SyCapacity(this%dis%area(n), this%sy(n))
        rho2 = sc2 * tled
        !
        if (sto%integratechanges /= 0) then
          ! -- Integration of storage changes (e.g. when using TVS):
          !    separate the old (start of time step) and new (end of time step)
          !    secondary storage capacities
          sc2old = SyCapacity(this%dis%area(n), this%oldsy(n))
          rho2old = sc2old * tled
        else
          ! -- No integration of storage changes: old and new values are
          !    identical => normal MF6 storage formulation
          rho2old = rho2
        end if
        !
        ! -- calculate specific storage terms
        call SyTerms(tp, bt, rho2, rho2old, snnew, snold, &
                     aterm, rhsterm)
!
        ! -- add specific yield terms to amat and rhs - subtract out aterm and rhsterm
        call matrix_sln%add_value_pos(idxglo(idiag), -aterm)
        rhs(n) = rhs(n) - rhsterm
 !csp*** old way of storage term commented out here       
     ! sc2 = SyCapacity(this%dis%area(n), this%sy(n))
     ! rho2 = -this%alphaf * sc2 * tled
     !   ! new and old zeta above bottom
     !   this%hcof(n) = rho2
     !   this%rhs(n) = rho2 * hold(n)
  !  
      endif
      !
    end do
    !
    ! -- return
    return
  end subroutine swi_fc_freshstorage

  subroutine swi_fn(this, kiter, matrix_sln, idxglo, rhs, hnew, hold, npf, sto)
    ! -- dummy
    class(GwfSwiType) :: this
    integer(I4B) :: kiter
    class(MatrixBaseType), pointer :: matrix_sln
    integer(I4B), intent(in), dimension(:) :: idxglo
    real(DP), intent(inout), dimension(:) :: rhs
    real(DP), intent(inout), dimension(:) :: hnew
    real(DP), intent(inout), dimension(:) :: hold
    type(GwfNpfType) :: npf
    type(GwfStoType) :: sto
    !
    call npf_fn_swi(npf, kiter, matrix_sln, idxglo, rhs, hnew, &
                    this%zeta, -this%alphaf)
    !
    ! -- test if steady-state stress period
    if (this%iss /= 0) return
    !
    ! -- Calculate fresh storage Newton terms and put in hcof/rhs
    if (this%iuseapi == 0) then
      if (this%isaltwater == 0) then
        call this%swi_fn_freshstorage(kiter, hold, hnew, matrix_sln, idxglo, rhs, sto)
      end if
    end if    
    !
  end subroutine swi_fn
 
  !> @ brief Calculate fresh storage terms
  !<
  subroutine swi_fn_freshstorage(this, kiter, hold, hnew, matrix_sln, idxglo, rhs, sto)
    ! -- modules
    use TdisModule, only: delt
    use GwfStorageUtilsModule, only: SsCapacity, SyCapacity, SsTerms, SyTerms
    ! 
    class(GwfStoType) :: sto    
    ! -- dummy variables
    class(GwfSwiType) :: this !< GwfSwiType object
    integer(I4B), intent(in) :: kiter !< outer iteration number
    real(DP), intent(in), dimension(:) :: hold !< previous heads
    real(DP), intent(in), dimension(:) :: hnew !< current heads
    class(MatrixBaseType), pointer :: matrix_sln !< A matrix
    integer(I4B), intent(in), dimension(:) :: idxglo !< global index model to solution
    real(DP), intent(inout), dimension(:) :: rhs !< right-hand side
    ! -- local variables
      ! -- local variables
    integer(I4B) :: n
    integer(I4B) :: idiag
    real(DP) :: tled
    real(DP) :: sc1
    real(DP) :: sc2
    real(DP) :: rho1
    real(DP) :: rho2
    real(DP) :: tp
    real(DP) :: bt
    real(DP) :: tthk
    real(DP) :: h
    real(DP) :: snnew
    real(DP) :: derv
    real(DP) :: rterm
    real(DP) :: drterm
    real(DP) :: zetanew
    real(DP) :: zetaold
    real(DP) :: sew,dereps
    !
    ! -- set variables
    tled = DONE / delt
    !
    ! -- loop through and calculate storage contribution to hcof and rhs
    do n = 1, this%dis%nodes
      idiag = this%dis%con%ia(n)
      if (this%ibound(n) <= 0) cycle
      !
      ! -- calculate zetanew and zetaold
      zetanew = calc_zeta(this%alphaf, hnew(n))
      zetaold = calc_zeta(this%alphaf, hold(n))
      !      
      ! -- aquifer elevations and thickness
      tp = this%dis%top(n)
      bt = this%dis%bot(n)
      tthk = tp - bt
      h = hnew(n)
      !
      ! -- aquifer saturation
      snnew = sQuadraticSaturation(tp, bt, zetanew)
      !
      ! -- storage coefficients
      sc1 = SsCapacity(sto%istor_coef, tp, bt, this%dis%area(n), sto%ss(n))
      sc2 = SyCapacity(this%dis%area(n), this%sy(n))
      rho1 = sc1 * tled
      rho2 = sc2 * tled
      !
      ! -- calculate newton terms for specific storage
      !    and specific yield
      if (sto%iconvert(n) /= 0) then
        !
        ! -- calculate saturation derivative as dS/dzeta * dzeta/dh_fresh
        derv = sQuadraticSaturationDerivative(tp, bt, zetanew)
        derv = derv * this%alphaf
        !
        sew = sQuadraticSaturation(tp, bt, zetanew, sto%satomega)
        dereps = 1e-6
        zetanew = calc_zeta(this%alphaf, hnew(n)+dereps)
        derv = sQuadraticSaturation(tp, bt, zetanew, sto%satomega)
        derv = (derv-sew)/dereps
        !
        ! -- newton terms for specific storage
        if (sto%iconf_ss == 0) then
          if (sto%iorig_ss == 0) then
            drterm = -rho1 * derv * (h - bt) + rho1 * tthk * snnew * derv
          else
            drterm = -(rho1 * derv * h)
          end if
          call matrix_sln%add_value_pos(idxglo(idiag), drterm)
          rhs(n) = rhs(n) + drterm * h
        end if
        !
        ! -- newton terms for specific yield
        !    only calculated if the current saturation
        !    is less than one
        if (snnew < DONE) then
          ! -- calculate newton terms for specific yield
          if (snnew > DZERO) then
            rterm = -rho2 * tthk * snnew
            drterm = -rho2 * tthk * derv
            ! subtract saltwater part from total flow terms
            call matrix_sln%add_value_pos(idxglo(idiag), -drterm - rho2)
            rhs(n) = rhs(n) -(- rterm + drterm * hnew(n) + rho2 * bt)
          end if
        end if
      end if
    end do  
    !
    ! -- return
    return
  end subroutine swi_fn_freshstorage  
  !
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

! CSP***--------------------------------------------------------------------------------
  !> @ brief Calculate flows for package
  !!
  !!  Flow calculation for the STO package components. Components include
  !!  specific storage and specific yield storage.
  !!
  !<
! subroutine swi_cq(this, flowja, hnew, hold)
!   ! -- modules
!   ! -- dummy variables
!   class(GwfSwiType) :: this !< GwfStoType object
!   real(DP), dimension(:), contiguous, intent(inout) :: flowja !< connection flows
!   real(DP), dimension(:), contiguous, intent(in) :: hnew !< current head
!   real(DP), dimension(:), contiguous, intent(in) :: hold !< previous head
!   ! -- local variables
!   integer(I4B) :: n
!   integer(I4B) :: idiag
!   real(DP) :: rate
!   !
!   ! -- initialize strg arrays
!   do n = 1, this%dis%nodes
!     this%storage(n) = DZERO
!   end do
!   !
!   ! Loop through cells
!   do n = 1, this%dis%nodes
!     !
!     ! -- Calculate change in freshwater storage
!     rate = this%hcof(n) * hnew(n) - this%rhs(n)
!     this%storage(n) = rate
!     !
!     ! -- Add storage term to flowja
!     idiag = this%dis%con%ia(n)
!     flowja(idiag) = flowja(idiag) + rate
!   end do
!   !
!   ! -- return
!   return
! end subroutine swi_cq
! CSP***--------------------------------------------------------------------------------

  !> @ brief Calculate flows and storages for SWI package and adjust flowja
  !!
  !<
  subroutine swi_cq(this, hnew, hold, flowja, npf, sto)
    ! -- modules
    use TdisModule, only: delt  
    use GwfStorageUtilsModule, only: SsCapacity, SyCapacity, SsTerms, SyTerms
    !
    ! -- dummy variables
    class(GwfSwiType) :: this !< GwfSwiType object
    real(DP), intent(in), dimension(:) :: hnew
    real(DP), dimension(:), contiguous, intent(in) :: hold !< previous head    
    real(DP), dimension(:), contiguous, intent(inout) :: flowja !< connection flows
    type(GwfNpfType), intent(in) :: npf
    type(GwfStoType), intent(in) :: sto
    ! -- local variables
    integer(I4B) :: n, ii, m, ictn, ictm 
    integer(I4B) :: idiag
    real(DP) :: qnm
    !  for storage terms
    real(DP) :: tled
    real(DP) :: rate
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
    real(DP) :: zetanew
    real(DP) :: zetaold
    
    !
    ! todo: need to issue error if xt3d is active
    ! if (npf%ixt3d /= 0) then
    !   call npf%xt3d%xt3d_fc(kiter, matrix_sln, idxglo, rhs, hnew)
    ! else

    ! loop over nodes and connections and call swi_qcalc to subtract qnm from flowja
    ictn = 1
    ictm = 1
    do n = 1, npf%dis%nodes
      do ii = npf%dis%con%ia(n) + 1, npf%dis%con%ia(n + 1) - 1
        if (npf%dis%con%mask(ii) == 0) cycle

        ! Calculate terms only for upper triangle but insert into
        ! upper and lower parts of amat.
        m = npf%dis%con%ja(ii)
        if (m < n) cycle
    !
        call swi_qcalc(npf, n, m, ii, ictn, ictm, hnew(n), hnew(m), qnm, this%zeta)
    !       change sign to subtract out qnm from salt side
        flowja(ii) = flowja(ii) - qnm
        flowja(this%dis%con%isym(ii)) = flowja(this%dis%con%isym(ii)) + qnm
      end do
    end do
    ! endif ! xt3d if-check
    !
    ! -- Set strt to zero or calculate terms if not steady-state stress period
    if (this%iss == 0) then
      !
      ! -- set variables
      tled = DONE / delt
      !
      ! -- Calculate storage change
      do n = 1, this%dis%nodes
        if (this%ibound(n) <= 0) cycle
        
        ! -- calculate zetanew and zetaold
        zetanew = calc_zeta(this%alphaf, hnew(n))
        zetaold = calc_zeta(this%alphaf, hold(n))
        ! -- aquifer elevations and thickness
        tp = this%dis%top(n)
        bt = this%dis%bot(n)
        !
        ! -- aquifer saturation
        if (sto%iconvert(n) == 0) then
          snold = DONE
          snnew = DONE
        else
          snold = sQuadraticSaturation(tp, bt, zetaold, sto%satomega)
          snnew = sQuadraticSaturation(tp, bt, zetanew, sto%satomega)
        end if
        !
        ! -- primary storage coefficient
        sc1 = SsCapacity(sto%istor_coef, tp, bt, this%dis%area(n), sto%ss(n))
        rho1 = sc1 * tled
        !
        if (sto%integratechanges /= 0) then
          ! -- Integration of storage changes (e.g. when using TVS):
          !    separate the old (start of time step) and new (end of time step)
          !    primary storage capacities
          sc1old = SsCapacity(sto%istor_coef, tp, bt, this%dis%area(n), &
                              this%oldss(n))
          rho1old = sc1old * tled
        else
          ! -- No integration of storage changes: old and new values are
          !    identical => normal MF6 storage formulation
          rho1old = rho1
        end if
        !
        ! -- calculate specific storage terms and rate
        call SsTerms(sto%iconvert(n), sto%iorig_ss, sto%iconf_ss, tp, bt, &
                     rho1, rho1old, snnew, snold, zetanew, zetaold, &
                     aterm, rhsterm, rate)
        !
        ! -- subtract rate in saltwater part from total
        sto%strgss(n) = sto%strgss(n) - rate
        !
        ! -- add storage term to flowja - subtract out saltwater part
        idiag = this%dis%con%ia(n)
        flowja(idiag) = flowja(idiag) - rate
        !
        ! -- specific yield
        rate = DZERO
        if (sto%iconvert(n) /= 0) then
          !
          ! -- secondary storage coefficient
          sc2 = SyCapacity(this%dis%area(n), this%sy(n))
          rho2 = sc2 * tled
          !
          if (sto%integratechanges /= 0) then
            ! -- Integration of storage changes (e.g. when using TVS):
            !    separate the old (start of time step) and new (end of time
            !    step) secondary storage capacities
            sc2old = SyCapacity(this%dis%area(n), this%oldsy(n))
            rho2old = sc2old * tled
          else
            ! -- No integration of storage changes: old and new values are
            !    identical => normal MF6 storage formulation
            rho2old = rho2
          end if
          !
          ! -- calculate specific yield storage terms and rate
          call SyTerms(tp, bt, rho2, rho2old, snnew, snold, &
                       aterm, rhsterm, rate)

        end if
        ! -- subtract rate in saltwater part from total        
        sto%strgsy(n) = sto%strgsy(n) - rate
        !
        ! -- add storage term to flowja - subtract out saltwater part
        idiag = this%dis%con%ia(n)
        flowja(idiag) = flowja(idiag) - rate
      end do
    end if
    !
    ! -- return
    return
  end subroutine swi_cq


  !> @ brief Calculate flows for SWI package and adjust flowja
  !!
  !<
  subroutine swi_qcalc(npf, n, m, ii, ictn, ictm, hn, hm,qnm, zeta)
    ! -- modules
    use ConstantsModule, only: DONE
    ! -- dummy
    class(GwfNpfType) :: npf
    integer(I4B) :: n, m, ii, ihc, ictn, ictm 
    real(DP), intent(in) :: hn
    real(DP), intent(in) :: hm
    real(DP), intent(out) :: qnm
    real(DP), intent(in), dimension(:) :: zeta
    ! -- local
    real(DP) :: hyn, hym
    real(DP) :: cond 
    real(DP) :: satn, satm

    ! --Calculate freshwater flow between nodes n and m on the saltwater side
    ! For SWI correction is only in horizontal direction
    ihc = npf%dis%con%ihc(npf%dis%con%jas(ii))
    if (ihc == C3D_VERTICAL) return

    ! Use NPF to get the effective hydraulic conductivity
    hyn = npf%hy_eff(n, m, ihc, ipos=ii)
    hym = npf%hy_eff(m, n, ihc, ipos=ii)

    ! -- Horizontal conductance
    ! calculate saturation based on zeta, so that hcond is for the
    ! region from zeta down to bottom; hnew is passed in so that
    ! upstream is based on head and not zeta
    call swi_thksat(n, npf%dis%top(n), npf%dis%bot(n), &
                    zeta(n), satn, npf%inewton)
    call swi_thksat(m, npf%dis%top(m), npf%dis%bot(m), &
                    zeta(m), satm, npf%inewton)
    cond = hcond(npf%ibound(n), npf%ibound(m), &
                 ictn, ictm, &
                 npf%inewton, &
                 npf%dis%con%ihc(npf%dis%con%jas(ii)), &
                 npf%icellavg, &
                 npf%condsat(npf%dis%con%jas(ii)), &
                 hn, hm, &
                 satn, satm, &
                 hyn, hym, &
                 npf%dis%top(n), npf%dis%top(m), &
                 npf%dis%bot(n), npf%dis%bot(m), &
                 npf%dis%con%cl1(npf%dis%con%jas(ii)), &
                 npf%dis%con%cl2(npf%dis%con%jas(ii)), &
                 npf%dis%con%hwva(npf%dis%con%jas(ii)))
    !
    ! -- Calculate flow positive into cell n
    qnm = cond * (hm - hn)
    !
    ! -- return
    return
  end subroutine swi_qcalc

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
    call mem_set_value(this%isaltwater, 'ISALTWATER', this%input_mempath, &
                       found%isaltwater)
    call mem_set_value(zeta_fname, 'ZETAFILE', this%input_mempath, &
                       found%zetafile)
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
        this%zeta(n) = calc_zeta(this%alphaf, this%hfresh(n), &
                                 this%alphas, this%hsalt(n))
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
    zeta = -alphaf * hf
    if (present(alphas) .and. present(hs)) then
      zeta = zeta + alphas * hs
    end if
    return
  end function calc_zeta

  !> @brief Add swi correction term
  !!
  !! This is a retooling of the npf_fc to subtract the saltwater flow
  !> that would occur below zeta.
  subroutine npf_fc_swi(npf, kiter, matrix_sln, idxglo, rhs, hnew, zeta)
    ! -- modules
    use ConstantsModule, only: DONE
    ! -- dummy
    class(GwfNpfType) :: npf
    integer(I4B) :: kiter
    class(MatrixBaseType), pointer :: matrix_sln
    integer(I4B), intent(in), dimension(:) :: idxglo
    real(DP), intent(inout), dimension(:) :: rhs
    real(DP), intent(in), dimension(:) :: hnew
    real(DP), intent(in), dimension(:) :: zeta
    ! -- local
    integer(I4B) :: n, m, ii, idiag, ihc
    integer(I4B) :: isymcon, idiagm
    real(DP) :: hyn, hym
    real(DP) :: cond
    real(DP) :: satn, satm
    integer(I4B) :: ictn, ictm
    !
    ! -- Calculate conductance and put into amat
    !
    ! todo: need to issue error if xt3d is active
    ! if (npf%ixt3d /= 0) then
    !   call npf%xt3d%xt3d_fc(kiter, matrix_sln, idxglo, rhs, hnew)
    ! else

    ! Set the celltype to be 1 so that zeta is used as the top
    ! surface for evaluation of the saturated thickness in the
    ! saltwater zone
    ictn = 1
    ictm = 1
!
    do n = 1, npf%dis%nodes
      do ii = npf%dis%con%ia(n) + 1, npf%dis%con%ia(n + 1) - 1
        if (npf%dis%con%mask(ii) == 0) cycle

        ! Calculate terms only for upper triangle but insert into
        ! upper and lower parts of amat.
        m = npf%dis%con%ja(ii)
        if (m < n) cycle

        ! For SWI correction is only in horizontal direction
        ihc = npf%dis%con%ihc(npf%dis%con%jas(ii))
        if (ihc == C3D_VERTICAL) cycle

        ! Use NPF to get the effective hydraulic conductivity
        hyn = npf%hy_eff(n, m, ihc, ipos=ii)
        hym = npf%hy_eff(m, n, ihc, ipos=ii)

        ! -- Horizontal conductance
        ! calculate saturation based on zeta, so that hcond is for the
        ! region from zeta down to bottom; hnew is passed in so that
        ! upstream is based on head and not zeta
        call swi_thksat(n, npf%dis%top(n), npf%dis%bot(n), &
                        zeta(n), satn, npf%inewton)
        call swi_thksat(m, npf%dis%top(m), npf%dis%bot(m), &
                        zeta(m), satm, npf%inewton)
        cond = hcond(npf%ibound(n), npf%ibound(m), &
                     ictn, ictm, &
                     npf%inewton, &
                     npf%dis%con%ihc(npf%dis%con%jas(ii)), &
                     npf%icellavg, &
                     npf%condsat(npf%dis%con%jas(ii)), &
                     hnew(n), hnew(m), &
                     satn, satm, &
                     hyn, hym, &
                     npf%dis%top(n), npf%dis%top(m), &
                     npf%dis%bot(n), npf%dis%bot(m), &
                     npf%dis%con%cl1(npf%dis%con%jas(ii)), &
                     npf%dis%con%cl2(npf%dis%con%jas(ii)), &
                     npf%dis%con%hwva(npf%dis%con%jas(ii)))

        ! Fill row n, Note signs are flipped in order to
        ! subtract the flow from the saltwater zone
        idiag = npf%dis%con%ia(n)
        call matrix_sln%add_value_pos(idxglo(ii), -cond)
        call matrix_sln%add_value_pos(idxglo(idiag), cond)

        ! Fill row m, Note signs are flipped in order to
        ! subtract the flow from the saltwater zone
        isymcon = npf%dis%con%isym(ii)
        idiagm = npf%dis%con%ia(m)
        call matrix_sln%add_value_pos(idxglo(isymcon), -cond)
        call matrix_sln%add_value_pos(idxglo(idiagm), cond)
      end do
    end do
  end subroutine npf_fc_swi

  !> @brief Fill newton terms
  !<
  subroutine npf_fn_swi(npf, kiter, matrix_sln, idxglo, rhs, hnew, zeta, dzetadh)
    ! -- dummy
    type(GwfNpfType) :: npf
    integer(I4B) :: kiter
    class(MatrixBaseType), pointer :: matrix_sln
    integer(I4B), intent(in), dimension(:) :: idxglo
    real(DP), intent(inout), dimension(:) :: rhs
    real(DP), intent(inout), dimension(:) :: hnew
    real(DP), intent(inout), dimension(:) :: zeta
    real(DP), intent(in) :: dzetadh
    ! -- local
    integer(I4B) :: nodes, nja
    integer(I4B) :: n, m, ii, idiag
    integer(I4B) :: isymcon, idiagm
    integer(I4B) :: iups
    integer(I4B) :: idn
    real(DP) :: cond
    real(DP) :: consterm
    real(DP) :: filledterm
    real(DP) :: derv
    real(DP) :: hds
    real(DP) :: term
    real(DP) :: topup
    real(DP) :: botup
    !
    ! -- add newton terms to solution matrix
    nodes = npf%dis%nodes
    nja = npf%dis%con%nja
    !
    ! todo: need to issue error if xt3d is active
    !if (npf%ixt3d /= 0) then
    !  call npf%xt3d%xt3d_fn(kiter, nodes, nja, matrix_sln, idxglo, rhs, hnew)
    !else
      !
      do n = 1, nodes
        idiag = npf%dis%con%ia(n)
        do ii = npf%dis%con%ia(n) + 1, npf%dis%con%ia(n + 1) - 1
          if (npf%dis%con%mask(ii) == 0) cycle

          m = npf%dis%con%ja(ii)
          isymcon = npf%dis%con%isym(ii)
          ! work on upper triangle
          if (m < n) cycle
          if (npf%dis%con%ihc(npf%dis%con%jas(ii)) == 0 .and. &
              npf%ivarcv == 0) then
            !call npf%vcond(n,m,hnew(n),hnew(m),ii,cond)
            ! do nothing
          else
            ! determine upstream node
            iups = m
            if (hnew(m) < hnew(n)) iups = n
            idn = n
            if (iups == n) idn = m
            !
            ! -- no newton terms if upstream cell is confined
            ! for swi, always do newton
            !if (npf%icelltype(iups) == 0) cycle
            !
            ! -- Set the upstream top and bot, and then recalculate for a
            !    vertically staggered horizontal connection
            topup = npf%dis%top(iups)
            botup = npf%dis%bot(iups)
            if (npf%dis%con%ihc(npf%dis%con%jas(ii)) == 2) then
              topup = min(npf%dis%top(n), npf%dis%top(m))
              botup = max(npf%dis%bot(n), npf%dis%bot(m))
            end if
            !
            ! get saturated conductivity for derivative
            cond = npf%condsat(npf%dis%con%jas(ii))
            !
            ! compute additional term
            consterm = -cond * (hnew(iups) - hnew(idn)) !needs to use hwadi instead of hnew(idn)
            !filledterm = cond
            filledterm = matrix_sln%get_value_pos(idxglo(ii))
            ! use zeta in derivative
            derv = sQuadraticSaturationDerivative(topup, botup, zeta(iups), &
                                                  npf%satomega)
            derv = derv * dzetadh
            idiagm = npf%dis%con%ia(m)
            ! fill jacobian for n being the upstream node
            if (iups == n) then
              hds = hnew(m)
              !isymcon =  npf%dis%con%isym(ii)
              term = consterm * derv
              ! flip signs for swi correction
              rhs(n) = rhs(n) - term * hnew(n) !+ amat(idxglo(isymcon)) * (dwadi * hds - hds) !need to add dwadi
              rhs(m) = rhs(m) + term * hnew(n) !- amat(idxglo(isymcon)) * (dwadi * hds - hds) !need to add dwadi
              ! fill in row of n
              ! flip sign for swi correction
              call matrix_sln%add_value_pos(idxglo(idiag), -term)
              ! fill newton term in off diagonal if active cell
              if (npf%ibound(n) > 0) then
                filledterm = matrix_sln%get_value_pos(idxglo(ii))
                call matrix_sln%set_value_pos(idxglo(ii), filledterm) !* dwadi !need to add dwadi
              end if
              !fill row of m
              filledterm = matrix_sln%get_value_pos(idxglo(idiagm))
              call matrix_sln%set_value_pos(idxglo(idiagm), filledterm) !- filledterm * (dwadi - DONE) !need to add dwadi
              ! fill newton term in off diagonal if active cell
              if (npf%ibound(m) > 0) then
                ! flip sign for swi correction
                call matrix_sln%add_value_pos(idxglo(isymcon), term)
              end if
              ! fill jacobian for m being the upstream node
            else
              hds = hnew(n)
              term = -consterm * derv
              ! flip sign for swi correction
              rhs(n) = rhs(n) - term * hnew(m) !+ amat(idxglo(ii)) * (dwadi * hds - hds) !need to add dwadi
              rhs(m) = rhs(m) + term * hnew(m) !- amat(idxglo(ii)) * (dwadi * hds - hds) !need to add dwadi
              ! fill in row of n
              filledterm = matrix_sln%get_value_pos(idxglo(idiag))
              call matrix_sln%set_value_pos(idxglo(idiag), filledterm) !- filledterm * (dwadi - DONE) !need to add dwadi
              ! fill newton term in off diagonal if active cell
              if (npf%ibound(n) > 0) then
                ! flip sign for swi correction
                call matrix_sln%add_value_pos(idxglo(ii), -term)
              end if
              !fill row of m
              ! flip sign for swi correction
              call matrix_sln%add_value_pos(idxglo(idiagm), term)
              ! fill newton term in off diagonal if active cell
              if (npf%ibound(m) > 0) then
                filledterm = matrix_sln%get_value_pos(idxglo(isymcon))
                call matrix_sln%set_value_pos(idxglo(isymcon), filledterm) !* dwadi  !need to add dwadi
              end if
            end if
          end if

        end do
      end do
      !
!    end if
    !
    ! -- Return
    return
  end subroutine npf_fn_swi

  !> @brief Fractional cell saturation
  !<
  subroutine swi_thksat(n, top, bot, zeta, thksat, inewton)
    ! -- dummy
    integer(I4B), intent(in) :: n
    real(DP), intent(in) :: top
    real(DP), intent(in) :: bot
    real(DP), intent(in) :: zeta
    real(DP), intent(inout) :: thksat
    integer(I4B), intent(in) :: inewton
    !
    ! -- Standard Formulation
    if (zeta >= top) then
      thksat = DONE
    else
      thksat = (zeta - bot) / (top - bot)
    end if
    !
    ! -- Smoothed thickness
    if (inewton /= 0) then
      thksat = sQuadraticSaturation(top, bot, zeta)
    end if
    !
    ! -- Return
    return
  end subroutine swi_thksat

end module GwfSwiModule

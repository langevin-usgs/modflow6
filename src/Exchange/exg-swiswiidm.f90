! ** Do Not Modify! MODFLOW 6 system generated file. **
module ExgSwiswiInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public exg_swiswi_param_definitions
  public exg_swiswi_aggregate_definitions
  public exg_swiswi_block_definitions
  public ExgSwiswiParamFoundType
  public exg_swiswi_multi_package

  type ExgSwiswiParamFoundType
    logical :: ipr_input = .false.
    logical :: ipr_flow = .false.
    logical :: nexg = .false.
    logical :: cellidm1 = .false.
    logical :: cellidm2 = .false.
    logical :: amat_fresh = .false.
    logical :: rhs_fresh = .false.
    logical :: amat_salt = .false.
    logical :: rhs_salt = .false.
  end type ExgSwiswiParamFoundType

  logical :: exg_swiswi_multi_package = .true.

  type(InputParamDefinitionType), parameter :: &
    exgswiswi_ipr_input = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'SWISWI', & ! subcomponent
    'OPTIONS', & ! block
    'PRINT_INPUT', & ! tag name
    'IPR_INPUT', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exgswiswi_ipr_flow = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'SWISWI', & ! subcomponent
    'OPTIONS', & ! block
    'PRINT_FLOWS', & ! tag name
    'IPR_FLOW', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exgswiswi_nexg = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'SWISWI', & ! subcomponent
    'DIMENSIONS', & ! block
    'NEXG', & ! tag name
    'NEXG', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exgswiswi_cellidm1 = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'SWISWI', & ! subcomponent
    'EXCHANGEDATA', & ! block
    'CELLIDM1', & ! tag name
    'CELLIDM1', & ! fortran variable
    'INTEGER1D', & ! type
    'NCELLDIM', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exgswiswi_cellidm2 = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'SWISWI', & ! subcomponent
    'EXCHANGEDATA', & ! block
    'CELLIDM2', & ! tag name
    'CELLIDM2', & ! fortran variable
    'INTEGER1D', & ! type
    'NCELLDIM', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exgswiswi_amat_fresh = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'SWISWI', & ! subcomponent
    'EXCHANGEDATA', & ! block
    'AMAT_FRESH', & ! tag name
    'AMAT_FRESH', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exgswiswi_rhs_fresh = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'SWISWI', & ! subcomponent
    'EXCHANGEDATA', & ! block
    'RHS_FRESH', & ! tag name
    'RHS_FRESH', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exgswiswi_amat_salt = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'SWISWI', & ! subcomponent
    'EXCHANGEDATA', & ! block
    'AMAT_SALT', & ! tag name
    'AMAT_SALT', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exgswiswi_rhs_salt = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'SWISWI', & ! subcomponent
    'EXCHANGEDATA', & ! block
    'RHS_SALT', & ! tag name
    'RHS_SALT', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exg_swiswi_param_definitions(*) = &
    [ &
    exgswiswi_ipr_input, &
    exgswiswi_ipr_flow, &
    exgswiswi_nexg, &
    exgswiswi_cellidm1, &
    exgswiswi_cellidm2, &
    exgswiswi_amat_fresh, &
    exgswiswi_rhs_fresh, &
    exgswiswi_amat_salt, &
    exgswiswi_rhs_salt &
    ]

  type(InputParamDefinitionType), parameter :: &
    exgswiswi_exchangedata = InputParamDefinitionType &
    ( &
    'EXG', & ! component
    'SWISWI', & ! subcomponent
    'EXCHANGEDATA', & ! block
    'EXCHANGEDATA', & ! tag name
    'EXCHANGEDATA', & ! fortran variable
    'RECARRAY CELLIDM1 CELLIDM2 AMAT_FRESH RHS_FRESH AMAT_SALT RHS_SALT', & ! type
    'NEXG', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    exg_swiswi_aggregate_definitions(*) = &
    [ &
    exgswiswi_exchangedata &
    ]

  type(InputBlockDefinitionType), parameter :: &
    exg_swiswi_block_definitions(*) = &
    [ &
    InputBlockDefinitionType( &
    'OPTIONS', & ! blockname
    .false., & ! required
    .false., & ! aggregate
    .false. & ! block_variable
    ), &
    InputBlockDefinitionType( &
    'DIMENSIONS', & ! blockname
    .true., & ! required
    .false., & ! aggregate
    .false. & ! block_variable
    ), &
    InputBlockDefinitionType( &
    'EXCHANGEDATA', & ! blockname
    .true., & ! required
    .true., & ! aggregate
    .false. & ! block_variable
    ) &
    ]

end module ExgSwiswiInputModule

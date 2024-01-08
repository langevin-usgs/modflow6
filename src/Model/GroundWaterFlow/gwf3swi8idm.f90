! ** Do Not Modify! MODFLOW 6 system generated file. **
module GwfSwiInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public gwf_swi_param_definitions
  public gwf_swi_aggregate_definitions
  public gwf_swi_block_definitions
  public GwfSwiParamFoundType
  public gwf_swi_multi_package

  type GwfSwiParamFoundType
    logical :: zeta = .false.
  end type GwfSwiParamFoundType

  logical :: gwf_swi_multi_package = .false.

  type(InputParamDefinitionType), parameter :: &
    gwfswi_zeta = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'SWI', & ! subcomponent
    'GRIDDATA', & ! block
    'ZETA', & ! tag name
    'ZETA', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwf_swi_param_definitions(*) = &
    [ &
    gwfswi_zeta &
    ]

  type(InputParamDefinitionType), parameter :: &
    gwf_swi_aggregate_definitions(*) = &
    [ &
    InputParamDefinitionType &
    ( &
    '', & ! component
    '', & ! subcomponent
    '', & ! block
    '', & ! tag name
    '', & ! fortran variable
    '', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    ) &
    ]

  type(InputBlockDefinitionType), parameter :: &
    gwf_swi_block_definitions(*) = &
    [ &
    InputBlockDefinitionType( &
    'OPTIONS', & ! blockname
    .false., & ! required
    .false., & ! aggregate
    .false. & ! block_variable
    ), &
    InputBlockDefinitionType( &
    'GRIDDATA', & ! blockname
    .true., & ! required
    .false., & ! aggregate
    .false. & ! block_variable
    ) &
    ]

end module GwfSwiInputModule

! ** Do Not Modify! MODFLOW 6 system generated file. **
module SwfIcInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public swf_ic_param_definitions
  public swf_ic_aggregate_definitions
  public swf_ic_block_definitions
  public SwfIcParamFoundType
  public swf_ic_multi_package

  type SwfIcParamFoundType
    logical :: strt = .false.
  end type SwfIcParamFoundType

  logical :: swf_ic_multi_package = .false.

  type(InputParamDefinitionType), parameter :: &
    swfic_strt = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'IC', & ! subcomponent
    'GRIDDATA', & ! block
    'STRT', & ! tag name
    'STRT', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swf_ic_param_definitions(*) = &
    [ &
    swfic_strt &
    ]

  type(InputParamDefinitionType), parameter :: &
    swf_ic_aggregate_definitions(*) = &
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
    swf_ic_block_definitions(*) = &
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

end module SwfIcInputModule

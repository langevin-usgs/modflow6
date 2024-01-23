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
    logical :: isaltwater = .false.
    logical :: zeta_filerecord = .false.
    logical :: zeta = .false.
    logical :: fileout = .false.
    logical :: zetafile = .false.
    logical :: zetastrt = .false.
  end type GwfSwiParamFoundType

  logical :: gwf_swi_multi_package = .false.

  type(InputParamDefinitionType), parameter :: &
    gwfswi_isaltwater = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'SWI', & ! subcomponent
    'OPTIONS', & ! block
    'SALTWATER', & ! tag name
    'ISALTWATER', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfswi_zeta_filerecord = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'SWI', & ! subcomponent
    'OPTIONS', & ! block
    'ZETA_FILERECORD', & ! tag name
    'ZETA_FILERECORD', & ! fortran variable
    'RECORD ZETA FILEOUT ZETAFILE', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfswi_zeta = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'SWI', & ! subcomponent
    'OPTIONS', & ! block
    'ZETA', & ! tag name
    'ZETA', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfswi_fileout = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'SWI', & ! subcomponent
    'OPTIONS', & ! block
    'FILEOUT', & ! tag name
    'FILEOUT', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfswi_zetafile = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'SWI', & ! subcomponent
    'OPTIONS', & ! block
    'ZETAFILE', & ! tag name
    'ZETAFILE', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .true., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfswi_zetastrt = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'SWI', & ! subcomponent
    'GRIDDATA', & ! block
    'ZETASTRT', & ! tag name
    'ZETASTRT', & ! fortran variable
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
    gwfswi_isaltwater, &
    gwfswi_zeta_filerecord, &
    gwfswi_zeta, &
    gwfswi_fileout, &
    gwfswi_zetafile, &
    gwfswi_zetastrt &
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

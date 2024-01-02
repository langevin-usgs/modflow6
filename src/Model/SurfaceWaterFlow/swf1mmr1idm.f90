! ** Do Not Modify! MODFLOW 6 system generated file. **
module SwfMmrInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public swf_mmr_param_definitions
  public swf_mmr_aggregate_definitions
  public swf_mmr_block_definitions
  public SwfMmrParamFoundType
  public swf_mmr_multi_package

  type SwfMmrParamFoundType
    logical :: ipakcb = .false.
    logical :: iprflow = .false.
    logical :: obs_filerecord = .false.
    logical :: obs6 = .false.
    logical :: filein = .false.
    logical :: obs6_filename = .false.
    logical :: iseg_order = .false.
    logical :: qoutflow0 = .false.
    logical :: k_coef = .false.
    logical :: x_coef = .false.
  end type SwfMmrParamFoundType

  logical :: swf_mmr_multi_package = .false.

  type(InputParamDefinitionType), parameter :: &
    swfmmr_ipakcb = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'MMR', & ! subcomponent
    'OPTIONS', & ! block
    'SAVE_FLOWS', & ! tag name
    'IPAKCB', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swfmmr_iprflow = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'MMR', & ! subcomponent
    'OPTIONS', & ! block
    'PRINT_FLOWS', & ! tag name
    'IPRFLOW', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swfmmr_obs_filerecord = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'MMR', & ! subcomponent
    'OPTIONS', & ! block
    'OBS_FILERECORD', & ! tag name
    'OBS_FILERECORD', & ! fortran variable
    'RECORD OBS6 FILEIN OBS6_FILENAME', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swfmmr_obs6 = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'MMR', & ! subcomponent
    'OPTIONS', & ! block
    'OBS6', & ! tag name
    'OBS6', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swfmmr_filein = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'MMR', & ! subcomponent
    'OPTIONS', & ! block
    'FILEIN', & ! tag name
    'FILEIN', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swfmmr_obs6_filename = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'MMR', & ! subcomponent
    'OPTIONS', & ! block
    'OBS6_FILENAME', & ! tag name
    'OBS6_FILENAME', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .true., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swfmmr_iseg_order = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'MMR', & ! subcomponent
    'GRIDDATA', & ! block
    'ISEG_ORDER', & ! tag name
    'ISEG_ORDER', & ! fortran variable
    'INTEGER1D', & ! type
    'NODES', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swfmmr_qoutflow0 = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'MMR', & ! subcomponent
    'GRIDDATA', & ! block
    'QOUTFLOW0', & ! tag name
    'QOUTFLOW0', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swfmmr_k_coef = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'MMR', & ! subcomponent
    'GRIDDATA', & ! block
    'K_COEF', & ! tag name
    'K_COEF', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swfmmr_x_coef = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'MMR', & ! subcomponent
    'GRIDDATA', & ! block
    'X_COEF', & ! tag name
    'X_COEF', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swf_mmr_param_definitions(*) = &
    [ &
    swfmmr_ipakcb, &
    swfmmr_iprflow, &
    swfmmr_obs_filerecord, &
    swfmmr_obs6, &
    swfmmr_filein, &
    swfmmr_obs6_filename, &
    swfmmr_iseg_order, &
    swfmmr_qoutflow0, &
    swfmmr_k_coef, &
    swfmmr_x_coef &
    ]

  type(InputParamDefinitionType), parameter :: &
    swf_mmr_aggregate_definitions(*) = &
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
    swf_mmr_block_definitions(*) = &
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

end module SwfMmrInputModule

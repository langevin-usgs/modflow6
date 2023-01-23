! ** Do Not Modify! MODFLOW 6 system generated file. **
module SwfMctInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public swf_mct_param_definitions
  public swf_mct_aggregate_definitions
  public swf_mct_block_definitions
  public SwfMctParamFoundType
  public swf_mct_multi_package

  type SwfMctParamFoundType
    logical :: unitconv = .false.
    logical :: ipakcb = .false.
    logical :: iprflow = .false.
    logical :: obs_filerecord = .false.
    logical :: obs6 = .false.
    logical :: filein = .false.
    logical :: obs6_filename = .false.
    logical :: icalc_order = .false.
    logical :: qoutflow0 = .false.
    logical :: width = .false.
    logical :: manningsn = .false.
    logical :: elevation = .false.
    logical :: slope = .false.
    logical :: idcxs = .false.
  end type SwfMctParamFoundType

  logical :: swf_mct_multi_package = .false.

  type(InputParamDefinitionType), parameter :: &
    swfmct_unitconv = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'MCT', & ! subcomponent
    'OPTIONS', & ! block
    'UNITCONV', & ! tag name
    'UNITCONV', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swfmct_ipakcb = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'MCT', & ! subcomponent
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
    swfmct_iprflow = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'MCT', & ! subcomponent
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
    swfmct_obs_filerecord = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'MCT', & ! subcomponent
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
    swfmct_obs6 = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'MCT', & ! subcomponent
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
    swfmct_filein = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'MCT', & ! subcomponent
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
    swfmct_obs6_filename = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'MCT', & ! subcomponent
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
    swfmct_icalc_order = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'MCT', & ! subcomponent
    'GRIDDATA', & ! block
    'ICALC_ORDER', & ! tag name
    'ICALC_ORDER', & ! fortran variable
    'INTEGER1D', & ! type
    'NODES', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swfmct_qoutflow0 = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'MCT', & ! subcomponent
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
    swfmct_width = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'MCT', & ! subcomponent
    'GRIDDATA', & ! block
    'WIDTH', & ! tag name
    'WIDTH', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swfmct_manningsn = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'MCT', & ! subcomponent
    'GRIDDATA', & ! block
    'MANNINGSN', & ! tag name
    'MANNINGSN', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swfmct_elevation = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'MCT', & ! subcomponent
    'GRIDDATA', & ! block
    'ELEVATION', & ! tag name
    'ELEVATION', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swfmct_slope = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'MCT', & ! subcomponent
    'GRIDDATA', & ! block
    'SLOPE', & ! tag name
    'SLOPE', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swfmct_idcxs = InputParamDefinitionType &
    ( &
    'SWF', & ! component
    'MCT', & ! subcomponent
    'GRIDDATA', & ! block
    'IDCXS', & ! tag name
    'IDCXS', & ! fortran variable
    'INTEGER1D', & ! type
    'NODES', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    swf_mct_param_definitions(*) = &
    [ &
    swfmct_unitconv, &
    swfmct_ipakcb, &
    swfmct_iprflow, &
    swfmct_obs_filerecord, &
    swfmct_obs6, &
    swfmct_filein, &
    swfmct_obs6_filename, &
    swfmct_icalc_order, &
    swfmct_qoutflow0, &
    swfmct_width, &
    swfmct_manningsn, &
    swfmct_elevation, &
    swfmct_slope, &
    swfmct_idcxs &
    ]

  type(InputParamDefinitionType), parameter :: &
    swf_mct_aggregate_definitions(*) = &
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
    swf_mct_block_definitions(*) = &
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

end module SwfMctInputModule

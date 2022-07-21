module GwfNpfInputModule
  use InputDefinitionModule, only: InputDefinitionType
  private
  public gwf_npf_definitions
  type(InputDefinitionType), parameter :: gwf_npf_definitions(*) = &
  [ &

    ! GWF NPF OPTIONS
    InputDefinitionType( &
      'GWF', &   ! component
      'NPF', &   ! subcomponent
      'OPTIONS', &   ! block
      'OPTIONS', &   ! tag name
      '--', &   ! fortran variable
      'BLOCK', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'NPF', &   ! subcomponent
      'OPTIONS', &   ! block
      'SAVE_FLOWS', &   ! tag name
      'IPAKCB', &   ! fortran variable
      'KEYWORD', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'NPF', &   ! subcomponent
      'OPTIONS', &   ! block
      'ALTERNATIVE_CELL_AVERAGING', &   ! tag name
      'CELLAVG', &   ! fortran variable
      'STRING', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'NPF', &   ! subcomponent
      'OPTIONS', &   ! block
      'THICKSTRT', &   ! tag name
      'ITHICKSTRT', &   ! fortran variable
      'KEYWORD', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'NPF', &   ! subcomponent
      'OPTIONS', &   ! block
      'CVOPTIONS', &   ! tag name
      'CVOPTIONS', &   ! fortran variable
      'RECORD VARIABLECV DEWATERED', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'NPF', &   ! subcomponent
      'OPTIONS', &   ! block
      'VARIABLECV', &   ! tag name
      'IVARCV', &   ! fortran variable
      'KEYWORD', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'NPF', &   ! subcomponent
      'OPTIONS', &   ! block
      'DEWATERED', &   ! tag name
      'IDEWATCV', &   ! fortran variable
      'KEYWORD', &   ! type
      '', &   ! shape
      .false., &   ! required
      .true. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'NPF', &   ! subcomponent
      'OPTIONS', &   ! block
      'PERCHED', &   ! tag name
      'IPERCHED', &   ! fortran variable
      'KEYWORD', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'NPF', &   ! subcomponent
      'OPTIONS', &   ! block
      'REWET_RECORD', &   ! tag name
      'REWET_RECORD', &   ! fortran variable
      'RECORD REWET WETFCT IWETIT IHDWET', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'NPF', &   ! subcomponent
      'OPTIONS', &   ! block
      'REWET', &   ! tag name
      'IREWET', &   ! fortran variable
      'KEYWORD', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'NPF', &   ! subcomponent
      'OPTIONS', &   ! block
      'WETFCT', &   ! tag name
      'WETFCT', &   ! fortran variable
      'DOUBLE', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'NPF', &   ! subcomponent
      'OPTIONS', &   ! block
      'IWETIT', &   ! tag name
      'IWETIT', &   ! fortran variable
      'INTEGER', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'NPF', &   ! subcomponent
      'OPTIONS', &   ! block
      'IHDWET', &   ! tag name
      'IHDWET', &   ! fortran variable
      'INTEGER', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'NPF', &   ! subcomponent
      'OPTIONS', &   ! block
      'XT3DOPTIONS', &   ! tag name
      'XT3DOPTIONS', &   ! fortran variable
      'RECORD XT3D RHS', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'NPF', &   ! subcomponent
      'OPTIONS', &   ! block
      'XT3D', &   ! tag name
      'IXT3D', &   ! fortran variable
      'KEYWORD', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'NPF', &   ! subcomponent
      'OPTIONS', &   ! block
      'RHS', &   ! tag name
      'IXT3DRHS', &   ! fortran variable
      'KEYWORD', &   ! type
      '', &   ! shape
      .false., &   ! required
      .true. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'NPF', &   ! subcomponent
      'OPTIONS', &   ! block
      'SAVE_SPECIFIC_DISCHARGE', &   ! tag name
      'ISAVSPDIS', &   ! fortran variable
      'KEYWORD', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'NPF', &   ! subcomponent
      'OPTIONS', &   ! block
      'SAVE_SATURATION', &   ! tag name
      'ISAVSAT', &   ! fortran variable
      'KEYWORD', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'NPF', &   ! subcomponent
      'OPTIONS', &   ! block
      'K22OVERK', &   ! tag name
      'IK22OVERK', &   ! fortran variable
      'KEYWORD', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'NPF', &   ! subcomponent
      'OPTIONS', &   ! block
      'K33OVERK', &   ! tag name
      'IK33OVERK', &   ! fortran variable
      'KEYWORD', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'NPF', &   ! subcomponent
      'OPTIONS', &   ! block
      'TVK_FILERECORD', &   ! tag name
      'TVK_FILERECORD', &   ! fortran variable
      'RECORD TVK6 FILEIN TVK_FILENAME', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'NPF', &   ! subcomponent
      'OPTIONS', &   ! block
      'TVK6', &   ! tag name
      'TVK6', &   ! fortran variable
      'KEYWORD', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'NPF', &   ! subcomponent
      'OPTIONS', &   ! block
      'FILEIN', &   ! tag name
      'FILEIN', &   ! fortran variable
      'KEYWORD', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'NPF', &   ! subcomponent
      'OPTIONS', &   ! block
      'TVK_FILENAME', &   ! tag name
      'TVK_FILENAME', &   ! fortran variable
      'STRING', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true. &   ! multi-record
    ), &

    ! GWF NPF GRIDDATA
    InputDefinitionType( &
      'GWF', &   ! component
      'NPF', &   ! subcomponent
      'GRIDDATA', &   ! block
      'GRIDDATA', &   ! tag name
      '--', &   ! fortran variable
      'BLOCK', &   ! type
      '', &   ! shape
      .true., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'NPF', &   ! subcomponent
      'GRIDDATA', &   ! block
      'ICELLTYPE', &   ! tag name
      'ICELLTYPE', &   ! fortran variable
      'INTEGER1D', &   ! type
      'NODES', &   ! shape
      .true., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'NPF', &   ! subcomponent
      'GRIDDATA', &   ! block
      'K', &   ! tag name
      'K', &   ! fortran variable
      'DOUBLE1D', &   ! type
      'NODES', &   ! shape
      .true., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'NPF', &   ! subcomponent
      'GRIDDATA', &   ! block
      'K22', &   ! tag name
      'K22', &   ! fortran variable
      'DOUBLE1D', &   ! type
      'NODES', &   ! shape
      .false., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'NPF', &   ! subcomponent
      'GRIDDATA', &   ! block
      'K33', &   ! tag name
      'K33', &   ! fortran variable
      'DOUBLE1D', &   ! type
      'NODES', &   ! shape
      .false., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'NPF', &   ! subcomponent
      'GRIDDATA', &   ! block
      'ANGLE1', &   ! tag name
      'ANGLE1', &   ! fortran variable
      'DOUBLE1D', &   ! type
      'NODES', &   ! shape
      .false., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'NPF', &   ! subcomponent
      'GRIDDATA', &   ! block
      'ANGLE2', &   ! tag name
      'ANGLE2', &   ! fortran variable
      'DOUBLE1D', &   ! type
      'NODES', &   ! shape
      .false., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'NPF', &   ! subcomponent
      'GRIDDATA', &   ! block
      'ANGLE3', &   ! tag name
      'ANGLE3', &   ! fortran variable
      'DOUBLE1D', &   ! type
      'NODES', &   ! shape
      .false., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'NPF', &   ! subcomponent
      'GRIDDATA', &   ! block
      'WETDRY', &   ! tag name
      'WETDRY', &   ! fortran variable
      'DOUBLE1D', &   ! type
      'NODES', &   ! shape
      .false., &   ! required
      .false. &   ! multi-record
    ) &
  ]
end module GwfNpfInputModule

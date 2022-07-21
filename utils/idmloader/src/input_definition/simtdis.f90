module SimTdisInputModule
  use InputDefinitionModule, only: InputDefinitionType
  private
  public sim_tdis_definitions
  type(InputDefinitionType), parameter :: sim_tdis_definitions(*) = &
  [ &

    ! SIM TDIS OPTIONS
    InputDefinitionType( &
      'SIM', &   ! component
      'TDIS', &   ! subcomponent
      'OPTIONS', &   ! block
      'OPTIONS', &   ! tag name
      '--', &   ! fortran variable
      'BLOCK', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'SIM', &   ! component
      'TDIS', &   ! subcomponent
      'OPTIONS', &   ! block
      'TIME_UNITS', &   ! tag name
      'TIME_UNITS', &   ! fortran variable
      'STRING', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'SIM', &   ! component
      'TDIS', &   ! subcomponent
      'OPTIONS', &   ! block
      'START_DATE_TIME', &   ! tag name
      'START_DATE_TIME', &   ! fortran variable
      'STRING', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'SIM', &   ! component
      'TDIS', &   ! subcomponent
      'OPTIONS', &   ! block
      'ATS_FILERECORD', &   ! tag name
      'ATS_FILERECORD', &   ! fortran variable
      'RECORD ATS6 FILEIN ATS6_FILENAME', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'SIM', &   ! component
      'TDIS', &   ! subcomponent
      'OPTIONS', &   ! block
      'ATS6', &   ! tag name
      'ATS6', &   ! fortran variable
      'KEYWORD', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true. &   ! multi-record
    ), &
    InputDefinitionType( &
      'SIM', &   ! component
      'TDIS', &   ! subcomponent
      'OPTIONS', &   ! block
      'FILEIN', &   ! tag name
      'FILEIN', &   ! fortran variable
      'KEYWORD', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true. &   ! multi-record
    ), &
    InputDefinitionType( &
      'SIM', &   ! component
      'TDIS', &   ! subcomponent
      'OPTIONS', &   ! block
      'ATS6_FILENAME', &   ! tag name
      'ATS6_FILENAME', &   ! fortran variable
      'STRING', &   ! type
      '', &   ! shape
      .true., &   ! required
      .true. &   ! multi-record
    ), &

    ! SIM TDIS DIMENSIONS
    InputDefinitionType( &
      'SIM', &   ! component
      'TDIS', &   ! subcomponent
      'DIMENSIONS', &   ! block
      'DIMENSIONS', &   ! tag name
      '--', &   ! fortran variable
      'BLOCK', &   ! type
      '', &   ! shape
      .true., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'SIM', &   ! component
      'TDIS', &   ! subcomponent
      'DIMENSIONS', &   ! block
      'NPER', &   ! tag name
      'NPER', &   ! fortran variable
      'INTEGER', &   ! type
      '', &   ! shape
      .true., &   ! required
      .false. &   ! multi-record
    ), &

    ! SIM TDIS PERIODDATA
    InputDefinitionType( &
      'SIM', &   ! component
      'TDIS', &   ! subcomponent
      'PERIODDATA', &   ! block
      'PERIODDATA', &   ! tag name
      'STRUCTARRAY PERLEN NSTP TSMULT', &   ! fortran variable
      'BLOCK', &   ! type
      'NPER', &   ! shape
      .true., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'SIM', &   ! component
      'TDIS', &   ! subcomponent
      'PERIODDATA', &   ! block
      'PERIODDATA', &   ! tag name
      'PERIODDATA', &   ! fortran variable
      'RECARRAY PERLEN NSTP TSMULT', &   ! type
      '', &   ! shape
      .true., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'SIM', &   ! component
      'TDIS', &   ! subcomponent
      'PERIODDATA', &   ! block
      'PERLEN', &   ! tag name
      'PERLEN', &   ! fortran variable
      'DOUBLE1D', &   ! type
      'NPER', &   ! shape
      .true., &   ! required
      .true. &   ! multi-record
    ), &
    InputDefinitionType( &
      'SIM', &   ! component
      'TDIS', &   ! subcomponent
      'PERIODDATA', &   ! block
      'NSTP', &   ! tag name
      'NSTP', &   ! fortran variable
      'INTEGER1D', &   ! type
      'NPER', &   ! shape
      .true., &   ! required
      .true. &   ! multi-record
    ), &
    InputDefinitionType( &
      'SIM', &   ! component
      'TDIS', &   ! subcomponent
      'PERIODDATA', &   ! block
      'TSMULT', &   ! tag name
      'TSMULT', &   ! fortran variable
      'DOUBLE1D', &   ! type
      'NPER', &   ! shape
      .true., &   ! required
      .true. &   ! multi-record
    ) &
  ]
end module SimTdisInputModule

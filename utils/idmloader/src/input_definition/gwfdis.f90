module GwfDisInputModule
  use InputDefinitionModule, only: InputDefinitionType
  private
  public gwf_dis_definitions
  type(InputDefinitionType), parameter :: gwf_dis_definitions(*) = &
  [ &

    ! GWF DIS OPTIONS
    InputDefinitionType( &
      'GWF', &   ! component
      'DIS', &   ! subcomponent
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
      'DIS', &   ! subcomponent
      'OPTIONS', &   ! block
      'LENGTH_UNITS', &   ! tag name
      'LENGTH_UNITS', &   ! fortran variable
      'STRING', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'DIS', &   ! subcomponent
      'OPTIONS', &   ! block
      'NOGRB', &   ! tag name
      'NOGRB', &   ! fortran variable
      'KEYWORD', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'DIS', &   ! subcomponent
      'OPTIONS', &   ! block
      'XORIGIN', &   ! tag name
      'XORIGIN', &   ! fortran variable
      'DOUBLE', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'DIS', &   ! subcomponent
      'OPTIONS', &   ! block
      'YORIGIN', &   ! tag name
      'YORIGIN', &   ! fortran variable
      'DOUBLE', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'DIS', &   ! subcomponent
      'OPTIONS', &   ! block
      'ANGROT', &   ! tag name
      'ANGROT', &   ! fortran variable
      'DOUBLE', &   ! type
      '', &   ! shape
      .false., &   ! required
      .false. &   ! multi-record
    ), &

    ! GWF DIS DIMENSIONS
    InputDefinitionType( &
      'GWF', &   ! component
      'DIS', &   ! subcomponent
      'DIMENSIONS', &   ! block
      'DIMENSIONS', &   ! tag name
      '--', &   ! fortran variable
      'BLOCK', &   ! type
      '', &   ! shape
      .true., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'DIS', &   ! subcomponent
      'DIMENSIONS', &   ! block
      'NLAY', &   ! tag name
      'NLAY', &   ! fortran variable
      'INTEGER', &   ! type
      '', &   ! shape
      .true., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'DIS', &   ! subcomponent
      'DIMENSIONS', &   ! block
      'NROW', &   ! tag name
      'NROW', &   ! fortran variable
      'INTEGER', &   ! type
      '', &   ! shape
      .true., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'DIS', &   ! subcomponent
      'DIMENSIONS', &   ! block
      'NCOL', &   ! tag name
      'NCOL', &   ! fortran variable
      'INTEGER', &   ! type
      '', &   ! shape
      .true., &   ! required
      .false. &   ! multi-record
    ), &

    ! GWF DIS GRIDDATA
    InputDefinitionType( &
      'GWF', &   ! component
      'DIS', &   ! subcomponent
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
      'DIS', &   ! subcomponent
      'GRIDDATA', &   ! block
      'DELR', &   ! tag name
      'DELR', &   ! fortran variable
      'DOUBLE1D', &   ! type
      'NCOL', &   ! shape
      .true., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'DIS', &   ! subcomponent
      'GRIDDATA', &   ! block
      'DELC', &   ! tag name
      'DELC', &   ! fortran variable
      'DOUBLE1D', &   ! type
      'NROW', &   ! shape
      .true., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'DIS', &   ! subcomponent
      'GRIDDATA', &   ! block
      'TOP', &   ! tag name
      'TOP', &   ! fortran variable
      'DOUBLE2D', &   ! type
      'NCOL, NROW', &   ! shape
      .true., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'DIS', &   ! subcomponent
      'GRIDDATA', &   ! block
      'BOTM', &   ! tag name
      'BOTM', &   ! fortran variable
      'DOUBLE3D', &   ! type
      'NCOL, NROW, NLAY', &   ! shape
      .true., &   ! required
      .false. &   ! multi-record
    ), &
    InputDefinitionType( &
      'GWF', &   ! component
      'DIS', &   ! subcomponent
      'GRIDDATA', &   ! block
      'IDOMAIN', &   ! tag name
      'IDOMAIN', &   ! fortran variable
      'INTEGER3D', &   ! type
      'NCOL, NROW, NLAY', &   ! shape
      .false., &   ! required
      .false. &   ! multi-record
    ) &
  ]
end module GwfDisInputModule

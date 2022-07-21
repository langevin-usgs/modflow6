module InputDefinitionModule
  use KindModule, only: I4B, DP, LGP
  use ConstantsModule, only: DONE

  implicit none
  private
  public :: input_definition_types
  public :: InputDefinitionType
  
  type InputDefinitionType
    character(len=100) :: component_type = ''
    character(len=100) :: subcomponent_type = ''
    character(len=100) :: blockname = ''
    character(len=100) :: tagname = ''
    character(len=100) :: mf6varname = ''
    character(len=100) :: datatype = ''
    character(len=100) :: shape = ''
    logical(LGP)       :: required = .false.
    logical(LGP)       :: in_record = .false.
  end type InputDefinitionType
  
  type(InputDefinitionType), parameter :: input_definition_types(*) = &
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
    ), &    
    
    ! GWF DIS OPTIONS
    InputDefinitionType( &
      'GWF', 'DIS', 'OPTIONS', 'OPTIONS', '--', 'BLOCK', '', .false. &
      ), &
    InputDefinitionType( &
      'GWF', 'DIS', 'OPTIONS', 'LENGTH_UNITS', 'LENUNI', 'CHARACTER', '', .false. &
      ), &
    InputDefinitionType( &
      'GWF', 'DIS', 'OPTIONS', 'NOGRB', 'NOGRB', 'KEYWORD', '', .false. &
      ), &
    InputDefinitionType( &
      'GWF', 'DIS', 'OPTIONS', 'XORIGIN', 'XORIGIN', 'DOUBLE', '', .false. &
      ), &
    InputDefinitionType( &
      'GWF', 'DIS', 'OPTIONS', 'YORIGIN', 'YORIGIN', 'DOUBLE', '', .false. &
      ), &
    InputDefinitionType( &
      'GWF', 'DIS', 'OPTIONS', 'ANGROT', 'ANGROT', 'DOUBLE', '', .false. &
      ), &
    
    ! GWF DIS DIMENSIONS
    InputDefinitionType( &
      'GWF', 'DIS', 'DIMENSIONS', 'DIMENSIONS', '--', 'BLOCK', '', .true. &
      ), &
    InputDefinitionType( &
      'GWF', 'DIS', 'DIMENSIONS', 'NLAY', 'NLAY', 'INTEGER', '', .true. &
      ), &
    InputDefinitionType( &
      'GWF', 'DIS', 'DIMENSIONS', 'NROW', 'NROW', 'INTEGER', '', .true. &
      ), &
    InputDefinitionType( &
      'GWF', 'DIS', 'DIMENSIONS', 'NCOL', 'NCOL', 'INTEGER', '', .true. &
      ), &
    
    ! GWF DIS GRIDDATA
    InputDefinitionType( &
      'GWF', 'DIS', 'GRIDDATA', 'GRIDDATA', '--', 'BLOCK', '', .true. &
      ), &
    InputDefinitionType( &
      'GWF', 'DIS', 'GRIDDATA', 'DELR', 'DELR', 'DOUBLE1D', 'NCOL', .true. &
      ), &
    InputDefinitionType( &
      'GWF', 'DIS', 'GRIDDATA', 'DELC', 'DELC', 'DOUBLE1D', 'NROW', .true. &
      ), &
    InputDefinitionType( &
      'GWF', 'DIS', 'GRIDDATA', 'TOP', 'TOP2D', 'DOUBLE2D', 'NCOL NROW', .true. &
      ), &
    InputDefinitionType( &
      'GWF', 'DIS', 'GRIDDATA', 'BOTM', 'BOT3D', 'DOUBLE3D', 'NCOL NROW NLAY', .true. &
      ), &
    InputDefinitionType( &
      'GWF', 'DIS', 'GRIDDATA', 'IDOMAIN', 'IDOMAIN', 'INTEGER3D', 'NCOL NROW NLAY', .true. &
      ), &
    
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
    ), &
    
    InputDefinitionType( &
      '---', &
      '---', &
      '---', &
      '---', &
      '---', &
      '---', &
      '---' &
      ) &
    
    ]
    
  
end module InputDefinitionModule

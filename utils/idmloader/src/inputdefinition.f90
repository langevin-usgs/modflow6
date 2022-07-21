module InputDefinitionModule

  use KindModule, only: LGP

  implicit none
  private
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

end module InputDefinitionModule

module InputDefinitionSelectorModule

  use SimVariablesModule, only: errmsg
  use SimModule, only: store_error
  use InputDefinitionModule, only: InputDefinitionType
  use SimTdisInputModule, only: sim_tdis_definitions
  use GwfDisInputModule, only: gwf_dis_definitions
  use GwfNpfInputModule, only: gwf_npf_definitions

  implicit none
  private
  public :: get_input_definition

  contains

  function get_input_definition(component) result(input_definition)
    character(len=*), intent(in) :: component 
    type(InputDefinitionType), dimension(:), pointer :: input_definition

    select case (component)
    case ('SIM/TDIS')
      call set_pointer(input_definition, sim_tdis_definitions)
    case ('GWF/DIS')
      call set_pointer(input_definition, gwf_dis_definitions)
    case ('GWF/NPF')
      call set_pointer(input_definition, gwf_npf_definitions)
    case default
      write(errmsg, '(4x,a,a)') 'Programming error determining type of input: ', trim(component)
      call store_error(errmsg, terminate=.true.)
    end select

    return
  end function get_input_definition
  
  subroutine set_pointer(input_definition, input_definition_target)
    type(InputDefinitionType), dimension(:), pointer :: input_definition
    type(InputDefinitionType), dimension(:), target :: input_definition_target
    input_definition => input_definition_target
  end subroutine set_pointer
  
end module InputDefinitionSelectorModule

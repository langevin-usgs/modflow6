! ** Do Not Modify! MODFLOW 6 system generated file. **
module IdmSwfDfnSelectorModule

  use ConstantsModule, only: LENVARNAME
  use SimModule, only: store_error
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  use SwfNamInputModule
  use SwfDislInputModule
  use SwfMmrInputModule
  use SwfMctInputModule
  use SwfCxsInputModule
  use SwfDfwInputModule
  use SwfIcInputModule
  use SwfChdInputModule
  use SwfFlwInputModule
  use SwfZdgInputModule

  implicit none
  private
  public :: swf_param_definitions
  public :: swf_aggregate_definitions
  public :: swf_block_definitions
  public :: swf_idm_multi_package
  public :: swf_idm_integrated

contains

  subroutine set_param_pointer(input_dfn, input_dfn_target)
    type(InputParamDefinitionType), dimension(:), pointer :: input_dfn
    type(InputParamDefinitionType), dimension(:), target :: input_dfn_target
    input_dfn => input_dfn_target
  end subroutine set_param_pointer

  subroutine set_block_pointer(input_dfn, input_dfn_target)
    type(InputBlockDefinitionType), dimension(:), pointer :: input_dfn
    type(InputBlockDefinitionType), dimension(:), target :: input_dfn_target
    input_dfn => input_dfn_target
  end subroutine set_block_pointer

  function swf_param_definitions(subcomponent) result(input_definition)
    character(len=*), intent(in) :: subcomponent
    type(InputParamDefinitionType), dimension(:), pointer :: input_definition
    nullify (input_definition)
    select case (subcomponent)
    case ('NAM')
      call set_param_pointer(input_definition, swf_nam_param_definitions)
    case ('DISL')
      call set_param_pointer(input_definition, swf_disl_param_definitions)
    case ('MMR')
      call set_param_pointer(input_definition, swf_mmr_param_definitions)
    case ('MCT')
      call set_param_pointer(input_definition, swf_mct_param_definitions)
    case ('CXS')
      call set_param_pointer(input_definition, swf_cxs_param_definitions)
    case ('DFW')
      call set_param_pointer(input_definition, swf_dfw_param_definitions)
    case ('IC')
      call set_param_pointer(input_definition, swf_ic_param_definitions)
    case ('CHD')
      call set_param_pointer(input_definition, swf_chd_param_definitions)
    case ('FLW')
      call set_param_pointer(input_definition, swf_flw_param_definitions)
    case ('ZDG')
      call set_param_pointer(input_definition, swf_zdg_param_definitions)
    case default
    end select
    return
  end function swf_param_definitions

  function swf_aggregate_definitions(subcomponent) result(input_definition)
    character(len=*), intent(in) :: subcomponent
    type(InputParamDefinitionType), dimension(:), pointer :: input_definition
    nullify (input_definition)
    select case (subcomponent)
    case ('NAM')
      call set_param_pointer(input_definition, swf_nam_aggregate_definitions)
    case ('DISL')
      call set_param_pointer(input_definition, swf_disl_aggregate_definitions)
    case ('MMR')
      call set_param_pointer(input_definition, swf_mmr_aggregate_definitions)
    case ('MCT')
      call set_param_pointer(input_definition, swf_mct_aggregate_definitions)
    case ('CXS')
      call set_param_pointer(input_definition, swf_cxs_aggregate_definitions)
    case ('DFW')
      call set_param_pointer(input_definition, swf_dfw_aggregate_definitions)
    case ('IC')
      call set_param_pointer(input_definition, swf_ic_aggregate_definitions)
    case ('CHD')
      call set_param_pointer(input_definition, swf_chd_aggregate_definitions)
    case ('FLW')
      call set_param_pointer(input_definition, swf_flw_aggregate_definitions)
    case ('ZDG')
      call set_param_pointer(input_definition, swf_zdg_aggregate_definitions)
    case default
    end select
    return
  end function swf_aggregate_definitions

  function swf_block_definitions(subcomponent) result(input_definition)
    character(len=*), intent(in) :: subcomponent
    type(InputBlockDefinitionType), dimension(:), pointer :: input_definition
    nullify (input_definition)
    select case (subcomponent)
    case ('NAM')
      call set_block_pointer(input_definition, swf_nam_block_definitions)
    case ('DISL')
      call set_block_pointer(input_definition, swf_disl_block_definitions)
    case ('MMR')
      call set_block_pointer(input_definition, swf_mmr_block_definitions)
    case ('MCT')
      call set_block_pointer(input_definition, swf_mct_block_definitions)
    case ('CXS')
      call set_block_pointer(input_definition, swf_cxs_block_definitions)
    case ('DFW')
      call set_block_pointer(input_definition, swf_dfw_block_definitions)
    case ('IC')
      call set_block_pointer(input_definition, swf_ic_block_definitions)
    case ('CHD')
      call set_block_pointer(input_definition, swf_chd_block_definitions)
    case ('FLW')
      call set_block_pointer(input_definition, swf_flw_block_definitions)
    case ('ZDG')
      call set_block_pointer(input_definition, swf_zdg_block_definitions)
    case default
    end select
    return
  end function swf_block_definitions

  function swf_idm_multi_package(subcomponent) result(multi_package)
    character(len=*), intent(in) :: subcomponent
    logical :: multi_package
    select case (subcomponent)
    case ('NAM')
      multi_package = swf_nam_multi_package
    case ('DISL')
      multi_package = swf_disl_multi_package
    case ('MMR')
      multi_package = swf_mmr_multi_package
    case ('MCT')
      multi_package = swf_mct_multi_package
    case ('CXS')
      multi_package = swf_cxs_multi_package
    case ('DFW')
      multi_package = swf_dfw_multi_package
    case ('IC')
      multi_package = swf_ic_multi_package
    case ('CHD')
      multi_package = swf_chd_multi_package
    case ('FLW')
      multi_package = swf_flw_multi_package
    case ('ZDG')
      multi_package = swf_zdg_multi_package
    case default
      call store_error('Idm selector subcomponent not found; '//&
                       &'component="SWF"'//&
                       &', subcomponent="'//trim(subcomponent)//'".', .true.)
    end select
    return
  end function swf_idm_multi_package

  function swf_idm_integrated(subcomponent) result(integrated)
    character(len=*), intent(in) :: subcomponent
    logical :: integrated
    integrated = .false.
    select case (subcomponent)
    case ('NAM')
      integrated = .true.
    case ('DISL')
      integrated = .true.
    case ('MMR')
      integrated = .true.
    case ('MCT')
      integrated = .true.
    case ('CXS')
      integrated = .true.
    case ('DFW')
      integrated = .true.
    case ('IC')
      integrated = .true.
    case ('CHD')
      integrated = .true.
    case ('FLW')
      integrated = .true.
    case ('ZDG')
      integrated = .true.
    case default
    end select
    return
  end function swf_idm_integrated

end module IdmSwfDfnSelectorModule

module LoadFromFileModule
  
  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH, LENMEMPATH
  use SimVariablesModule, only: iout, errmsg
  use SimModule, only: store_error
  use BlockParserModule, only: BlockParserType
  use ArrayReadersModule, only: ReadArray
  use InputOutputModule,  only: openfile, parseline
  use InputDefinitionModule, only: InputDefinitionType  !, input_definition_types
  use InputDefinitionSelectorModule, only: get_input_definition
  use MemoryManagerModule, only: mem_allocate, mem_setptr
  use MemoryHelperModule, only: create_mem_path
  
  implicit none
  public
  
  contains
  
  subroutine input_from_file(ftype, fname, component_type, subcomponent_type, &
    component_name, subcomponent_name, mshape)
    character(len=*) :: ftype
    character(len=*) :: fname
    character(len=*), intent(in) :: component_type
    character(len=*), intent(in) :: subcomponent_type
    character(len=*), intent(in) :: component_name
    character(len=*), intent(in) :: subcomponent_name
    integer(I4B), dimension(:), optional, intent(in) :: mshape
    character(len=LENMEMPATH) :: memoryPath
    character(len=100), dimension(:), allocatable :: unique_blocknames
    type(BlockParserType) :: parser
    integer(I4B) :: inunit
    integer(I4B) :: iblock
    type(InputDefinitionType), dimension(:), pointer :: input_definition_types
    
    ! create the memory path for information in this file
    memoryPath = create_mem_path(component_name, subcomponent_name)
    input_definition_types => get_input_definition(trim(component_type)//'/'//trim(subcomponent_type))

    ! open the file
    inunit = 99
    call openfile(inunit, 0, fname, ftype)

    ! initialize parser
    call parser%initialize(inunit, iout)
    
    ! determine number of unique blocks
    call get_unique_blocknames(input_definition_types, &
      component_type, subcomponent_type, unique_blocknames)
    
    ! process each block
    do iblock = 1, size(unique_blocknames)
      call parse_block(parser, component_type, subcomponent_type, &
        unique_blocknames(iblock), iout, memoryPath, input_definition_types)
    end do

    ! close
    close(inunit)
    
    ! hacky set of model shape into memory manager
    call set_model_shape(ftype, component_name, memoryPath)
    
    return
  end subroutine input_from_file
    
  subroutine parse_block(parser, component_type, subcomponent_type, &
      blockname, iout, memoryPath, input_definition_types)
    type(BlockParserType), intent(inout) :: parser
    character(len=*), intent(in) :: component_type
    character(len=*), intent(in) :: subcomponent_type
    character(len=*), intent(in) :: blockname
    integer(I4B), intent(in) :: iout
    character(len=*), intent(in) :: memoryPath
    type(InputDefinitionType), dimension(:), intent(in) :: input_definition_types
    logical(LGP) :: isblockfound
    logical(LGP) :: istagfound
    logical(LGP) :: endOfBlock
    logical(LGP) :: supportOpenClose
    logical(LGP) :: blockRequired
    logical(LGP) :: isStructArrayBlock
    integer(I4B) :: ierr
    
    ! determine if block is required
    blockRequired = is_block_required(input_definition_types, &
      component_type, subcomponent_type, blockname)
    
    ! determine if block supports open/close redirect
    supportOpenClose = support_open_close(input_definition_types, &
      component_type, subcomponent_type, blockname)
    
    ! determine if block is a structured list array
    isStructArrayBlock = is_structarray_block(input_definition_types, &
      component_type, subcomponent_type, blockname)
    
    call parser%GetBlock(blockname, isblockfound, ierr, &
                         supportOpenClose=supportOpenClose, &
                         blockRequired=blockRequired)
    
    if (isblockfound) then
      if (iout > 0) then
        write(iout,'(1x,a)') 'processing block ' // trim(blockname)
      end if

      if (isStructArrayBlock) then
          
        ! hack to parse a structured array block
        call parse_structarray_block(parser, component_type, &
          subcomponent_type, blockname, iout, memoryPath, input_definition_types)
      
      else

        do
        
          ! process each line in block
          call parser%GetNextLine(endOfBlock)
          if (endOfBlock) exit
        
          ! call routine to read data
          call parse_tag(parser, component_type, subcomponent_type, &
            blockname, iout, memoryPath, .false., input_definition_types)

        end do

      end if

    end if

    return
  end subroutine parse_block
      
  subroutine parse_structarray_block(parser, component_type, subcomponent_type, &
      blockname, iout, memoryPath, input_definition_types)
    use StructArrayModule, only: StructArrayType, constructStructArray
    type(BlockParserType), intent(inout) :: parser
    character(len=*), intent(in) :: component_type
    character(len=*), intent(in) :: subcomponent_type
    character(len=*), intent(in) :: blockname
    integer(I4B), intent(in) :: iout
    character(len=*), intent(in) :: memoryPath
    type(InputDefinitionType), dimension(:), intent(in) :: input_definition_types
    type(InputDefinitionType), pointer :: idt
    character(len=LINELENGTH) :: tag
    integer(I4B), pointer :: nrow
    integer(I4B) :: icol
    integer(I4B) :: ncol
    integer(I4B) :: nwords
    character(len=16), dimension(:), allocatable :: words
    type(StructArrayType) :: struct_array
  
    print *, 'processing list block ' // trim(blockname)
    
    ! find the data type for this block
    idt => get_input_definition_type(input_definition_types, &
      component_type, subcomponent_type, blockname)
    
    ! parse to find the names of the columns in this struct array
    ! ncol is one less than nwords because STRUCTARRAY is first item
    call parseline(idt%mf6varname, nwords, words)
    ncol = nwords - 1
    
    ! use shape to set the max num of rows for this struct array
    call mem_setptr(nrow, idt%shape, memoryPath)
    
    ! create a structured array
    struct_array = constructStructArray(ncol, nrow)
    do icol = 1, ncol
      
      ! set pointer to input definition for this 1d vector
      idt => get_input_definition_type(input_definition_types, &
        component_type, subcomponent_type, words(icol + 1))

      ! allocate a new vector in the memory manager using idt information
      ! and add it to the struct_array in position icol
      call struct_array%mem_create_vector(icol, idt%datatype, nrow, &
                                          idt%mf6varname, memoryPath)

    end do

    ! read the structured array
    call struct_array%read_from_parser(parser)
    call parser%terminateblock()
    
    ! destroy the structured array reader

    return
  end subroutine parse_structarray_block
      
  recursive subroutine parse_tag(parser, component_type, subcomponent_type, &
      blockname, iout, memoryPath, recursive_call, input_definition_types)
    type(BlockParserType), intent(inout) :: parser
    character(len=*), intent(in) :: component_type
    character(len=*), intent(in) :: subcomponent_type
    character(len=*), intent(in) :: blockname
    integer(I4B), intent(in) :: iout
    character(len=*), intent(in) :: memoryPath
    logical(LGP), intent(in) :: recursive_call
    type(InputDefinitionType), dimension(:), intent(in) :: input_definition_types
    character(len=LINELENGTH) :: tag
    type(InputDefinitionType), pointer :: idt
    character(len=16), dimension(:), allocatable :: words
  
    ! read tag name
    call parser%GetStringCaps(tag)
    if (recursive_call) then
      if (tag == '') then
        ! no data on line so return
        return
      end if
    end if
    print *, 'processing input tag ' // trim(tag)
        
    ! find keyword in input definition
    idt => get_input_definition_type(input_definition_types, &
      component_type, subcomponent_type, tag)
    if (.not. associated(idt)) then
      write(errmsg, '(4x,a,a)') 'Unrecognized data tag: ', trim(tag)
      call store_error(errmsg)
      call parser%StoreErrorUnit()
    end if
        
    select case (idt%datatype)
      case('KEYWORD')
        call load_keyword_type(parser, idt, memoryPath, iout)
      case('STRING')
        call load_string_type(parser, idt, memoryPath, iout)
      case('INTEGER')
        call load_integer_type(parser, idt, memoryPath, iout)
      case('INTEGER1D')
        call load_integer1d_type(parser, idt, memoryPath, iout)
      case('INTEGER3D')
        call load_integer3d_type(parser, idt, memoryPath, iout)
      case('DOUBLE')
        call load_double_type(parser, idt, memoryPath, iout)
      case('DOUBLE1D')
        call load_double1d_type(parser, idt, memoryPath, iout)
      case('DOUBLE2D')
        call load_double2d_type(parser, idt, memoryPath, iout)
      case('DOUBLE3D')
        call load_double3d_type(parser, idt, memoryPath, iout)
      case default
        write(errmsg, '(4x,a,a)') 'Failure reading data for tag: ', trim(tag)
        call store_error(errmsg)
        call parser%StoreErrorUnit()
      end select
      
      if (idt%in_record) then
        ! recursively call parse tag again to read rest of line
        call parse_tag(parser, component_type, subcomponent_type, &
          blockname, iout, memoryPath, .true., input_definition_types)
      end if
      
    return
  end subroutine parse_tag
  
  subroutine load_keyword_type(parser, idt, memoryPath, iout)
    type(BlockParserType), intent(inout) :: parser
    type(InputDefinitionType), intent(in) :: idt
    character(len=*), intent(in) :: memoryPath
    integer(I4B), intent(in) :: iout
    integer(I4B), pointer :: intvar
    call mem_allocate(intvar, idt%mf6varname, memoryPath)
    intvar = 1
    return
  end subroutine load_keyword_type
  
  subroutine load_string_type(parser, idt, memoryPath, iout)
    type(BlockParserType), intent(inout) :: parser
    type(InputDefinitionType), intent(in) :: idt
    character(len=*), intent(in) :: memoryPath
    integer(I4B), intent(in) :: iout
    character(len=LINELENGTH), pointer :: cstr
    integer(I4B) :: ilen
    ilen = LINELENGTH
    call mem_allocate(cstr, ilen, idt%mf6varname, memoryPath)
    call parser%GetStringCaps(cstr)
    return
  end subroutine load_string_type
  
  subroutine load_integer_type(parser, idt, memoryPath, iout)
    type(BlockParserType), intent(inout) :: parser
    type(InputDefinitionType), intent(in) :: idt
    character(len=*), intent(in) :: memoryPath
    integer(I4B), intent(in) :: iout
    integer(I4B), pointer :: intvar
    call mem_allocate(intvar, idt%mf6varname, memoryPath)
    intvar = parser%GetInteger()
    return
  end subroutine load_integer_type
  
  subroutine load_integer1d_type(parser, idt, memoryPath, iout)
    type(BlockParserType), intent(inout) :: parser
    type(InputDefinitionType), intent(in) :: idt
    character(len=*), intent(in) :: memoryPath
    integer(I4B), intent(in) :: iout
    integer(I4B), dimension(:), pointer, contiguous :: int1d
    integer(I4B), pointer :: nsize1
    integer(I4B) :: nvals
    integer(I4B), dimension(:), contiguous, pointer :: mshape
    
    if (idt%shape == 'NODES') then
      call mem_setptr(mshape, 'MODEL_SHAPE', 'MYMODEL') !todo: harwired; memoryPath)
      nvals = product(mshape)
      call mem_allocate(int1d, nvals, idt%mf6varname, memoryPath)
    else
      call mem_setptr(nsize1, idt%shape, memoryPath)
      call mem_allocate(int1d, nsize1, idt%mf6varname, memoryPath)
    end if
    
    call read_grid_array(parser, mshape, idt%tagname, intarray=int1d)
    
    return
  end subroutine load_integer1d_type

  subroutine load_integer3d_type(parser, idt, memoryPath, iout)
    type(BlockParserType), intent(inout) :: parser
    type(InputDefinitionType), intent(in) :: idt
    character(len=*), intent(in) :: memoryPath
    integer(I4B), intent(in) :: iout
    integer(I4B), dimension(:, :, :), pointer, contiguous :: int3d
    integer(I4B), pointer :: nsize1
    integer(I4B), pointer :: nsize2
    integer(I4B), pointer :: nsize3
    integer(I4B) :: ndim = 3
    integer(I4B) :: nwords
    character(len=16), dimension(:), allocatable :: words
    character(len=LINELENGTH) :: keyword
    
    ! split shape into three sizes
    call parseline(idt%shape, nwords, words)
    
    ! find sizes in memory manager
    call mem_setptr(nsize1, words(1), memoryPath)
    call mem_setptr(nsize2, words(2), memoryPath)
    call mem_setptr(nsize3, words(3), memoryPath)
    
    ! allocate the array using the memory manager
    call mem_allocate(int3d, nsize1, nsize2, nsize3, idt%mf6varname, memoryPath)
    
    ! fill the array from the file
    if (idt%blockname == 'GRIDDATA') then
      call parser%GetStringCaps(keyword)
      if (keyword == 'LAYERED') then
        ! read by layer
        call ReadArray(parser%iuactive, int3d(:,:,:), &
                       idt%mf6varname, ndim, nsize1, nsize2, &
                       nsize3, iout, 1, nsize3)
      else
        ! read full 3d array
        call ReadArray(parser%iuactive, int3d(:,:,:), idt%mf6varname, &
                       ndim, nsize1 * nsize2 * nsize3, iout)
      end if
    else
      ! read full 3d array
      call ReadArray(parser%iuactive, int3d(:,:,:), idt%mf6varname, &
                     ndim, nsize1 * nsize2 * nsize3, iout)
    end if
    
    return
  end subroutine load_integer3d_type
  
  subroutine load_double_type(parser, idt, memoryPath, iout)
    type(BlockParserType), intent(inout) :: parser
    type(InputDefinitionType), intent(in) :: idt
    character(len=*), intent(in) :: memoryPath
    integer(I4B), intent(in) :: iout
    real(DP), pointer :: dblvar
    call mem_allocate(dblvar, idt%mf6varname, memoryPath)
    dblvar = parser%GetDouble()
    return
  end subroutine load_double_type
  
  subroutine load_double1d_type(parser, idt, memoryPath, iout)
    type(BlockParserType), intent(inout) :: parser
    type(InputDefinitionType), intent(in) :: idt
    character(len=*), intent(in) :: memoryPath
    integer(I4B), intent(in) :: iout
    real(DP), dimension(:), pointer, contiguous :: dbl1d
    integer(I4B), pointer :: nsize1
    integer(I4B) :: ndim = 1
    integer(I4B) :: nvals
    integer(I4B), dimension(:), contiguous, pointer :: mshape
    
    if (idt%shape == 'NODES') then
      ! todo: 'MYMODEL' needs to be replaced with component
      call mem_setptr(mshape, 'MODEL_SHAPE', 'MYMODEL') !todo: harwired; memoryPath)
      nvals = product(mshape)
      call mem_allocate(dbl1d, nvals, idt%mf6varname, memoryPath)
    else
      call mem_setptr(nsize1, idt%shape, memoryPath)
      call mem_allocate(dbl1d, nsize1, idt%mf6varname, memoryPath)
      allocate(mshape(1))
      mshape(1) = nsize1
    end if
    
    call read_grid_array(parser, mshape, idt%tagname, dbl1d)
    
    !call ReadArray(parser%iuactive, dbl1d, idt%mf6varname, &
    !               ndim, nsize1, iout, 0)
    
    
    return
  end subroutine load_double1d_type
  
  subroutine load_double2d_type(parser, idt, memoryPath, iout)
    type(BlockParserType), intent(inout) :: parser
    type(InputDefinitionType), intent(in) :: idt
    character(len=*), intent(in) :: memoryPath
    integer(I4B), intent(in) :: iout
    real(DP), dimension(:, :), pointer, contiguous :: dbl2d
    integer(I4B), pointer :: nsize1
    integer(I4B), pointer :: nsize2
    integer(I4B) :: ndim = 2
    integer(I4B) :: nwords
    character(len=16), dimension(:), allocatable :: words
    
    ! split shape into two sizes
    call parseline(idt%shape, nwords, words)
    
    ! find sizes in memory manager
    call mem_setptr(nsize1, words(1), memoryPath)
    call mem_setptr(nsize2, words(2), memoryPath)
    
    ! allocate the array using the memory manager
    call mem_allocate(dbl2d, nsize1, nsize2, idt%mf6varname, memoryPath)
    
    ! fill the array from the file
    call ReadArray(parser%iuactive, dbl2d, idt%mf6varname, &
                   ndim, nsize1, nsize2, iout, 0)
    
    return
  end subroutine load_double2d_type
  
  subroutine load_double3d_type(parser, idt, memoryPath, iout)
    type(BlockParserType), intent(inout) :: parser
    type(InputDefinitionType), intent(in) :: idt
    character(len=*), intent(in) :: memoryPath
    integer(I4B), intent(in) :: iout
    real(DP), dimension(:, :, :), pointer, contiguous :: dbl3d
    integer(I4B), pointer :: nsize1
    integer(I4B), pointer :: nsize2
    integer(I4B), pointer :: nsize3
    integer(I4B) :: ndim = 3
    integer(I4B) :: nwords
    character(len=16), dimension(:), allocatable :: words
    character(len=LINELENGTH) :: keyword
    
    ! split shape into three sizes
    call parseline(idt%shape, nwords, words)
    
    ! find sizes in memory manager
    call mem_setptr(nsize1, words(1), memoryPath)
    call mem_setptr(nsize2, words(2), memoryPath)
    call mem_setptr(nsize3, words(3), memoryPath)
    
    ! allocate the array using the memory manager
    call mem_allocate(dbl3d, nsize1, nsize2, nsize3, idt%mf6varname, memoryPath)
    
    ! fill the array from the file
    if (idt%blockname == 'GRIDDATA') then
      call parser%GetStringCaps(keyword)
      if (keyword == 'LAYERED') then
        ! read by layer
        call ReadArray(parser%iuactive, dbl3d(:,:,:), &
                        idt%mf6varname, ndim, nsize1, nsize2, &
                        nsize3, iout, 1, nsize3)
      else
        ! read full 3d array
        call ReadArray(parser%iuactive, dbl3d(:,:,:), idt%mf6varname, &
                       ndim, nsize1 * nsize2 * nsize3, iout)
      end if
    else
      ! read full 3d array
      call ReadArray(parser%iuactive, dbl3d(:,:,:), idt%mf6varname, &
                     ndim, nsize1 * nsize2 * nsize3, iout)
    end if
    
    return
  end subroutine load_double3d_type
  
  function get_input_definition_type(input_definition_types, &
      component_type, subcomponent_type, tagname) result (idt)
    type(InputDefinitionType), dimension(:), intent(in), target :: input_definition_types
    character(len=*), intent(in) :: component_type
    character(len=*), intent(in) :: subcomponent_type
    character(len=*), intent(in) :: tagname
    type(InputDefinitionType), pointer :: idt
    type(InputDefinitionType), pointer :: tmp_ptr
    integer(I4B) :: i
    
    idt => null()
    do i = 1, size(input_definition_types)
      tmp_ptr => input_definition_types(i)
      if (tmp_ptr%component_type == component_type .and. &
          tmp_ptr%subcomponent_type == subcomponent_type .and. &
          tmp_ptr%tagname == tagname) then
        idt => input_definition_types(i)
        exit
      end if
    end do
    
  end function get_input_definition_type

  subroutine set_model_shape(ftype, component_name, memoryPath)
    character(len=*) :: ftype
    character(len=*), intent(in) :: component_name
    character(len=LENMEMPATH), intent(in) :: memoryPath
    integer(I4B), dimension(:), pointer, contiguous :: model_shape
    integer(I4B), pointer :: ndim1
    integer(I4B), pointer :: ndim2
    integer(I4B), pointer :: ndim3
    
    select case (ftype)
    case ('DIS6')
      call mem_allocate(model_shape, 3, 'MODEL_SHAPE', component_name)
      call mem_setptr(ndim1, 'NLAY', memoryPath)
      call mem_setptr(ndim2, 'NROW', memoryPath)
      call mem_setptr(ndim3, 'NCOL', memoryPath)
      model_shape = [ndim1, ndim2, ndim3]
    case ('DISV6')
      call mem_allocate(model_shape, 2, 'MODEL_SHAPE', component_name)
      call mem_setptr(ndim1, 'NLAY', memoryPath)
      call mem_setptr(ndim2, 'NCPL', memoryPath)
      model_shape = [ndim1, ndim2]
    case ('DISU6')
      call mem_allocate(model_shape, 1, 'MODEL_SHAPE', component_name)
      call mem_setptr(ndim1, 'NODES', memoryPath)
      model_shape = [ndim1]
    end select
    
    return
  end subroutine set_model_shape
    
  function is_block_required(input_definition_types, &
      component_type, subcomponent_type, blockname) result (blockRequired)
    type(InputDefinitionType), dimension(:), intent(in), target :: input_definition_types
    character(len=*), intent(in) :: component_type
    character(len=*), intent(in) :: subcomponent_type
    character(len=*), intent(in) :: blockname
    logical(LGP) :: blockRequired
    type(InputDefinitionType), pointer :: idt
    type(InputDefinitionType), pointer :: tmp_ptr
    integer(I4B) :: i
    
    idt => null()
    do i = 1, size(input_definition_types)
      tmp_ptr => input_definition_types(i)
      if (tmp_ptr%component_type == component_type .and. &
          tmp_ptr%subcomponent_type == subcomponent_type .and. &
          tmp_ptr%blockname == blockname .and. &
          tmp_ptr%tagname == blockname) then
        idt => input_definition_types(i)
        exit
      end if
    end do
    
    if (associated(idt)) then
      blockRequired = idt%required
    else
      write(errmsg, '(4x,a,a)') 'Programming error determing if block is required: ', trim(blockname)
      call store_error(errmsg, terminate=.true.)
    end if
    
  end function is_block_required

  function support_open_close(input_definition_types, &
      component_type, subcomponent_type, blockname) result (supportOpenClose)
    type(InputDefinitionType), dimension(:), intent(in), target :: input_definition_types
    character(len=*), intent(in) :: component_type
    character(len=*), intent(in) :: subcomponent_type
    character(len=*), intent(in) :: blockname
    logical(LGP) :: supportOpenClose
    if (blockname == 'GRIDDATA') then
      supportOpenClose = .false.
    else
      supportOpenClose = .true.
    end if
    return
  end function support_open_close

  function is_structarray_block(input_definition_types, &
      component_type, subcomponent_type, blockname) result (isStructArray)
    type(InputDefinitionType), dimension(:), intent(in), target :: input_definition_types
    character(len=*), intent(in) :: component_type
    character(len=*), intent(in) :: subcomponent_type
    character(len=*), intent(in) :: blockname
    logical(LGP) :: isStructArray
    type(InputDefinitionType), pointer :: idt
    type(InputDefinitionType), pointer :: tmp_ptr
    integer(I4B) :: i
    
    idt => null()
    do i = 1, size(input_definition_types)
      tmp_ptr => input_definition_types(i)
      if (tmp_ptr%component_type == component_type .and. &
          tmp_ptr%subcomponent_type == subcomponent_type .and. &
          tmp_ptr%blockname == blockname .and. &
          tmp_ptr%tagname == blockname) then
        idt => input_definition_types(i)
        exit
      end if
    end do
    
    if (associated(idt)) then
      if (idt%mf6varname(1:11) == 'STRUCTARRAY') then
        isStructArray = .true.
      else
        isStructArray = .false.
      end if
    else
      write(errmsg, '(4x,a,a)') 'Programming error determing block properties: ', trim(blockname)
      call store_error(errmsg, terminate=.true.)
    end if
  
    return
  end function is_structarray_block

  subroutine get_unique_blocknames(input_definition_types, &
      component_type, subcomponent_type, unique_blocknames)
    type(InputDefinitionType), dimension(:), intent(in), target :: input_definition_types
    character(len=*), intent(in) :: component_type
    character(len=*), intent(in) :: subcomponent_type
    character(len=100), dimension(:), allocatable :: unique_blocknames
    character(len=100) :: lastblock
    
    type(InputDefinitionType), pointer :: idt
    type(InputDefinitionType), pointer :: tmp_ptr
    integer(I4B) :: i
    integer(I4B) :: nblock
    
    ! first pass to count number of unique block names
    nblock = 0
    lastblock = ''
    idt => null()
    do i = 1, size(input_definition_types)
      tmp_ptr => input_definition_types(i)
      if (tmp_ptr%component_type == component_type .and. &
          tmp_ptr%subcomponent_type == subcomponent_type) then
        idt => input_definition_types(i)
        if (idt%blockname /= lastblock) then
          nblock = nblock + 1
          lastblock = idt%blockname
        end if
      end if
    end do
    
    ! second pass to allocate and store unique block names
    if (allocated(unique_blocknames)) then
      deallocate(unique_blocknames)
    end if
    allocate(unique_blocknames(nblock))
    nblock = 0
    lastblock = ''
    idt => null()
    do i = 1, size(input_definition_types)
      tmp_ptr => input_definition_types(i)
      if (tmp_ptr%component_type == component_type .and. &
          tmp_ptr%subcomponent_type == subcomponent_type) then
        idt => input_definition_types(i)
        if (idt%blockname /= lastblock) then
          nblock = nblock + 1
          unique_blocknames(nblock) = idt%blockname
          lastblock = idt%blockname
        end if
      end if
    end do
  end subroutine get_unique_blocknames

  subroutine read_grid_array(parser, mshape, array_name, dblarray, intarray)
    type(BlockParserType), intent(inout) :: parser
    integer(I4B), dimension(:), intent(in) :: mshape
    character(len=*), intent(in) :: array_name
    real(DP), dimension(:), optional, intent(inout) :: dblarray
    integer(I4B), dimension(:), optional, intent(inout) :: intarray
    character(len=LINELENGTH) :: keyword
    integer(I4B) :: nvals
    integer(I4B) :: ndim
    integer(I4B) :: ndim1
    integer(I4B) :: ndim2
    integer(I4B) :: ndim3
    integer(I4B) :: k1
    integer(I4B) :: k2
    integer(I4B) :: iout
    
    ndim = size(mshape)
    if (present(dblarray)) then
      nvals = size(dblarray)
    end if
    if (present(intarray)) then
      nvals = size(intarray)
    end if
    iout = 0
    
    ! disu
    if (ndim == 1) then
      ndim1 = mshape(1)  ! nodesuser
      ndim2 = 1          ! none
      ndim3 = 1          ! none
      k1 = 0
      k2 = 0
      
    ! disv
    else if (ndim == 2) then
      ndim1 = mshape(1)  ! nlay
      ndim2 = 1          ! none
      ndim3 = mshape(2)  ! ncpl
      k1 = 1
      k2 = ndim1
      
    ! dis
    else if (ndim == 3) then
      ndim1 = mshape(1)  ! nlay
      ndim2 = mshape(2)  ! nrow
      ndim3 = mshape(3)  ! ncol
      k1 = 1
      k2 = ndim1
    end if
    
    call parser%GetStringCaps(keyword)
    if (keyword == 'LAYERED') then

      ! float array
      if (present(dblarray)) then
        call ReadArray(parser%iuactive, dblarray, &
                       array_name, ndim, ndim3, ndim2, &
                       ndim1, nvals, iout, k1, k2)
      end if

      ! integer array
      if (present(intarray)) then
        call ReadArray(parser%iuactive, intarray, &
                       array_name, ndim, ndim3, ndim2, &
                       ndim1, nvals, iout, k1, k2)
      end if
      
      
    else
      
      ! float array
      if (present(dblarray)) then
        call ReadArray(parser%iuactive, dblarray, array_name, &
                       ndim, nvals, iout, 0)
      end if

      ! integer array
      if (present(intarray)) then
        call ReadArray(parser%iuactive, intarray, array_name, &
                       ndim, nvals, iout, 0)
      end if
      
      
    end if

    return
  end subroutine read_grid_array
  
      
end module LoadFromFileModule
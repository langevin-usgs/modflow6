module StructArrayModule
  
  use KindModule, only: I4B, DP, LGP
  use ConstantsModule, only: DNODATA
  use StructVectorModule, only: StructVectorType
  use MemoryManagerModule, only: mem_allocate
  implicit none
  private
  public :: StructArrayType
  public :: constructStructArray
  
  type StructArrayType
    
    integer(I4B) :: ncol
    integer(I4B) :: nrow
    type(StructVectorType), dimension(:), allocatable :: struct_vector_1d

  contains
    procedure :: mem_create_vector
    procedure :: add_vector_int1d
    procedure :: add_vector_dbl1d
    procedure :: add_vector_str1d
    procedure :: read_from_parser
    
  end type StructArrayType
  
  contains
  
  function constructStructArray(ncol, nrow) result(struct_array)
    integer(I4B), intent(in) :: ncol
    integer(I4B), intent(in) :: nrow
    type(StructArrayType), pointer :: struct_array
    
    allocate(struct_array)
    struct_array%ncol = ncol
    struct_array%nrow = nrow
    allocate(struct_array%struct_vector_1d(ncol))
  
  end function constructStructArray

  subroutine mem_create_vector(this, icol, vartype, nrow, name, memoryPath)
    class(StructArrayType) :: this
    integer(I4B), intent(in) :: icol
    integer(I4B), intent(in) :: nrow
    character(len=*), intent(in) :: vartype
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: memoryPath
    integer(I4B), dimension(:), pointer, contiguous :: int1d
    real(DP), dimension(:), pointer, contiguous :: dbl1d
    integer(I4B) :: j
    integer(I4B) :: inodata = 999  !todo: create INODATA in constants?

    select case (vartype)
    case ('INTEGER1D')
      call mem_allocate(int1d, nrow, name, memoryPath)
      do j = 1, nrow
        int1d(j) = inodata
      end do
      call this%add_vector_int1d(icol, int1d)
    case ('DOUBLE1D')
      call mem_allocate(dbl1d, nrow, name, memoryPath)
      do j = 1, nrow
        dbl1d(j) = DNODATA
      end do
      call this%add_vector_dbl1d(icol, dbl1d)
    end select

    return
  end subroutine mem_create_vector
  
  subroutine add_vector_int1d(this, icol, int1d)
    class(StructArrayType) :: this
    integer(I4B), intent(in) :: icol
    integer(I4B), dimension(:), pointer, contiguous, intent(in) :: int1d
    type(StructVectorType) :: sv
    sv%memtype = 1
    sv%int1d => int1d
    this%struct_vector_1d(icol) = sv
    return
  end subroutine add_vector_int1d
  
  subroutine add_vector_dbl1d(this, icol, dbl1d)
    class(StructArrayType) :: this
    integer(I4B), intent(in) :: icol
    real(DP), dimension(:), pointer, contiguous, intent(in) :: dbl1d
    type(StructVectorType) :: sv
    sv%memtype = 2
    sv%dbl1d => dbl1d
    this%struct_vector_1d(icol) = sv
    return
  end subroutine add_vector_dbl1d
  
  subroutine add_vector_str1d(this, icol, str1d)
    class(StructArrayType) :: this
    integer(I4B), intent(in) :: icol
    character(len=:), dimension(:), pointer, contiguous, intent(in) :: str1d
    type(StructVectorType) :: sv
    sv%memtype = 3
    sv%str1d => str1d
    this%struct_vector_1d(icol) = sv
    return
  end subroutine add_vector_str1d
  
  subroutine read_from_parser(this, parser)
    use BlockParserModule, only: BlockParserType
    class(StructArrayType) :: this
    type(BlockParserType) :: parser
    logical(LGP) :: endOfBlock
    integer(I4B) :: i
    integer(I4B) :: j
    do i = 1, this%nrow
      call parser%GetNextLine(endOfBlock)
      if (endOfBlock) exit
      do j = 1, this%ncol
        select case (this%struct_vector_1d(j)%memtype)
        case (1)
          this%struct_vector_1d(j)%int1d(i) = parser%GetInteger()
        case (2)
          this%struct_vector_1d(j)%dbl1d(i) = parser%GetDouble()
        case (3)
          call parser%GetStringCaps(this%struct_vector_1d(j)%str1d(i))
        end select
      end do
    end do
    return
  end subroutine read_from_parser
  
end module StructArrayModule
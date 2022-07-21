module StructVectorModule

  use KindModule, only: I4B, DP, LGP
  implicit none
  private
  public :: StructVectorType

  type StructVectorType
    integer(I4B) :: memtype = 0
    integer(I4B), dimension(:), pointer, contiguous :: int1d => null()
    real(DP), dimension(:), pointer, contiguous :: dbl1d => null()
    character(len=:), dimension(:), pointer, contiguous :: str1d => null()
  end type StructVectorType

contains

end module StructVectorModule

module castings

  implicit none

contains
 
 subroutine castSidlDouble2DToSimple2D(sidlArray, simpleArray)
  use sidl_double_array
 
  type(sidl_double_2d), intent(in) :: sidlArray
  real, dimension(:,:), intent(out) :: simpleArray
  integer horizontal_size, vertical_size
  double precision value
  integer hor, ver

  horizontal_size =  length(sidlArray, 0)
  vertical_size = length(sidlArray, 1)
  if (horizontal_size*vertical_size.ne.size(simpleArray)) then 
    print*, "Different sizes (castSidlDouble2DToSimple2D)! Size of sidlArray:", &
    horizontal_size, vertical_size, &
    " Size of simpleArray:", size(simpleArray)
  endif
  
  simpleArray(:,:) = 0.0
  do hor = 1, horizontal_size
    do ver = 1, vertical_size
	  call sidl_double__array_get2_m(sidlArray,hor-1, ver-1, value)
	  simpleArray(hor, ver) = value
    enddo
  enddo

 end subroutine castSidlDouble2DToSimple2D

!------------------------------------------------------------------ 
 subroutine castSimple2DtoSidlDouble2D(simpleArray, sidlArray)
  use sidl_double_array
 
  type(sidl_double_2d), intent(out) :: sidlArray
  real, dimension(:,:), intent(in) :: simpleArray
  integer horizontal_size, vertical_size
  double precision value
  integer hor, ver

  horizontal_size =  length(sidlArray, 0)
  vertical_size = length(sidlArray, 1)
  if (horizontal_size*vertical_size.ne.size(simpleArray)) then 
    print*, "Different sizes (castSimple2DtoSidlDouble2D)! Size of sidlArray:", &
    horizontal_size, vertical_size, &
    " Size of simpleArray:", size(simpleArray)
  endif
  
  do hor = 1, horizontal_size
    do ver = 1, vertical_size
	  value = simpleArray(hor, ver)
	  call sidl_double__array_set2_m(sidlArray,hor-1, ver-1, value)
    enddo
  enddo
 end subroutine castSimple2DtoSidlDouble2D

!------------------------------------------------------------------ 
 subroutine castSidlDouble1DToSimple1D(sidlArray, simpleArray)
  use sidl_double_array
 
  type(sidl_double_1d), intent(in) :: sidlArray
  real, dimension(:), intent(out) :: simpleArray
  integer horizontal_size
  double precision value
  integer hor
  
  horizontal_size =  length(sidlArray, 0)
  if (horizontal_size.ne.size(simpleArray)) then 
    print*, "Different sizes (castSidlDouble1DToSimple1D)! Size of sidlArray:", horizontal_size, &
    " Size of simpleArray:", size(simpleArray)
  endif
  
  simpleArray(:) = 0.0
  do hor = 1, horizontal_size
	  call sidl_double__array_get1_m(sidlArray, hor-1, value)
	  simpleArray(hor) = value
  enddo

 end subroutine castSidlDouble1DToSimple1D

!------------------------------------------------------------------ 
 subroutine castSimple1DtoSidlDouble1D(simpleArray, sidlArray)
  use sidl_double_array
 
  type(sidl_double_1d), intent(out) :: sidlArray
  real, dimension(:), intent(in) :: simpleArray
  integer horizontal_size
  double precision value
  integer hor
  
  horizontal_size =  length(sidlArray, 0)
  if (horizontal_size.ne.size(simpleArray)) then 
    print*, "Different sizes (castSimple1DtoSidlDouble1D)! Size of sidlArray:", horizontal_size, &
    " Size of simpleArray:", size(simpleArray)
  endif

  do hor = 1, horizontal_size
	  value = simpleArray(hor)
	  call sidl_double__array_set1_m(sidlArray, hor-1, value)
  enddo
  
 end subroutine castSimple1DtoSidlDouble1D

!------------------------------------------------------------------ 
 subroutine cast0Simple1DtoSidlDouble1D(simpleArray, sidlArray)
  use sidl_double_array
 
  type(sidl_double_1d), intent(out) :: sidlArray
  real, dimension(:), intent(in) :: simpleArray
  integer horizontal_size
  double precision value
  integer hor
  
  horizontal_size =  length(sidlArray, 0)
  if (horizontal_size.ne.size(simpleArray)) then 
    print*, "Different sizes (castSimple1DtoSidlDouble1D)! Size of sidlArray:", horizontal_size, &
    " Size of simpleArray:", size(simpleArray)
  endif

  do hor = 0, horizontal_size
	  value = simpleArray(hor)
	  call sidl_double__array_set1_m(sidlArray, hor, value)
  enddo
  
 end subroutine cast0Simple1DtoSidlDouble1D
 
!------------------------------------------------------------------ 
 subroutine castSidlInt1DToSimple1D(sidlArray, simpleArray)
  use sidl_int_array
 
  type(sidl_int_1d), intent(in) :: sidlArray
  integer, dimension(:), intent(out) :: simpleArray
  integer horizontal_size
  integer value
  integer hor
  
  horizontal_size =  length(sidlArray, 0)
  if (horizontal_size.ne.size(simpleArray)) then 
    print*, "Different sizes (castSidlInt1DToSimple1D)! Size of sidlArray:", horizontal_size, &
    " Size of simpleArray:", size(simpleArray)
  endif
  
  simpleArray(:) = 0
  do hor = 1, horizontal_size
	  call sidl_int__array_get1_m(sidlArray,hor-1, value)
	  simpleArray(hor) = value
  enddo

 end subroutine castSidlInt1DToSimple1D

!------------------------------------------------------------------ 
 subroutine castSimple1DtoSidlInt1D(simpleArray, sidlArray)
  use sidl_int_array
 
  type(sidl_int_1d), intent(out) :: sidlArray
  integer, dimension(:), intent(in) :: simpleArray
  integer horizontal_size
  integer value
  integer hor
  
  horizontal_size =  length(sidlArray, 0)
  if (horizontal_size.ne.size(simpleArray)) then 
    print*, "Different sizes (castSimple1DtoSidlInt1D)! Size of sidlArray:", &
    horizontal_size, &
    " Size of simpleArray:", size(simpleArray)
  endif
  
  do hor = 1, horizontal_size
	  value = simpleArray(hor)
	  call sidl_int__array_set1_m(sidlArray, hor-1, value)
  enddo
 end subroutine castSimple1DtoSidlInt1D 

!------------------------------------------------------------------ 
 subroutine castSimple2DtoSidlInt2D(simpleArray, sidlArray)
  use sidl_int_array
 
  type(sidl_int_2d), intent(out) :: sidlArray
  integer, dimension(:,:), intent(in) :: simpleArray
  integer horizontal_size, vertical_size
  integer value
  integer hor, ver
  
  horizontal_size =  length(sidlArray, 0)
  vertical_size = length(sidlArray, 1)
  if (horizontal_size*vertical_size.ne.size(simpleArray)) then 
    print*, "Different sizes (castSimple2DtoSidlInt2D)! Size of sidlArray:", &
    horizontal_size, vertical_size, &
    " Size of simpleArray:", size(simpleArray)
  endif
  
  do hor = 1, horizontal_size
    do ver = 1, vertical_size
	  value = simpleArray(hor, ver)
	  call sidl_int__array_set2_m(sidlArray,hor-1, ver-1, value)
    enddo
  enddo
 end subroutine castSimple2DtoSidlInt2D

!------------------------------------------------------------------ 
 subroutine castSimple3DtoSidlDouble3D(simpleArray, sidlArray)
  use sidl_double_array
 
  type(sidl_double_3d), intent(out) :: sidlArray
  real, dimension(:,:,:), intent(in) :: simpleArray
  integer first_dim, second_dim, third_dim
  double precision value
  integer first, second, third
  
  first_dim  = length(sidlArray, 0)
  second_dim = length(sidlArray, 1)
  third_dim  = length(sidlArray, 2)
  if (first_dim*second_dim*third_dim.ne.size(simpleArray)) then 
    print*, "Different sizes (castSimple3DtoSidlDouble3D)! Size of sidlArray:", &
    first_dim, second_dim, third_dim, &
    " Size of simpleArray:", size(simpleArray)
  endif

  do first = 1, first_dim
    do second = 1, second_dim
      do third = 1, third_dim
	  value = simpleArray(first, second, third)
	  call sidl_double__array_set3_m(sidlArray, first-1, second-1, third-1, value)
      enddo
    enddo
  enddo
  
 end subroutine castSimple3DtoSidlDouble3D

!------------------------------------------------------------------ 
 subroutine castSimple4DtoSidlDouble4D(simpleArray, sidlArray)
  use sidl_double_array
 
  type(sidl_double_4d), intent(out) :: sidlArray
  real, dimension(:,:,:,:), intent(in) :: simpleArray
  integer first_dim, second_dim, third_dim, forth_dim
  double precision value
  integer first, second, third, forth
  
  first_dim  = length(sidlArray, 0)
  second_dim = length(sidlArray, 1)
  third_dim  = length(sidlArray, 2)
  forth_dim  = length(sidlArray, 3)
  if (first_dim*second_dim*third_dim*forth_dim.ne.size(simpleArray)) then 
    print*, "Different sizes (castSimple4DtoSidlDouble4D)! Size of sidlArray:", &
    first_dim, second_dim, third_dim, forth_dim, &
    " Size of simpleArray:", size(simpleArray)
  endif

  do first = 1, first_dim
    do second = 1, second_dim
      do third = 1, third_dim
        do forth = 1, forth_dim
	  value = simpleArray(first, second, third, forth)
	  call sidl_double__array_set4_m(sidlArray, first-1, second-1, third-1, forth-1, value)
        enddo
      enddo
    enddo
  enddo
  
 end subroutine castSimple4DtoSidlDouble4D

!------------------------------------------------------------------ 
 
 subroutine copy(array1, array2)
 
  real, dimension(:, :), intent(in)  :: array1
  real, dimension(:, :), intent(out) :: array2
  integer dim1, dim2, i, j
  
  if ((size(shape(array1)).ne.size(shape(array2))).or.(size(shape(array1)).gt.2)) then
    print*, "Inappropriate size"
  else

  dim1 = min(size(array1, 1), size(array2, 1))
  dim2 = min(size(array1, 2), size(array2, 2))
  
  do i = 1, dim1
    do j = 1, dim2
      array2(i, j) = array1(i, j)
    enddo
  enddo
  
  do i = dim1+1, size(array2, 1)
    do j = 1, size(array2, 2)
      array2(i, j) = 0.0
    enddo
  enddo

  do j = dim2+1, size(array2, 2)
    do i = 1, size(array2, 1)
      array2(i, j) = 0.0
    enddo
  enddo

 endif
 
 end subroutine copy

 
end module castings

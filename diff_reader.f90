program diff_reader

! total number of grid points in each direction of the grid

integer, parameter :: NX = 641
integer, parameter :: NY = 321  !NY = 800

integer :: i,j

double precision, dimension(-4:NX+4,-4:NY+4) :: vx,vy
double precision, parameter :: ZERO = 0.d0


open(unit=1,file='dataset.dat',status='old',action='read',form='unformatted')
read(1) vy
close(unit=1)

!open(unit=2,file='converted_dataset.txt',form='formatted',status='replace', action='write')
!write(unit=2, fmt=*) vy
!close(2)

do i=1,NX
  do j=1,NY
    if(vy(i,j).ne.ZERO)  write(*,*) vy(i,j), i,j
  enddo
enddo

end program diff_reader
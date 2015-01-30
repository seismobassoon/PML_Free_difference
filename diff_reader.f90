program diff_reader

! total number of grid points in each direction of the grid

  integer, parameter :: NX = 641
  integer, parameter :: NY = 321  !NY = 800

! thickness of the PML layer in grid points
  integer, parameter :: NPOINTS_PML = 10

! heterogeneous model and height of the interface
  logical, parameter :: HETEROGENEOUS_MODEL = .true.

! source
! integer, parameter :: ISOURCE = NX - 2*NPOINTS_PML - 1
  integer, parameter :: ISOURCE = NPOINTS_PML+11
  integer, parameter :: JSOURCE = 2*NY / 3

  integer :: i,j,iunit, irec

  double precision, dimension(-4:NX+4,-4:NY+4) :: vy,vx_pml,vy_pml
  double precision, dimension(-4:NX+4,-4:NY+4) :: vx_free,vy_free
  double precision, parameter :: ZERO = 0.d0

  integer, parameter :: NREC = 3
  integer, dimension(NREC) :: ix_rec,iy_rec
! flags to add PML layers to the edges of the grid
  logical, parameter :: USE_PML_XMIN = .false.
  logical, parameter :: USE_PML_XMAX = .false.
  logical, parameter :: USE_PML_YMIN = .false.
  logical, parameter :: USE_PML_YMAX = .false.

  double precision max_amplitudeVy

  double precision, parameter :: DELTAX = 5.d0 !, ONE_OVER_DELTAX = 1.d0 / DELTAX
  double precision, parameter :: DELTAY = DELTAX
  double precision, parameter :: xsource = (ISOURCE) * DELTAX
  double precision, parameter :: ysource = (JSOURCE) * DELTAY
  double precision, parameter :: INTERFACE_HEIGHT = ysource - 125*DELTAY
  integer, parameter:: JINTERFACE=INT(INTERFACE_HEIGHT/DELTAY)+1

  double precision, dimension(NREC) :: xrec,yrec

  ! large value for maximum
  double precision, parameter :: HUGEVAL = 1.d+30
  double precision dist, distval
  character(len=100) :: filename

! open(unit=1,file='dataset.dat',status='old',action='read',form='unformatted')
! read(1) vy
! close(unit=1)

!open(unit=2,file='converted_dataset.txt',form='formatted',status='replace', action='write')
!write(unit=2, fmt=*) vy
!close(2)

! do i=1,NX
!   do j=1,NY
!     if(vy(i,j).ne.ZERO)  write(*,*) vy(i,j), i,j
!   enddo
! enddo
  xrec(1) = xsource
  yrec(1) = ysource - 393*DELTAY
  xrec(2) = xsource
  yrec(2) = ysource + 191*DELTAY
  xrec(3) = xsource + 101*DELTAX
  yrec(3) = ysource

! find closest grid point for each receiver
  do irec=1,nrec
    dist = HUGEVAL
    do j = 1,NY
    do i = 1,NX
      distval = sqrt((DELTAX*dble(i) - xrec(irec))**2 + (DELTAY*dble(j) - yrec(irec))**2)
      if(distval < dist) then
        dist = distval
        ix_rec(irec) = i
        iy_rec(irec) = j
      endif
    enddo
    enddo
    !print *,'receiver ',irec,' x_target,y_target = ',xrec(irec),yrec(irec)
    !print *,'closest grid point found at distance ',dist,' in i,j = ',ix_rec(irec),iy_rec(irec)
    !print *
  enddo

!loop through all files dataset_pml_ and dataset_free_
 do i=1,100
   iunit=i*5	!set counter to fit names of files
   !pml
   write(*,*) iunit
   
   write(filename,"('dataset_pml_',i4.4,'.dat')")iunit
   OPEN(UNIT=iunit,FILE=filename,FORM="UNFORMATTED", STATUS="old",ACTION="read")
   read(iunit) vy_pml
   CLOSE(UNIT=iunit)

   !free
   write(filename,"('dataset_free_',i4.4,'.dat')")iunit
   OPEN(UNIT=iunit,FILE=filename,FORM="UNFORMATTED", STATUS="old",ACTION="read")
   read(iunit) vy_free
   CLOSE(UNIT=iunit)

   vy(:,:)=vy_free(:,:)-vy_pml(:,:)
   
   call create_color_image(vy(1:NX,1:NY),NX,NY,iunit,ISOURCE,JSOURCE,ix_rec,iy_rec,nrec, &
                         NPOINTS_PML,USE_PML_XMIN,USE_PML_XMAX,USE_PML_YMIN,USE_PML_YMAX,2,max_amplitudeVy,JINTERFACE)


 enddo

end program diff_reader




 subroutine create_color_image(image_data_2D,NX,NY,it,ISOURCE,JSOURCE,ix_rec,iy_rec,nrec, &
              NPOINTS_PML,USE_PML_LEFT,USE_PML_RIGHT,USE_PML_BOTTOM,USE_PML_TOP,field_number,max_amplitude,JINTERFACE)


  implicit none

! non linear display to enhance small amplitudes for graphics
  double precision, parameter :: POWER_DISPLAY = 0.30d0

! amplitude threshold above which we draw the color point
  double precision, parameter :: cutvect = 0.01d0

! use black or white background for points that are below the threshold
  logical, parameter :: WHITE_BACKGROUND = .true.

! size of cross and square in pixels drawn to represent the source and the receivers
  integer, parameter :: width_cross = 5, thickness_cross = 1, size_square = 3

  integer NX,NY,it,field_number,ISOURCE,JSOURCE,NPOINTS_PML,nrec
  logical USE_PML_LEFT,USE_PML_RIGHT,USE_PML_BOTTOM,USE_PML_TOP

  double precision, dimension(NX,NY) :: image_data_2D

  integer, dimension(nrec) :: ix_rec,iy_rec

  integer ix,iy,irec,JINTERFACE

  double precision max_amplitude

  character(len=100) file_name,system_command

  double precision normalized_value
  integer :: R, G, B

! open image file and create system command to convert image to more convenient format
! use the "convert" command from ImageMagick http://www.imagemagick.org
  if(field_number == 1) then
    write(file_name,"('image',i6.6,'_Vx.pnm')") it
    write(system_command,"('convert image',i6.6,'_Vx.pnm image',i6.6,'_Vx.gif ; rm image',i6.6,'_Vx.pnm')") it,it,it
  endif
  if(field_number == 2) then
    write(file_name,"('image',i6.6,'_Vy.pnm')") it
    write(system_command,"('convert image',i6.6,'_Vy.pnm image',i6.6,'_Vy.gif ; rm image',i6.6,'_Vy.pnm')") it,it,it
  endif
  if(field_number == 3) then
    write(file_name,"('image',i6.6,'_Vnorm.pnm')") it
    write(system_command,"('convert image',i6.6,'_Vnorm.pnm image',i6.6,'_Vnorm.gif ; rm image',i6.6,'_Vnorm.pnm')") it,it,it
  endif

  open(unit=27, file=file_name, status='unknown')

  write(27,"('P3')") ! write image in PNM P3 format

  write(27,*) NX,NY ! write image size
  write(27,*) '255' ! maximum value of each pixel color

! compute maximum amplitude
  max_amplitude = maxval(abs(image_data_2D))

! image starts in upper-left corner in PNM format
  do iy=NY,1,-1
    do ix=1,NX

! define data as vector component normalized to [-1:1] and rounded to nearest integer
! keeping in mind that amplitude can be negative
    normalized_value = image_data_2D(ix,iy) / max_amplitude

! suppress values that are outside [-1:+1] to avoid small edge effects
    if(normalized_value < -1.d0) normalized_value = -1.d0
    if(normalized_value > 1.d0) normalized_value = 1.d0

! draw an orange cross to represent the source
    if((ix >= ISOURCE - width_cross .and. ix <= ISOURCE + width_cross .and. &
        iy >= JSOURCE - thickness_cross .and. iy <= JSOURCE + thickness_cross) .or. &
       (ix >= ISOURCE - thickness_cross .and. ix <= ISOURCE + thickness_cross .and. &
        iy >= JSOURCE - width_cross .and. iy <= JSOURCE + width_cross)) then
      R = 255
      G = 157
      B = 0

! display two-pixel-thick black frame around the image
  else if(ix <= 2 .or. ix >= NX-1 .or. iy <= 2 .or. iy >= NY-1) then
      R = 0
      G = 0
      B = 0

! display edges of the PML layers
  else if((USE_PML_LEFT .and. ix == NPOINTS_PML) .or. &
          (USE_PML_RIGHT .and. ix == NX - NPOINTS_PML) .or. &
          (USE_PML_BOTTOM .and. iy == NPOINTS_PML) .or. &
          (USE_PML_TOP .and. iy == NY - NPOINTS_PML)) then
      R = 255
      G = 150
      B = 0
 else if(iy==JINTERFACE) then
        R = 0
        G = 0
        B = 0
! suppress all the values that are below the threshold
    else if(abs(image_data_2D(ix,iy)) <= max_amplitude * cutvect) then

! use a black or white background for points that are below the threshold
      if(WHITE_BACKGROUND) then
        R = 255
        G = 255
        B = 255
      else
        R = 0
        G = 0
        B = 0
      endif

! represent regular image points using red if value is positive, blue if negative
    else if(normalized_value >= 0.d0) then
      R = nint(255.d0*normalized_value**POWER_DISPLAY)
      G = 0
      B = 0
    else
      R = 0
      G = 0
      B = nint(255.d0*abs(normalized_value)**POWER_DISPLAY)
    endif

! draw a green square to represent the receivers
  do irec = 1,nrec
    if((ix >= ix_rec(irec) - size_square .and. ix <= ix_rec(irec) + size_square .and. &
        iy >= iy_rec(irec) - size_square .and. iy <= iy_rec(irec) + size_square) .or. &
       (ix >= ix_rec(irec) - size_square .and. ix <= ix_rec(irec) + size_square .and. &
        iy >= iy_rec(irec) - size_square .and. iy <= iy_rec(irec) + size_square)) then
! use dark green color
      R = 30
      G = 180
      B = 60
    endif
  enddo

! write color pixel
    write(27,"(i3,' ',i3,' ',i3)") R,G,B

    enddo
  enddo

! close file
  close(27)

! call the system to convert image to JPEG
! call system(system_command)

  end subroutine create_color_image
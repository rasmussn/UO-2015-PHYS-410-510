!! Need to read mask and USE IT...
!

program surfdata
  use netcdf
  implicit none

  integer,  parameter :: r8 = kind(1.0d0)
  real(r8), parameter :: eps_fact = 2._r8

  ! This is the name of the data file we will read. 
  character(len=*), parameter :: SURFDATA_FILE = "surfdata_test.nc"

  logical :: normalize

  integer :: lon, lat, nl, pft, nLand, nShort, nTall
  integer :: ncid, varid, dimid, numDims, numAttrs, nLat, nLon, nPFT
  integer :: varid_mask, varid_wetland, varid_lake, varid_urban, varid_glacier
  integer, dimension(nf90_max_var_dims) :: dimIDs

  integer , allocatable :: mask(:,:)
  real(r8), allocatable :: pct_pft(:,:,:)
  real(r8), allocatable :: pct_wetland(:,:), pct_lake(:,:), pct_urban(:,:), pct_glacier(:,:)
  real(r8)              :: total, factor, pctspec

  ! Open the file. NF90_NOWRITE tells netCDF we want read-only access to
  ! the file.

  call check( nf90_open(SURFDATA_FILE, NF90_WRITE, ncid) )

  ! checking for existence
  call check( nf90_inq_dimid(ncid, "lsmlon", dimid) )
  call check( nf90_inq_dimid(ncid, "lsmlat", dimid) )

  call check( nf90_inq_varid(ncid, "PFTDATA_MASK", varid_mask) )
  call check( nf90_inquire_variable(ncid, varid_mask, ndims=numDims, dimids=dimIDs) )
  call check( nf90_inquire_dimension(ncid, dimIDs(1), len=nLon) )
  call check( nf90_inquire_dimension(ncid, dimIDs(2), len=nLat) )

  call check( nf90_inq_varid(ncid, "PCT_WETLAND", varid_wetland) )
  call check( nf90_inquire_variable(ncid, varid_wetland, ndims=numDims, dimids=dimIDs) )
  call check( nf90_inquire_dimension(ncid, dimIDs(1), len=nLon) )
  call check( nf90_inquire_dimension(ncid, dimIDs(2), len=nLat) )

  call check( nf90_inq_varid(ncid, "PCT_LAKE", varid_lake) )
  call check( nf90_inquire_variable(ncid, varid_lake, ndims=numDims, dimids=dimIDs) )
  call check( nf90_inquire_dimension(ncid, dimIDs(1), len=nLon) )
  call check( nf90_inquire_dimension(ncid, dimIDs(2), len=nLat) )

  call check( nf90_inq_varid(ncid, "PCT_URBAN", varid_urban) )
  call check( nf90_inquire_variable(ncid, varid_urban, ndims=numDims, dimids=dimIDs) )
  call check( nf90_inquire_dimension(ncid, dimIDs(1), len=nLon) )
  call check( nf90_inquire_dimension(ncid, dimIDs(2), len=nLat) )
  if (numDims /= 2) then
     print *, "PCT_URBAN has more than 2 dimensions."
     stop
  end if

  call check( nf90_inq_varid(ncid, "PCT_GLACIER", varid_glacier) )
  call check( nf90_inquire_variable(ncid, varid_glacier, ndims=numDims, dimids=dimIDs) )
  call check( nf90_inquire_dimension(ncid, dimIDs(1), len=nLon) )
  call check( nf90_inquire_dimension(ncid, dimIDs(2), len=nLat) )

  call check( nf90_inq_varid(ncid, "PCT_PFT", varid) )
  call check( nf90_inquire_variable(ncid, varid, ndims=numDims, dimids=dimIDs) )

  !! Just to double check to make sure variable order is correct
  !
  call check( nf90_inq_dimid(ncid, "lsmlon", dimid) )
  call check( nf90_inq_dimid(ncid, "lsmlat", dimid) )
  call check( nf90_inq_dimid(ncid, "lsmpft", dimid) )

  call check( nf90_inquire_dimension(ncid, dimIDs(1), len=nLon) )
  call check( nf90_inquire_dimension(ncid, dimIDs(2), len=nLat) )
  call check( nf90_inquire_dimension(ncid, dimIDs(3), len=nPFT) )

  allocate( pct_pft(nLon,nLat,nPFT) )
  call check( nf90_get_var(ncid, varid, pct_pft) )

  allocate( mask(nLon,nLat) )
  call check( nf90_get_var(ncid, varid_mask, mask) )

  allocate( pct_wetland(nLon,nLat) )
  call check( nf90_get_var(ncid, varid_wetland, pct_wetland) )

  allocate( pct_lake(nLon,nLat) )
  call check( nf90_get_var(ncid, varid_lake, pct_lake) )

  allocate( pct_urban(nLon,nLat) )
  call check( nf90_get_var(ncid, varid_urban, pct_urban) )

  allocate( pct_glacier(nLon,nLat) )
  call check( nf90_get_var(ncid, varid_glacier, pct_glacier) )

  !! Normalize the percentages to 100
  !

  nLand = 0
  nShort = 0
  nTall = 0

  normalize = .true.

  !... I believe the problem is at (934,149) zero based (actually is (933,149) zero based)
  !  nl = 93308  ng = 172582
  !  pct_pft = {5.6934043770315634, 0, 0, 0, 0, 81.432455232035437, 0, 0, 1.6887125979816497, 0, 0, 3.5032933806183961, 5.4218562274550051, 0, 0, 0, 0}
  !

#define NORMALIZE_DATA

  do lat = 1, nLat
    do lon = 1, nLon
      nl = (lon-1) + (lat-1)*nLon
      if (mask(lon,lat) > 0) then
         total = 0._r8
!
! pct_urban is removed because it is really pct_urban_tot which is set to 0. if not multi-density
!     see line 715 in surfrdMod.F90:
!
! 715     write(iulog,*)'PCT_URBAN is not multi-density, pcturb set to 0'
!
!         pctspec =  pct_wetland(lon,lat) + pct_lake(lon,lat) + pct_urban(lon,lat) + pct_glacier(lon,lat)
!
         pctspec =  pct_wetland(lon,lat) + pct_lake(lon,lat) + pct_glacier(lon,lat)
         if (pctspec < 100._r8 * (1._r8 - eps_fact*epsilon(1._r8))) then
            total = sum(pct_pft(lon,lat,:))

            if (lon == 934 .and. lat == 150) then
               !            print *, lon, lat, nl
               normalize = .true.
            end if
            if (total > 0.0d0) then
               if (pctspec > 99.999999d0) then
                  print *, "HELP, pctspec too big for total > 0.0"
               end if
               total = total * 100._r8/(100._r8-pctspec)
               nLand = nLand + 1
               if (total < 99.99999d0) then
                  nShort = nShort + 1
#ifdef NORMALIZE_DATA
                  if (normalize) then
                     pct_pft(lon,lat,:) = pct_pft(lon,lat,:) / (total/100.0d0)
                     normalize = .true.  ! change all values
                  end if
#endif
               end if
               if (total > 100.00001d0) then
                  nTall = nTall + 1
               end if
            else
!               print *, "HELP, total == 0.0"
#ifdef NORMALIZE_DATA
               pct_pft(lon,lat,:)  = 0._r8
               pct_pft(lon,lat,1) = (100._r8-pctspec)

#endif
            end if
         end if
      end if
   end do
 end do

  if (nTall == 0 .and. nShort == 0) goto 99

#ifdef NORMALIZE_DATA
  nLand = 0
  nShort = 0
  nTall = 0

  do lat = 1, nLat
    do lon = 1, nLon
      if (mask(lon,lat) > 0) then
         if (pctspec < 100._r8 * (1._r8 - eps_fact*epsilon(1._r8))) then
            total = sum(pct_pft(lon,lat,:))
            if (total > 0.0d0) then
               total = total * 100._r8/(100._r8-pctspec)
               nLand = nLand + 1
               if (total < 99.99999d0) then
                  nShort = nShort + 1
               end if
               if (total > 100.00001d0) then
                  nTall = nTall + 1
               end if
            end if
         end if
      end if
    end do
  end do

  call check( nf90_put_var(ncid, varid, pct_pft) )
#endif

  call check( nf90_close(ncid) )

!  open(13, form="FORMATTED", err=99)
!  write(13,*), nLand, nShort, nTall
!  close(13)

99  continue

CONTAINS

  subroutine check(status)
    integer, intent ( in) :: status
    
    if(status /= nf90_noerr) then 
      print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if
  end subroutine check  

end program

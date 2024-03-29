!############################# Change Log ##################################
! 5.0.0
!
!###########################################################################
!  Copyright (C)  1990, 1995, 1999, 2000, 2003 - All Rights Reserved
!  Regional Atmospheric Modeling System - RAMS
!###########################################################################

subroutine eng_params

  ! eng_params:
  !  Set some constants that were formerly defined in the namelist.
  !  This set should be changed only for special tests or with code modification.

  use mem_grid, only: &
       sspct,         &
       impl,          &
       iadvl,         &
       iadvf

  use io_params, only: &
       ntopsmth,      &
       izflat
  
  implicit none
  
  sspct    = 0.  ! Sound speed fraction
  impl     = 1   ! Implicit flag for acoustic model  -  0=off, 1=on
  ntopsmth = 0   ! Number of passes for topography smoother
  izflat   = 0   ! Width of flat margin around domain (in grid points)
  iadvl    = 2   ! Order of advection - Leapfrog - 2 or 4 - only 2 now
  iadvf    = 2   ! Order of advection - Forward - 2 or 6 - only 2 now

end subroutine eng_params

!***************************************************************************

subroutine toptinit_user(n2,n3,ifm,topt,topzo)

implicit none

integer :: n2,n3,ifm,nx,ny
real, dimension(n2,n3) :: topt,topzo

integer :: i,j

!  This subroutine is the intended location for a user to customize TOPT,
!  the surface topography array.  It is called after all other types of
!  initialization of this field, so this subroutine has the last word.
!  By default the subroutine makes no change to the field.  The commented
!  lines below serve as a template for user-designed changes; the example
!  shown is the Witch of Agnesi mountain, a common test case.   Note that
!  this routine is called for each grid separately, so attention to the
!  current value of ngrid in this routine may be required by the user.


       return !<<<<<<<<<<<<<<<

!      if (itoptflg(ifm) .lt. 2) return
!
      if(ifm.eq.1)then
      print*,'tttttttttttttttttttttttttttttttttttttttt'
      print*,'Opening topography file: data/topo_AMSUL_wvln11-15-02_dz150m_g1.dat'
      print*,'tttttttttttttttttttttttttttttttttttttttt'
      print*,' '

       open(10,file='data/topo_AMSUL_wvln11-15-02_dz150m_g1.dat',&
            status='old',form='formatted')

        read(10,1998) nx,ny
        if(nx.ne.n2.or.ny.ne.n3) then
	print*,'Grades nao correspondentes nx n2 ny n3:',nx,n2,ny,n3
	print*,'STOP at ruser_traceA'
	stop
	endif
!		
        do j=ny,1,-1
            do i=1,nx
             read(10,2000) topt(i,j)
            enddo
        enddo

      endif

      if(ifm.eq.2)then
      print*,'tttttttttttttttttttttttttttttttttttttttt'
      print*,'Opening topography file: data/topo_AMSUL_wvln11-15-02_dz150m_q2003_g2.dat'
      print*,'tttttttttttttttttttttttttttttttttttttttt'
      print*,' '

       open(10,file='data/topo_AMSUL_wvln11-15-02_dz150m_g2.dat',&
            status='old',form='formatted')

        read(10,1998) nx,ny
        if(nx.ne.n2.or.ny.ne.n3) then
	print*,'Grades nao correspondentes nx n2 ny n3:',nx,n2,ny,n3
	print*,'STOP at ruser_traceA'
	stop
	endif
!		
        do j=ny,1,-1
            do i=1,nx
             read(10,2000) topt(i,j)
            enddo
        enddo

      endif

 1998   format(1x,2i6)
 2000   format(1x,f12.3)	


!
!      if(ifm.eq.1)then
!         hfwid=10000.
!         hgt=10.
!         hfwid2=hfwid**2
!         do j=1,n3
!            do i=1,n2
!               topt(i,j)=hgt*hfwid2/(hfwid2+xtn(i,1)**2)
!               topt(i,j) = 0. + float(i)
!               topzo(i,j) = 0.001 + topt(i,j)
!            enddo
!         enddo
!c         topt(5,1) = 10.
!      elseif(ifm.eq.2)then
!      endif

!if(ifm == 3) then
!   call toposmooth(n2,n3,topt,6,100,nnxp(ifm)-1,2,100)
!endif

return
end

subroutine toposmooth(n2,n3,topt,ntopsmth,i1,i2,j1,j2)

implicit none
integer :: n2,n3,ntopsmth,i1,i2,j1,j2

real, dimension(n2,n3) :: topt
real, allocatable :: vt2da(:,:)

integer :: i,j,iter

allocate (vt2da(n2,n3))
!          smooth the topography if desired.

do iter=1,ntopsmth
   do j=j1,j2
      do i=i1,i2
         vt2da(i,j)=abs(topt(i+1,j)-topt(i,j))
      enddo
   enddo
   print*,'pass:',iter, 'max dtopox:',maxval(vt2da(i1:i2,j1:j2))
   do j=j1,j2
      do i=i1,i2
         vt2da(i,j)=abs(topt(i,j+1)-topt(i,j))
      enddo
   enddo
   print*,'pass:',iter, 'max dtopoy:',maxval(vt2da(i1:i2,j1:j2))
   do j=j1,j2
      do i=i1,i2
         vt2da(i,j)=( (topt(i+1,j)+topt(i-1,j)+topt(i,j+1)+topt(i,j-1))&
                    + 4.*topt(i,j)) / 8.
      enddo
   enddo
   do j=j1,j2
      do i=i1,i2
         topt(i,j)=vt2da(i,j)
      enddo
   enddo
enddo

deallocate (vt2da)

return
end


! ****************************************************************************

subroutine sstinit_user(n2,n3,ifm,seatf)
implicit none
integer :: n2,n3,ifm,i,j
real, dimension(n2,n3) :: seatf

!  This subroutine is the intended location for a user to customize the
!  SEATP and SEATF arrays.  It is called after all other types of
!  initialization of these fields, so this subroutine has the last word.
!  By default the subroutine makes no change to the fields.  The commented
!  lines below serve as a template for user-designed changes.   Note that
!  this routine is called for each grid separately, so attention to the
!  current value of ngrid in this routine may be required by the user.

! if (ifm .eq. 1) then
!    do j = 1,n3
!       do i = 1,n2
!          seatf(i,j) =
!       enddo
!    enddo

! elseif (ifm .eq. 2) then
! endif

return
end

! ****************************************************************************

subroutine ndviinit_user(n2,n3,npat,ifm,veg_ndvif)
implicit none
integer :: n2,n3,npat,ifm,i,j
real, dimension(n2,n3,npat) :: veg_ndvif

!  This subroutine is the intended location for a user to customize the
!  NDVIP and NDVIF arrays.  It is called after all other types of
!  initialization of these fields, so this subroutine has the last word.
!  By default the subroutine makes no change to the fields.  The commented
!  lines below serve as a template for user-designed changes.   Note that
!  this routine is called for each grid separately, so attention to the
!  current value of ngrid in this routine may be required by the user.

! if (ifm .eq. 1) then

!    do j = 1,n3
!       do i = 1,n2
!          veg_ndvif(i,j,1) =
!          veg_ndvif(i,j,2) =

!          do ipat = 3,npat
!             veg(ndvif(i,j,ipat) =
!          enddo

!       enddo
!    enddo

! elseif (ifm .eq. 2) then
! endif

return
end

!*****************************************************************************

subroutine sfcinit_file_user(n2,n3,mzg,npat,ifm  &
   ,patch_area,leaf_class,soil_color,soil_text)

use rconstants

use catt_start, only: CATT !INTENT(IN)

implicit none

integer :: n2,n3,mzg,npat,ifm,i,j,k,ipat

real, dimension(mzg,n2,n3,npat) :: soil_text
real, dimension(n2,n3,npat) :: patch_area,leaf_class,soil_color

!  This subroutine is the intended location for a user to customize the
!  PATCH_AREA, leaf_class, and SOIL_TEXT arrays.  It is called after all
!  other types of initialization of these fields, so this subroutine has
!  the last word.  By default the subroutine makes no change to the
!  fields.  The commented lines below serve as a template for user-designed
!  changes.   Note that this routine is called for each grid separately, so
!  attention to the current value of ngrid in this routine may be required
!  by the user.

if (CATT==1) then

   !srf-06-08-2003: altera vegetacao no norte da Africa
   !srf-22-08-2003: altera vegetacao em todo dominio
   do j = 1,n3
      do i = 1,n2
         do ipat = 2,npat
            !print*,'veg=',leaf_class(i,j,ipat)
            if (nint(leaf_class(i,j,ipat)) == 10 ) then  !   semi-deserto
               
               !      if(nveg == 10 ) then                              !   semi-deserto
               ! em 22 fe agosto => rlong divergiu em regioes de semi deserto sobre a Am. do Sul
               !
               !       if(glat(i,j) .ge. 5. .and. glat(i,j) .lt. 35.) then !   5N < lat < 35N
               !        if(glon(i,j) .ge. -15. .and. glon(i,j) .le. 50.) then !  15W < lon < 50E
               
               !          schar(i,j,3,ipatch) = 13.   ! semi-deserto = cerrado
               !print*,'veg=',i,j,leaf_class(i,j,ipat)
               leaf_class(i,j,ipat)= 13.   ! semi-deserto = cerrado
               !	endif
               !       endif
            endif

         enddo
      enddo
   enddo

endif

! if (ifm .eq. 1) then

! do j = 1,n3
!    do i = 1,n2

!       patch_area(i,j,1) =         ! patch 1
!       leaf_class(i,j,1) =         ! patch 1

!       patch_area(i,j,2) =         ! patch 2
!       leaf_class(i,j,2) =         ! patch 2

!       do k = 1,nzg
!          soil_text(k,i,j,1) =     ! patch 1
!          soil_text(k,i,j,2) =     ! patch 2
!       enddo

!    enddo
! enddo

! do ipat = 3,npat
!    do j = 1,n3
!       do i = 1,n2

!          patch_area(i,j,ipat) =
!          leaf_class(i,j,ipat) =

!          do k = 1,nzg
!             soil_text(k,i,j,ipat) =
!          enddo

!       enddo
!    enddo
! enddo

! elseif (ifm .eq. 2) then
! endif

return
end
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
!     This subroutine is the intended location for a user to customize the primary LEAF3   !
! arrays for which standard RAMS data files do not exist.  It is called after all other    !
! types of initialization of these fields, so this subroutine has the last word.  By       !
! default the subroutine makes no change to the fields.  The commented lines below serve   !
! as a template for user-designed changes.   Note that this routine is called for each     !
! grid separately, so attention to the current value of ngrid in this routine may be       !
! required by the user.                                                                    !
!------------------------------------------------------------------------------------------!
subroutine sfcinit_nofile_user(n1,n2,n3,mzg,mzs,npat,ifm,theta,pi0,pp,rv,co2p,soil_water   &
                              ,soil_energy,psibar_10d,soil_color,soil_text,sfcwater_mass   &
                              ,sfcwater_energy,sfcwater_depth,ustar,tstar,rstar,cstar,zeta &
                              ,ribulk,veg_fracarea,veg_agb,veg_lai,veg_tai,veg_rough       &
                              ,veg_height,veg_displace,veg_albedo,patch_area,patch_rough   &
                              ,patch_wetind,leaf_class,soil_rough,sfcwater_nlev            &
                              ,stom_condct,ground_rsat,ground_rvap,ground_temp,ground_fliq &
                              ,veg_water,veg_hcap,veg_energy,can_prss,can_theiv,can_vpdef  &
                              ,can_theta,can_rvap,can_co2,hflxac_out,wflxac_out            &
                              ,qwflxac_out,eflxac_out,cflxac_out,hflxgc_out,wflxgc_out     &
                              ,qwflxgc_out,hflxvc_out,wflxvc_out,qwflxvc_out,transp_out    &
                              ,qtransp_out,intercepted_out,qintercepted_out,wshed_out      &
                              ,qwshed_out,throughfall_out,qthroughfall_out,runoff_out      &
                              ,qrunoff_out,drainage_out,qdrainage_out,gpp_out,plresp_out   &
                              ,resphet_out,growresp,veg_ndvip,veg_ndvic,veg_ndvif          &
                              ,snow_mass,snow_depth,rshort_gnd,rlong_gnd,cosz,rlongup      &
                              ,albedt,rvv,prsv,piv,vt2da,vt2db,glat,glon,zot,flpw,rtgt)

   use mem_grid
   use mem_leaf
   use leaf_coms
   use io_params
   use rconstants
   use therm_lib , only : reducedpress & ! function
                        , thetaeiv     & ! function
                        , tl2uint      & ! function
                        , cmtl2uext    & ! function
                        , exner2press  ! ! function

   implicit none
   !----- Arguments. ----------------------------------------------------------------------!
   integer                        , intent(in)    :: n1
   integer                        , intent(in)    :: n2
   integer                        , intent(in)    :: n3
   integer                        , intent(in)    :: mzg
   integer                        , intent(in)    :: mzs
   integer                        , intent(in)    :: npat
   integer                        , intent(in)    :: ifm
   real, dimension(n1,n2,n3)      , intent(in)    :: theta
   real, dimension(n1,n2,n3)      , intent(in)    :: pi0
   real, dimension(n1,n2,n3)      , intent(in)    :: pp
   real, dimension(n1,n2,n3)      , intent(in)    :: rv
   real, dimension(n1,n2,n3)      , intent(in)    :: co2p
   real, dimension(   n2,n3)      , intent(in)    :: snow_mass
   real, dimension(   n2,n3)      , intent(in)    :: snow_depth
   real, dimension(   n2,n3)      , intent(in)    :: flpw
   real, dimension(   n2,n3)      , intent(in)    :: rtgt
   real, dimension(   n2,n3)      , intent(in)    :: cosz
   real, dimension(mzg,n2,n3,npat), intent(inout) :: soil_water
   real, dimension(mzg,n2,n3,npat), intent(inout) :: soil_energy
   real, dimension(    n2,n3,npat), intent(inout) :: soil_color
   real, dimension(    n2,n3,npat), intent(inout) :: psibar_10d
   real, dimension(mzg,n2,n3,npat), intent(inout) :: soil_text
   real, dimension(mzs,n2,n3,npat), intent(inout) :: sfcwater_mass
   real, dimension(mzs,n2,n3,npat), intent(inout) :: sfcwater_energy
   real, dimension(mzs,n2,n3,npat), intent(inout) :: sfcwater_depth
   real, dimension(    n2,n3,npat), intent(inout) :: ustar
   real, dimension(    n2,n3,npat), intent(inout) :: tstar
   real, dimension(    n2,n3,npat), intent(inout) :: rstar
   real, dimension(    n2,n3,npat), intent(inout) :: cstar
   real, dimension(    n2,n3,npat), intent(inout) :: zeta
   real, dimension(    n2,n3,npat), intent(inout) :: ribulk
   real, dimension(    n2,n3,npat), intent(inout) :: veg_fracarea
   real, dimension(    n2,n3,npat), intent(inout) :: veg_agb
   real, dimension(    n2,n3,npat), intent(inout) :: veg_lai
   real, dimension(    n2,n3,npat), intent(inout) :: veg_tai
   real, dimension(    n2,n3,npat), intent(inout) :: veg_rough
   real, dimension(    n2,n3,npat), intent(inout) :: veg_height
   real, dimension(    n2,n3,npat), intent(inout) :: veg_displace
   real, dimension(    n2,n3,npat), intent(inout) :: veg_albedo
   real, dimension(    n2,n3,npat), intent(inout) :: patch_area
   real, dimension(    n2,n3,npat), intent(inout) :: patch_rough
   real, dimension(    n2,n3,npat), intent(inout) :: patch_wetind
   real, dimension(    n2,n3,npat), intent(inout) :: leaf_class
   real, dimension(    n2,n3,npat), intent(inout) :: soil_rough
   real, dimension(    n2,n3,npat), intent(inout) :: sfcwater_nlev
   real, dimension(    n2,n3,npat), intent(inout) :: stom_condct
   real, dimension(    n2,n3,npat), intent(inout) :: ground_rsat
   real, dimension(    n2,n3,npat), intent(inout) :: ground_rvap
   real, dimension(    n2,n3,npat), intent(inout) :: ground_temp
   real, dimension(    n2,n3,npat), intent(inout) :: ground_fliq
   real, dimension(    n2,n3,npat), intent(inout) :: veg_water
   real, dimension(    n2,n3,npat), intent(inout) :: veg_energy
   real, dimension(    n2,n3,npat), intent(inout) :: veg_hcap
   real, dimension(    n2,n3,npat), intent(inout) :: can_prss
   real, dimension(    n2,n3,npat), intent(inout) :: can_theiv
   real, dimension(    n2,n3,npat), intent(inout) :: can_vpdef
   real, dimension(    n2,n3,npat), intent(inout) :: can_theta
   real, dimension(    n2,n3,npat), intent(inout) :: can_rvap
   real, dimension(    n2,n3,npat), intent(inout) :: can_co2
   real, dimension(    n2,n3,npat), intent(inout) :: hflxac_out
   real, dimension(    n2,n3,npat), intent(inout) :: wflxac_out
   real, dimension(    n2,n3,npat), intent(inout) :: qwflxac_out
   real, dimension(    n2,n3,npat), intent(inout) :: eflxac_out
   real, dimension(    n2,n3,npat), intent(inout) :: cflxac_out
   real, dimension(    n2,n3,npat), intent(inout) :: hflxgc_out
   real, dimension(    n2,n3,npat), intent(inout) :: wflxgc_out
   real, dimension(    n2,n3,npat), intent(inout) :: qwflxgc_out
   real, dimension(    n2,n3,npat), intent(inout) :: hflxvc_out
   real, dimension(    n2,n3,npat), intent(inout) :: wflxvc_out
   real, dimension(    n2,n3,npat), intent(inout) :: qwflxvc_out
   real, dimension(    n2,n3,npat), intent(inout) :: transp_out
   real, dimension(    n2,n3,npat), intent(inout) :: qtransp_out
   real, dimension(    n2,n3,npat), intent(inout) :: intercepted_out
   real, dimension(    n2,n3,npat), intent(inout) :: qintercepted_out
   real, dimension(    n2,n3,npat), intent(inout) :: wshed_out
   real, dimension(    n2,n3,npat), intent(inout) :: qwshed_out
   real, dimension(    n2,n3,npat), intent(inout) :: throughfall_out
   real, dimension(    n2,n3,npat), intent(inout) :: qthroughfall_out
   real, dimension(    n2,n3,npat), intent(inout) :: runoff_out
   real, dimension(    n2,n3,npat), intent(inout) :: qrunoff_out
   real, dimension(    n2,n3,npat), intent(inout) :: drainage_out
   real, dimension(    n2,n3,npat), intent(inout) :: qdrainage_out
   real, dimension(    n2,n3,npat), intent(inout) :: gpp_out
   real, dimension(    n2,n3,npat), intent(inout) :: plresp_out
   real, dimension(    n2,n3,npat), intent(inout) :: resphet_out
   real, dimension(    n2,n3,npat), intent(inout) :: growresp
   real, dimension(    n2,n3,npat), intent(inout) :: veg_ndvip
   real, dimension(    n2,n3,npat), intent(inout) :: veg_ndvic
   real, dimension(    n2,n3,npat), intent(inout) :: veg_ndvif
   real, dimension(    n2,n3,npat), intent(inout) :: rshort_gnd
   real, dimension(    n2,n3,npat), intent(inout) :: rlong_gnd
   real, dimension(   n2,n3)      , intent(inout) :: rlongup
   real, dimension(   n2,n3)      , intent(inout) :: albedt
   real, dimension(   n2,n3)      , intent(inout) :: rvv
   real, dimension(   n2,n3)      , intent(inout) :: prsv
   real, dimension(   n2,n3)      , intent(inout) :: piv
   real, dimension(   n2,n3)      , intent(inout) :: vt2da
   real, dimension(   n2,n3)      , intent(inout) :: vt2db
   real, dimension(   n2,n3)      , intent(in)    :: glat
   real, dimension(   n2,n3)      , intent(in)    :: glon
   real, dimension(   n2,n3)      , intent(inout) :: zot
   !----- Local variables. ----------------------------------------------------------------!
   integer                                        :: k2
   integer                                        :: i
   integer                                        :: j
   integer                                        :: k
   integer                                        :: ipat
   integer                                        :: nveg
   integer                                        :: nsoil
   real                                           :: soil_temp
   real                                           :: soil_fliq
   !---------------------------------------------------------------------------------------!

   ! select case (ifm)
   ! case (1)

   ! !----- Set up some scratch variables. ------------------------------------------------!
   ! g_urban    = 0.
   ! emis_town  = 0.
   ! alb_town   = 0.
   ! ts_town    = 0.
   ! !-------------------------------------------------------------------------------------!

   !   jloop: do j = 1,n3
   !      iloop: do i = 1,n2
   !         k2=nint(flpw(i,j))
   !         piv(i,j)  = 0.5 * (pi0(k2-1,i,j) + pi0(k2,i,j) + pp(k2-1,i,j) + pp(k2,i,j))
   !         prsv(i,j) = exner2press(piv(i,j))
   !         geoht     = (zt(k2)-zm(k2-1)) * rtgt(i,j)

   !         atm_shv   = rv(k2,i,j) / (rv(k2,i,j) + 1.)

   !         patch_rough(i,j,1) = waterrough

   !         !-----------------------------------------------------------------------------!
   !         !     Canopy properties.  Copy conserved variables from lowest atmospheric    !
   !         ! grid, and compute pressure and temperature.                                 !
   !         !-----------------------------------------------------------------------------!
   !         can_prss(i,j,1) = reducedpress(prsv(i,j),theta(i,j),atm_shv,geoht             &
   !                                       ,theta(i,j),atm_shv,can_depth)
   !         can_theta(i,j,1)   = theta(k2,i,j)
   !         can_rvap(i,j,1)    = rv(k2,i,j)
   !         can_co2(i,j,1)     = co2p(k2,i,j)
   !         can_exner          = press2exner(can_prss(i,j,1))
   !         can_temp           = extheta2temp(can_exner,theta(k2,i,j))
   !         can_theiv(i,j,1)   = thetaeiv(can_theta(i,j,1),can_prss(i,j,1),can_temp       &
   !                                      ,can_rvap(i,j,1),can_rvap(i,j,1))
   !         can_vpdef(i,j,1)   = vpdefil (can_prss(i,j,1),can_temp,can_rvap(i,j,1)        &
   !                                      ,.false.)

   !         !----- Water patch, so we set vegetation properties to zero. -----------------!
   !         veg_energy(i,j,1)  = 0.0
   !         veg_water (i,j,1)  = 0.0
   !         veg_hcap  (i,j,1)  = 0.0

   !         !-----------------------------------------------------------------------------!
   !         !     Soil properties. Except for top layer energy, everything is set to      !
   !         ! zero.                                                                       !
   !         !-----------------------------------------------------------------------------!
   !         soil_energy(:,i,j,1) = 0.
   !         soil_water (:,i,j,1) = 1.
   !         soil_energy(mzg,i,j,1) = tl2uint( seatp(i,j)                                  &
   !                                         + (seatf(i,j) - seatp(i,j))* timefac_sst, 1.0)
   !         !-----------------------------------------------------------------------------!

   !         !----- Fluxes.  Initially they should be all zero. ---------------------------!
   !         hflxac      (i,j,1) = 0.0
   !         wflxac      (i,j,1) = 0.0
   !         qwflxac     (i,j,1) = 0.0
   !         eflxac      (i,j,1) = 0.0
   !         cflxac      (i,j,1) = 0.0
   !         hflxgc      (i,j,1) = 0.0
   !         wflxgc      (i,j,1) = 0.0
   !         qwflxgc     (i,j,1) = 0.0
   !         hflxvc      (i,j,1) = 0.0
   !         wflxvc      (i,j,1) = 0.0
   !         qwflxvc     (i,j,1) = 0.0
   !         transp      (i,j,1) = 0.0
   !         qtransp     (i,j,1) = 0.0
   !         intercepted (i,j,1) = 0.0
   !         qintercepted(i,j,1) = 0.0
   !         wshed       (i,j,1) = 0.0
   !         qwshed      (i,j,1) = 0.0
   !         throughfall (i,j,1) = 0.0
   !         qthroughfall(i,j,1) = 0.0
   !         runoff      (i,j,1) = 0.0
   !         qrunoff     (i,j,1) = 0.0
   !         drainage    (i,j,1) = 0.0
   !         qdrainage   (i,j,1) = 0.0
   !         gpp         (i,j,1) = 0.0
   !         plresp      (i,j,1) = 0.0
   !         resphet     (i,j,1) = 0.0
   !         growresp    (i,j,1) = 0.0

   !         !----- Above-ground biomass.  This should be always 0 for water patches. -----!
   !         veg_agb  (i,j,1) = 

   !         !-----------------------------------------------------------------------------!
   !         !     We now loop over the land patches.                                      !
   !         !-----------------------------------------------------------------------------!
   !         patchloop: do ipat = 2,npat

   !            nveg = nint(leaf_class(i,j,ipat))

   !            soil_rough(i,j,ipat)    = 
   !            patch_rough(i,j,ipat)   = 
   !            veg_rough(i,j,ipat)     = 

   !            veg_height(i,j,ipat)    = 
   !            veg_displace(i,j,ipat)  = 
   !            veg_albedo(i,j,ipat)    =
   !            stom_condct(i,j,ipat)   =

   !            veg_hcap  (i,j,ipat)    = 
   !            veg_water (i,j,ipat)    = 
   !            veg_energy(i,j,ipat)    = 

   !            !----- Above-ground biomass.  This is non-0 only when we run ED-2. --------!
   !            veg_agb   (i,j,ipat) =

   !            can_prss (i,j,ipat) = 
   !            can_theiv(i,j,ipat) = 
   !            can_vpdef(i,j,ipat) = 
   !            can_theta(i,j,ipat) = 
   !            can_rvap (i,j,ipat) = 
   !            can_co2  (i,j,ipat) = 

   !            !----- Fluxes. ------------------------------------------------------------!
   !            sensible_gc (i,j,ipat) = 
   !            sensible_vc (i,j,ipat) = 
   !            evap_gc     (i,j,ipat) = 
   !            evap_vc     (i,j,ipat) = 
   !            transp      (i,j,ipat) = 
   !            gpp         (i,j,ipat) = 
   !            plresp      (i,j,ipat) = 
   !            resphet     (i,j,ipat) = 
   !            growresp    (i,j,ipat) = 

   !            do k = 1,mzg

   !               nsoil = nint(soil_text(k,i,j,ipat))

   !               !-----------------------------------------------------------------------!
   !               !     For persistent wetlands (bogs, marshes, fens, swamps) and         !
   !               ! irrigated crops, initialize with saturated soil.  Currently, this     !
   !               ! corresponds to leaf classes 16, 17, and 20.  Otherwise, use the       !
   !               ! user-defined input profile.                                           !
   !               !-----------------------------------------------------------------------!
   !               select case (nint(leaf_class(i,j,ipat)))
   !               case (16,17,20)
   !                  soil_water(k,i,j,ipat) = 
   !               case default
   !                  soil_water(k,i,j,ipat) = 
   !               end select

   !               !-----------------------------------------------------------------------!
   !               !     By default, initialize soil internal energy at a temperature      !
   !               ! equal to can_temp + stgoff(k).  If the temperature is initially below !
   !               ! triple point, we assume all soil water to be frozen, otherwise we     !
   !               ! assume all water to be liquid.  At the triple point, we assume that   !
   !               ! the liquid fraction is 50%.                                           !
   !               !-----------------------------------------------------------------------!
   !               soil_temp = can_temp + stgoff(k)
   !               if (soil_temp == t3ple) then
   !                  soil_fliq = 0.5
   !               elseif (soil_temp > t3ple) then
   !                  soil_fliq = 1.0
   !               else
   !                  soil_fliq = 0.0
   !               end if
   !               soil_energy(k,i,j,ipat) = cmtl2uext( slcpd(nsoil)                       &
   !                                                  , soil_water(k,i,j,ipat)             &
   !                                                  , soil_temp,soil_fliq)
   !            end do

   !            !------ Surface water, if any, will initially occupy just the first level. ----!
   !            do 1 = 1,mzs
   !               sfcwater_mass(k,i,j,ipat) = 
   !               sfcwater_energy(k,i,j,ipat) = 
   !               sfcwater_depth(k,i,j,ipat) = 
   !            end do

   !            !--------------------------------------------------------------------------!
   !            !    For persistent wetlands (bogs, marshes, fens, swamps), initialise     !
   !            ! with 10 cm water depth and temperature not colder than the triple point. !
   !            ! Currently, this corresponds to leaf classes 17 and  20.  Glaciers (leaf  !
   !            ! class 2) will be initialised with a thick layer of compact ice           !
   !            ! (currently 6 m), not warmer than the triple point, so we make sure there !
   !            ! will enough ice there.  The ice will be eventually split into several    !
   !            ! layers.                                                                  !
   !            !--------------------------------------------------------------------------!
   !            select case (nint(leaf_class(i,j,ipat))
   !            case (2)
   !               sfcwater_depth (1,i,j,ipat) = 
   !               sfcwater_mass  (1,i,j,ipat) = 
   !               sfcwater_energy(1,i,j,ipat) = tl2uint(min(t3ple,can_temp),0.0)
   !            case (17,20)
   !               sfcwater_depth (1,i,j,ipat) = 
   !               sfcwater_mass  (1,i,j,ipat) = 
   !               sfcwater_energy(1,i,j,ipat) = tl2uint(max(t3ple,can_temp),1.0)
   !            end select

   !            !--------------------------------------------------------------------------!
   !            !    If there is initial snow information, add the information into the    !
   !            ! first layer.                                                             !
   !            !--------------------------------------------------------------------------!
   !            if (snow_mass(i,j) > 0.) then
   !               sfcwater_energy(1,i,j,ipat) = ( sfcwater_energy(1,i,j,ipat)             &
   !                                             * sfcwater_mass  (1,i,j,ipat)             &
   !                                             + snow_mass(i,j)                          &
   !                                             * tl2uint(min(t3ple,can_temp),0.0) )      &
   !                                           / (sfcwater_mass(1,i,j,ipat)+snow_mass(i,j))
   !               sfcwater_mass  (1,i,j,ipat) = sfcwater_mass (1,i,j,ipat)+snow_mass(i,j)
   !               !-----------------------------------------------------------------------!
   !               !    Add a depth of 5x the equivalent of liquid depth so the extra snow !
   !               ! is fresh and fluffy.                                                  !
   !               !-----------------------------------------------------------------------!
   !               sfcwater_depth(1,i,j,ipat)  = sfcwater_depth(1,i,j,ipat)                &
   !                                           + snow_mass(i,j) * 5. * wdnsi
   !            end if

   !            !----- We must have either nothing or a single layer at this point. -------!
   !            if (sfcwater_mass(1,i,j,ipat) > 0.) then
   !               sfcwater_nlev(i,j,ipat) = 1.
   !            else
   !               sfcwater_nlev(i,j,ipat) = 0.
   !            end if


   !           !---------------------------------------------------------------------------!
   !           !     Compute some vegetation properties, such as LAI, heat capacity, and   !
   !           ! update energy whilst preserving the temperature and liquid fraction.  We  !
   !           ! assign a dummy heat capacity because the routine will transform the       !
   !           ! energy back to temperature.                                               !
   !           !----------------------------------------------------------- ---------------!
   !           veg_temp             = can_temp
   !           veg_hcap  (i,j,ipat) = hcapveg_ref * max(veg_height(i,j,ipat),hcapveg_hmin)
   !           veg_energy(i,j,ipat) = veg_hcap(i,j,ipat)
   !           veg_water (i,j,ipat) = 0.
   !           call veg_misc_update( ifm                                                   &
   !                               , patch_area      (i,j,ipat)                            &
   !                               , leaf_class      (i,j,ipat)                            &
   !                               , veg_fracarea    (i,j,ipat)                            &
   !                               , veg_lai         (i,j,ipat)                            &
   !                               , veg_tai         (i,j,ipat)                            &
   !                               , veg_rough       (i,j,ipat)                            &
   !                               , veg_height      (i,j,ipat)                            &
   !                               , veg_displace    (i,j,ipat)                            &
   !                               , veg_albedo      (i,j,ipat)                            &
   !                               , veg_ndvip       (i,j,ipat)                            &
   !                               , veg_ndvic       (i,j,ipat)                            &
   !                               , veg_ndvif       (i,j,ipat)                            &
   !                               , veg_agb         (i,j,ipat)                            &
   !                               , veg_energy      (i,j,ipat)                            &
   !                               , veg_water       (i,j,ipat)                            &
   !                               , veg_hcap        (i,j,ipat)                            &
   !                               , psibar_10d      (i,j,ipat)                            )
   !           !------------------------------------------------------------------------------!

   !            call leaf_grndvap(soil_energy(mzg,i,j,ipat),soil_water     (mzg,i,j,ipat)  &
   !                             ,soil_text  (mzg,i,j,ipat),sfcwater_energy(mzs,i,j,ipat)  &
   !                             ,sfcwater_nlev  (i,j,ipat),can_rvap       (i,j,ipat)      &
   !                             ,can_prss       (i,j,ipat),ground_rsat        (i,j,ipat)  &
   !                             ,ground_rvap    (i,j,ipat),ground_temp        (i,j,ipat)  &
   !                             ,ground_fliq    (i,j,ipat))

   !          call sfcrad( nzg, nzs, ipat                                                  &
   !                          , soil_water     (:,i,j,ipat) , soil_color     (  i,j,ipat)  &
   !                          , soil_text      (:,i,j,ipat) , sfcwater_depth (:,i,j,ipat)  &
   !                          , patch_area     (  i,j,ipat) , veg_fracarea   (  i,j,ipat)  &
   !                          , leaf_class     (  i,j,ipat) , veg_albedo     (  i,j,ipat)  &
   !                          , sfcwater_nlev  (  i,j,ipat) , rshort         (  i,j     )  &
   !                          , rlong          (  i,j     ) , albedt         (  i,j     )  &
   !                          , rlongup        (  i,j     ) , cosz           (  i,j     )  &
   !                          , g_urban                     , emis_town                    &
   !                          , alb_town                    , ts_town                      )
   !         end do patchloop
   !      end do iloop
   !   end do jloop
   ! case (2)
   !    ...
   ! case default
   !    ...
   ! end select

   return
end subroutine sfcinit_nofile_user
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
subroutine bubble(m1,m2,m3,thp,rtp)

implicit none
integer :: m1,m2,m3,i,j,k
real, dimension(m1,m2,m3) :: thp,rtp
!
do j = 1,1
   do i = 17,26
     do k = 2,7
!e            thp(k,i,j) = thp(k,i,j) + 5.
!e            rtp(k,i,j) = rtp(k,i,j) * 1.2
     enddo
   enddo
enddo
return
end

!     ******************************************************************

subroutine trtend(m1,m2,m3,k1,k2,i1,i2,j1,j2,rtt,tht,wt,thp,wp,dtlt)
implicit none
integer :: m1,m2,m3,k1,k2,i1,i2,j1,j2,i,j,k

real :: dtlt
real, dimension(m1,m2,m3) :: rtt,tht,wt,thp,wp

do j=j1,j2
  do i=i1,i2
    do k=k1,k2
      rtt(k,i,j)=rtt(k,i,j)+10./3600.
    enddo
  enddo
enddo
do j=j1,j2
  do i=i1,i2
    do k=k1,k2
      tht(k,i,j)=(310.-thp(k,i,j))/dtlt
    enddo
  enddo
enddo
do j=j1,j2
  do i=i1,i2
    do k=k1-1,k2
      wt(k,i,j)=(2.-wp(k,i,j))/dtlt
    enddo
  enddo
enddo
return
end

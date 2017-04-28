module TracingAux

! some subroutines from 'Tracing' module
! were moved here to resolve circular dependency
! between 'HeadWave' and 'Tracing'

contains

      subroutine block(x,layer1,iblk1)
!
!     determine block of point x in layer
!
      include 'rayinvr.par'
      include 'rayinvr.com'
!
      do 10 i=1,nblk(layer1)
         if(x.ge.xbnd(layer1,i,1) .and. x.le.xbnd(layer1,i,2)) then
           iblk1=i
           return
         end if
10    continue
!
      write(6,5) layer1,x
5     format(/'***  block undetermined  ***'/   &
        'layer=',i3,'  x=',f10.3/)
!
      stop
      end
!
!     ----------------------------------------------------------------
!
     function vel(x,z)
!
!     calculate p-wave velocity at point (x,z) in model
!
      include 'rayinvr.par'
      include 'rayinvr.com'
!
      vel=(c(layer,iblk,1)*x+c(layer,iblk,2)*x**2+c(layer,iblk,3)*z     &
            +c(layer,iblk,4)*x*z+c(layer,iblk,5))/(c(layer,iblk,6)*x    &
            +c(layer,iblk,7))

      return
      end


end module

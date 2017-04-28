
module Misc

use Tracing

contains

!
!     version 1.3  Aug 1992
!
!     Misc routines for RAYINVR
!
!     ----------------------------------------------------------------
!
      subroutine ttime(is,xshot,npt,nr,a1,ifam,itt,iszero,iflag,    &
                      uf,irayf)
!
!     calculate travel time along a single ray using the equation:
!
!                       t=2*h/(v1+v2)
!
!     for the travel time between two points a distance h apart
!
      include 'rayinvr.par'
      real vave(ppray)
      integer itt(1)
      include 'rayinvr.com'
!
      if(idump.eq.1) write(12,15) ifam,nr,npt,xr(npt),zr(npt),  &
        ar(npt,1)*pi18,ar(npt,2)*pi18,vr(npt,1),vr(npt,2),      &
        layer,iblk,id,iwave
15    format(i4,i3,i4,2f8.3,2f8.2,2f7.2,4i3)

      time=0.
      iflagw=0

      do 10 i=1,npt-1
         tr(i)=sqrt((xr(i+1)-xr(i))**2+(zr(i+1)-zr(i))**2)
         vave(i)=(vr(i,2)+vr(i+1,1))/2.
         if(iflagw.eq.0.and.vave(i).gt.1.53) iflagw=1
         if(iflagw.eq.1.and.vave(i).lt.1.53.and.nr.ne.0) then
!          write(67,25) nr,xr(i),zr(i),time,uf,irayf
25         format(i5,4f10.3,i10)
           iflagw=2
         end if
         time=time+tr(i)/vave(i)
10    continue

      a2=fid1*(90.-fid*ar(npt,1)*pi18)/fid
      nptr=npt
      if(vred.eq.0.) then
        timer=time
      else
        timer=time-abs(xr(npt)-xshot)/vred
      end if
      rayid(ntt)=float(idray(1))+float(idray(2))/10.
      if(nr.eq.0) go to 999
      write(11,5) is,nr,a1,a2,xr(npt),zr(npt),timer,nptr,rayid(ntt)
5     format(2i4,2f9.3,f9.3,f8.2,f8.3,i6,f6.1)
      if(vr(npt,2).ne.0.) then
        itt(ifam)=itt(ifam)+1
        if(iszero.eq.0) then
          range(ntt)=xr(npt)
        else
          range(ntt)=abs(xr(npt)-xshot)
        end if
        tt(ntt)=timer
        xshtar(ntt)=xshot
        fidarr(ntt)=fid1
        ntt=ntt+1
      end if
999   return
      end
!
!     ----------------------------------------------------------------
!
      subroutine sort(x,npts)
!
!     sort the elements of array x in order of increasing size using
!     a bubble sort technique
!
      real x(1)
      do 10 i=1,npts-1
         iflag=0
         do 20 j=1,npts-1
            if(x(j).gt.x(j+1)) then
              iflag=1
              xh=x(j)
              x(j)=x(j+1)
              x(j+1)=xh
            end if
20       continue
         if(iflag.eq.0) return
10     continue
      return
      end
!
!     ----------------------------------------------------------------
!
      subroutine smooth(x,n)
!
!     three point triangular smoothing filter
!
      real x(n)
      m=n-1
      a=0.77*x(1)+0.23*x(2)
      b=0.77*x(n)+0.23*x(m)
      xx=x(1)
      xr=x(2)
      do 10 i=2,m
         xl=xx
         xx=xr
         xr=x(i+1)
         x(i)=0.54*xx+0.23*(xl+xr)
 10   continue
      x(1)=a
      x(n)=b
      return
      end
!
!    ----------------------------------------------------------------
!
      subroutine smooth2(x,n,n1,n2)
!
!     three point triangular smoothing filter
!
      real x(n)
      m=n-1
      a=0.77*x(1)+0.23*x(2)
      b=0.77*x(n)+0.23*x(m)
      xx=x(1)
      xr=x(2)
      do 10 i=2,m
         xl=xx
         xx=xr
         xr=x(i+1)
         if(i.lt.n1.or.i.gt.n2) x(i)=0.54*xx+0.23*(xl+xr)
 10   continue
      if(n1.gt.1) x(1)=a
      if(n2.lt.n) x(n)=b
      return
      end
!
!     ----------------------------------------------------------------
!
      subroutine sort3(ra,rb,n)
!
!     sort the elements of array x in order of increasing size using
!     a heapsort technique
!
      real ra(n)
      integer rb(n)

      do 30 i=1,n
         rb(i)=i
30    continue

      l=n/2+1
      ir=n

10    continue

      if(l.gt.1) then
        l=l-1
        rra=ra(l)
        rrb=rb(l)
      else
        rra=ra(ir)
        rrb=rb(ir)
        ra(ir)=ra(1)
        rb(ir)=rb(1)
        ir=ir-1
        if(ir.eq.1) then
          ra(1)=rra
          rb(1)=rrb
          return
        end if
      end if
      i=l
      j=l+l
20    if(j.le.ir) then
        if(j.lt.ir) then
          if(ra(j).lt.ra(j+1)) j=j+1
        end if
        if(rra.lt.ra(j)) then
          ra(i)=ra(j)
          rb(i)=rb(j)
          i=j
          j=j+j
        else
          j=ir+1
        end if
        go to 20
      end if
      ra(i)=rra
      rb(i)=rrb
      go to 10

      end
!
!     ----------------------------------------------------------------
!
      subroutine modwr(modout,dx,dz,modi,ifrbnd,frz,xmmin,xmmax)
!
!     output the velocity model on a uniform grid for input to the
!     plotting program MODPLT
!
      include 'rayinvr.par'
      include 'rayinvr.com'

      real vzgrid(pxgrid),xgrid(player+1),xgmt(pxgrid),zgmt(pxgrid)
      integer igrid(player+1),modi(player),zsmax(pxgrid)

      write(31,5) xmmin,xmmax,zmin,zmax,zmin,dx,dz
5     format(7f10.3)

      nx=nint((xmmax-xmmin)/dx)
      nz=nint((zmax-zmin)/dz)

      write(31,15) nx,nz
15    format(10i7)

      if(abs(modout).eq.3) then
        do 310 j=1,nx
           do i=nz,1,-1
              if(sample(i,j).gt.0) then
                zsmax(j)=i
                go to 310
              end if
           enddo
           zsmax(j)=0
310     continue
      end if

      do 10 i=1,nz+1
         zmod=zmin+float(i-1)*dz
         do 20 j=1,nx+1
            xmod=xmmin+float(j-1)*dx

            call xzpt(xmod,zmod,layer,iblk,iflag)

            if(iflag.eq.0) then
              vzgrid(j)=vel(xmod,zmod)
            else
              vzgrid(j)=9.999
            end if

            xgmt(j)=xmod
            zgmt(j)=zmod
            if(modout.le.-2) zgmt(j)=-zgmt(j)

20       continue

         if(abs(modout).eq.2) then
           do 110 j=1,nx+1
              iflag=0
              if(i.gt.1.and.j.gt.1) then
                if(sample(i-1,j-1).gt.0) iflag=1
              end if
              if(i.gt.1.and.j.le.nx) then
                if(sample(i-1,j).gt.0) iflag=1
              end if
              if(j.gt.1.and.i.le.nz) then
                if(sample(i,j-1).gt.0) iflag=1
              end if
              if(i.le.nz.and.j.le.nx) then
                if(sample(i,j).gt.0) iflag=1
              end if
              if(iflag.eq.0) vzgrid(j)=9.999
              if(iflag.eq.1) then
                jl=j
                go to 111
              end if
110        continue
111        if (jl.lt.nx+1) then
             do 120 j=nx+1,jl,-1
                iflag=0
                if(i.gt.1.and.j.gt.1) then
                  if(sample(i-1,j-1).gt.0) iflag=1
                end if
                if(i.gt.1.and.j.le.nx) then
                  if(sample(i-1,j).gt.0) iflag=1
                end if
                if(j.gt.1.and.i.le.nz) then
                  if(sample(i,j-1).gt.0) iflag=1
                end if
                if(i.le.nz.and.j.le.nx) then
                  if(sample(i,j).gt.0) iflag=1
                end if
                if(iflag.eq.0) vzgrid(j)=9.999
                if(iflag.eq.1) go to 112
120          continue
           end if
         end if

         if(abs(modout).eq.3) then
           do 210 j=1,nx+1
              iflag=0
              if(j.gt.1) then
                if(zsmax(j-1).ge.i) iflag=1
              end if
              if(j.le.nx) then
                if(zsmax(j).ge.i) iflag=1
              end if
              if(iflag.eq.0) vzgrid(j)=9.999
210        continue
         end if

112      write(31,25) (vzgrid(j),j=1,nx+1)
25       format(10f10.3)

         do 130 j=1,nx+1
            if(vzgrid(j).ne.9.999)  &
             write(35,35) xgmt(j),zgmt(j),vzgrid(j)
35          format(3f10.3)
            write(63,26) xgmt(j),-zgmt(j),sample(i,j)
26          format(2f10.3,i10)
130      continue

10    continue

      do 30 i=nlayer,1,-1
         if(modi(i).gt.0) then
           nmodi=i
           go to 40
         end if
30    continue
      nmodi=0

40    write(31,15) nmodi

      if(nmodi.gt.0) then
        do 50 i=1,nmodi
           igrid(i)=nx+1
           xgrid(i)=xmmin
50      continue

        write(31,15) (igrid(i),i=1,nmodi)
        write(31,25) (xgrid(i),i=1,nmodi)

        do 60 ii=1,nmodi
           i=modi(ii)
           il=i
           ib=1
           iblk=1
           do 70 j=1,nx+1
              x=xmmin+float(j-1)*dx
              if(x.lt.xmmin) x=xmmin+.001
              if(x.gt.xmmax) x=xmmax-.001
80            if(x.ge.xbnd(il,iblk,1).and.x.le.xbnd(il,iblk,2)) then
                vzgrid(j)=s(il,iblk,ib)*x+b(il,iblk,ib)
                go to 70
              else
                iblk=iblk+1
                go to 80
              end if
70         continue
!
           write(31,25) (vzgrid(j),j=1,nx+1)
!
60      continue
      end if
!
      if(ifrbnd.eq.1) then
!       open(unit=32, file='f.out')
!
        if(frz.eq.0.) frz=(zmax-zmin)/1000.
!
        do 90 i=1,nfrefl
!          write(32,15) npfref(i)*2
!          write(32,25) (xfrefl(i,j),zfrefl(i,j),j=1,npfref(i)),
!    +                  (xfrefl(i,j),zfrefl(i,j)+frz,j=npfref(i),1,-1)
90      continue
      end if
!
      return
      end
!
!     ----------------------------------------------------------------
!
      subroutine fd(dxz,xmmin,xmmax,ifd)
!
!     output the velocity model on a uniform grid for input to the
!     finite difference program FD

      include 'rayinvr.par'
      include 'rayinvr.com'
!
      real vzgrid(pxgrid),xgmt(pxgrid),zgmt(pxgrid)
!
      nx=int((xmmax-xmmin)/dxz)
      nz=int((zmax-zmin)/dxz)
      xmmaxr=xmmin+float(nx)*dxz
      zmaxr=zmin+float(nz)*dxz
!
      write(0,*) xmmin,xmmaxr,zmin,zmaxr,dxz,nx+1,nz+1
      if(ifd.ne.2)  &
            write(35,5) xmmin,xmmaxr,zmin,zmaxr,dxz,nx+1,nz+1
5     format(5f10.3,2i10)

      do 10 i=1,nz+1
         zmod=zmin+float(i-1)*dxz
         do 20 j=1,nx+1
            xmod=xmmin+float(j-1)*dxz

            call xzpt(xmod,zmod,layer,iblk,iflag)

            if(iflag.eq.0) then
              vzgrid(j)=vel(xmod,zmod)
            else
              vzgrid(j)=9.999
            end if

            xgmt(j)=xmod
            zgmt(j)=-zmod
!
20       continue
!
         if(ifd.ne.2) then
112        write(35,25) (vzgrid(j),j=1,nx+1)
25         format(10f10.3)
         else
           do 30 j=1,nx+1
              write(35,15) xgmt(j),zgmt(j),vzgrid(j)
15            format(3f10.3)
30         continue
         end if

10    continue

      return
      end
!
!     ----------------------------------------------------------------
!
      subroutine cells(npt,xmmin,dx,dz)
!
!     identify grid cells that have been sampled by ray path
!
      include 'rayinvr.par'
      include 'rayinvr.com'
!
      do 10 i=1,npt-1
         nspts=nint((((xr(i+1)-xr(i))**2+(zr(i+1)-zr(i))**2)**.5)/  &
              min(dx,dz))+1
         if(nspts.le.1) nspts=2
         xinc=(xr(i+1)-xr(i))/float(nspts-1)
         zinc=(zr(i+1)-zr(i))/float(nspts-1)
         do 20 j=1,nspts
            xrp=xr(i)+float(j-1)*xinc
            zrp=zr(i)+float(j-1)*zinc
            nx=nint((xrp-xmmin)/dx)+1
            nz=nint((zrp-zmin)/dz)+1
            sample(nz,nx)=sample(nz,nx)+1
20       continue
10    continue

      return
      end


end module

module PlotLib
! subroutine stubs for plot calls


contains

!     File patched for g77 compilation
!     Scott Pearse (scott.pearse@durham.ac.uk)
!
!
!     version 1.3  Aug 1992
!
!     ----------------------------------------------------------------
!     |                                                              |
!     |           ***********  P L T L I B  ***********              |
!     |                                                              |
!     |         Plot library to convert Calcomp-like calls           |
!     |          to Uniras or other local graphics system            |
!     |                                                              |
!     |                   Written by C. A. Zelt                      |
!     |                                                              |
!     |                Geological Survey of Canada                   |
!     |                  Ottawa, Canada K1A 0Y3                      |
!     |                                                              |
!     ----------------------------------------------------------------
!
!     The purpose of each routine is described below the subroutine
!     statement.
!
!     The routine pcolor is required only if the local graphics allows
!     colour plotting. The routine segmnt creates a Uniras device-
!     independent plot metafile and can be ignored if Uniras is not
!     the local graphics system.
!
!     The following is a list and description of all Uniras plot calls:
!
!     groute - select graphics device
!     gshmes - set Uniras error-message display mode
!     gopen  - open uniras
!     grpsiz - inquire plot area size (for background colour only)
!     rrect  - plot rectangle (for background colour only)
!     gclear - erase screen
!     gempty - empty graphics buffer
!     gwicol - set polyline colour
!     gvect  - plot polyline
!     rtxcol - set text colour
!     rtxhei - set text height
!     rtxang - set text angle
!     rtxn   - plot floating point number
!     rtx    - plot text
!     gdot   - plot dot
!     gsegcr - create segment file (device-independent plot metafile)
!     gsegcl - close segment file (device-independent plot metafile)
!     gclose - terminate Uniras
!
!     ----------------------------------------------------------------
!
      subroutine plots(x,y,iroute)
!
!     initialize the plot; this must be the first plot call
!
!     iroute = 1 plots to the screen
!              2 creates a postscript file
!              3 creates a uniras file for the VERSATEC plotter
!              4 creates a postscript file for the colour plotter
!              5 creates a legal size postscript file
!              6 creates an A3 postscript file
!
      common /cplot/ iplot,isep,iseg,nseg,xwndow,ywndow,ibcol,ifcol,sf
!
      character*50 proute
!
      x=x*sf
      y=y*sf
!
      if(iplot.ge.0) then
        if(iroute.lt.2.or.iroute.gt.7) then
          if(x.le.0..or.y.le.0.) then
            proute='select mx11;exit'
          else
            if(x.gt.350.) x=350.
            if(y.gt.265.) y=265.
            ix=nint(x)
            iy=nint(y)
            ixd1=ix/100
            ixd2=(ix-ixd1*100)/10
            ixd3=ix-ixd1*100-ixd2*10
            iyd1=iy/100
            iyd2=(iy-iyd1*100)/10
            iyd3=iy-iyd1*100-iyd2*10
            proute='select mx11;area    ,   ;exit'
            proute(18:20)=char(ixd1+48)//char(ixd2+48)//char(ixd3+48)
            proute(22:24)=char(iyd1+48)//char(iyd2+48)//char(iyd3+48)
          end if
        else
          if(iroute.eq.2) proute='select mpost;exit'
          if(iroute.eq.3) proute='select gv7236;exit'
          if(iroute.eq.4) proute='select hcposta;exit'
          if(iroute.eq.5) proute='select hpostl;exit'
          if(iroute.eq.6) proute='select hposta3;exit'
          if(iroute.eq.7) proute='select hposta4;exit'
        end if
!
!       call groute(proute)
!       call gshmes('SUP','SUP')
!       call gopen
!
        if(ibcol.ne.0) then
!         call grpsiz(xwndow,ywndow)
!         call rrect(0.,0.,xwndow,ywndow,ibcol,0.)
        end if
        if(ifcol.ne.1) then
!         call gwicol(-1.,ifcol)
!         call gwicol(.001,ifcol)
!         call rtxcol(ifcol,ifcol)
        end if
!
      end if
!
      if(iplot.le.0) write(19,5) -1,ibcol,ifcol
5     format(i2/2i10)
!
      return
      end
!
!     ----------------------------------------------------------------
!
      subroutine erase
!
!     erase the screen
!
      common /cplot/ iplot,isep,iseg,nseg,xwndow,ywndow,ibcol,ifcol,sf
!
      if(iplot.ge.0) then
        if(ibcol.ne.0) then
!         call rrect(0.,0.,xwndow,ywndow,ibcol,0.)
        else
!         call gclear
        end if
      end if

!
      if(iplot.le.0) write(19,5) -2
5     format(i2)
!
      return
      end
!
!     ----------------------------------------------------------------
!
      subroutine plot(x,y,ipen)
!
!     move to the point (x,y); pen up if ipen=3, pen down if ipen=2
!
      common /cplot/ iplot,isep,iseg,nseg,xwndow,ywndow,ibcol,ifcol,sf
!
!     if(iplot.ge.0) call gvect(x*sf,y*sf,3-ipen)
!
      if(iplot.le.0) write(19,5) 1,x,y,ipen
5     format(i2/2e15.5,i10)
!
      return
      end
!
!     ----------------------------------------------------------------
!
      subroutine number(x,y,ht,xnum,ang,ndeci)
!
!     plot the floating point number
!
      common /cplot/ iplot,isep,iseg,nseg,xwndow,ywndow,ibcol,ifcol,sf
!
      if(iplot.ge.0) then
!       call rtxhei(ht)
!       call rtxang(ang)
!       call rtxn(xnum,ndeci,x*sf,y*sf)
      end if
!
      if (iplot.le.0) write(19,5) 2,x,y,ht,xnum,ang,ndeci
5     format(i2/5e15.5,i10)
!
      return
      end
!
!     ----------------------------------------------------------------
!
      subroutine symbol(x,y,ht,label,ang,nchar)
!
!     plot the character string label; special symbols can be plotted
!     with the routine ssymbol
!
      common /cplot/ iplot,isep,iseg,nseg,xwndow,ywndow,ibcol,ifcol,sf
!
      character label(nchar)
!
      if(iplot.ge.0) then
!       call rtxhei(ht)
!       call rtxang(ang)
!       call rtx(nchar,label,x*sf,y*sf)
      end if
!
      if(iplot.le.0) write(19,5)    &
        3,nchar,x,y,ht,ang,(label(i),i=1,nchar)
5     format(i2/i10/4e15.5,10000a1)
!
      return
      end
!
!     ----------------------------------------------------------------
!
      subroutine line(x,y,npts)
!
!     connect the (x,y) points with straight line segments
!
      common /cplot/ iplot,isep,iseg,nseg,xwndow,ywndow,ibcol,ifcol,sf
!
      real x(npts),y(npts)
!
      if(iplot.ge.0) then
        do 10 i=1,npts
           x(i)=x(i)*sf
10         y(i)=y(i)*sf
!
!       call gvect(x,y,npts)
!
        do 20 i=1,npts
          x(i)=x(i)/sf
20        y(i)=y(i)/sf
      end if
!
      if(iplot.le.0) then
        write(19,5) 9,npts
        write(19,15) (x(i),y(i),i=1,npts)
5       format(i2/i10)
15      format(2e15.5)
      end if
!
      return
      end
!
!     ----------------------------------------------------------------
!
      subroutine empty
!
!     flush the graphics buffer
!
      common /cplot/ iplot,isep,iseg,nseg,xwndow,ywndow,ibcol,ifcol,sf
!
!     if(iplot.ge.0) call gempty
!
      if(iplot.le.0) write(19,5) 4
5     format(i2)
!
      return
      end
!
!     ----------------------------------------------------------------
!
      subroutine dot(x,y,size,icol)
!
!     plot a dot centred at (x,y) of size isize pixels or mm and colour
!     icol
!
      common /cplot/ iplot,isep,iseg,nseg,xwndow,ywndow,ibcol,ifcol,sf
!
      if(iplot.ge.0) then
!       call gwicol(size,icol)
!       call gdot(sf*x,sf*y,1)
!       call gwicol(-1.,1)
!       call gwicol(.001,ifcol)
      end if
!
      if(iplot.le.0) write(19,5) 10,x,y,size,icol
5     format(i2/3e15.5,i10)
!
      return
      end
!
!     ----------------------------------------------------------------
!
      subroutine pcolor(icol)
!
!     set the colour for polylines
!
      common /cplot/ iplot,isep,iseg,nseg,xwndow,ywndow,ibcol,ifcol,sf
!
!     if(iplot.ge.0) call gwicol(-1.,icol)
!     if(iplot.ge.0) call gwicol(.001,icol)
!
      if(iplot.le.0) write(19,5) 7,icol
5     format(i2/i10)
!
      return
      end
!
!     ----------------------------------------------------------------
!
      subroutine segmnt(iflag)
!
!     open and close Uniras segments
!
      common /cplot/ iplot,isep,iseg,nseg,xwndow,ywndow,ibcol,ifcol,sf
!
      if(iplot.ge.0) then
        if(iseg.eq.0) return
!       if(nseg.gt.0) call gsegcl(nseg)
        if(iflag.eq.1) then
          nseg=nseg+1
!         call gsegcr(nseg)
        end if
      end if
!
      if(iplot.le.0) write(19,5) 8,iflag
5     format(i2/i10)
!
      return
      end
!
!     ----------------------------------------------------------------
!
      subroutine aldone
!
!     wait unitl the user is ready for the next plot
!
      common /cplot/ iplot,isep,iseg,nseg,xwndow,ywndow,ibcol,ifcol,sf
!
      character reply*1
!
      if(iplot.ge.0) then
        write(6,15)
15      format(/'Enter <CR> to continue')
        read(5,25) reply
25      format(a1)
        if(reply(1:1).eq.'s') then
          call plotnd
          stop
        end if
        if(reply(1:1).eq.'0') isep=0
        if(reply(1:1).eq.'1') isep=1
        if(reply(1:1).eq.'2') isep=2
        if(reply(1:1).eq.'3') isep=3
        call segmnt(1)
      end if
!
      if(iplot.le.0) write(19,5) 5
5     format(i2)
!
      return
      end
!
!     ----------------------------------------------------------------
!
      subroutine plotnd
!
!     terminate all Uniras plotting
!
      common /cplot/ iplot,isep,iseg,nseg,xwndow,ywndow,ibcol,ifcol,sf
!
      if(iplot.ge.0) then
        call segmnt(0)
!       call gclose
      end if
!
      if(iplot.le.0) write(19,5) 6
5     format(i2)
!
      return
      end



end module

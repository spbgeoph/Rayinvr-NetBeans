module PlotSubroutines

    use PlotLib

contains
!
!     version 1.3  Aug 1992
!
!     General plot routines
!
!     ----------------------------------------------------------------
!
      subroutine axis(xorig,yorig,amin,amax,amm,ascale,theta,iside,     &
                     amint,amaxt,ntick,ndeci,label,nchar,albht)
!
!     plot axis
!
!     xorig, yorig - (x,y) origin of axis
!     amin, amax   - min and max value of axis
!     amm          - length of axis (mm)
!     ascale       - plot scale in axis-units/plot-units
!     theta        - angle of axis in degrees (i.e. 0 or 90)
!     iside        - side of axis that is labelled
!                    (+1 for left or bottom, -1 for right or top)
!     amint, amaxt - min and max value of tick marks
!     ntick        - number of intervals separated by tick marks
!                    (i.e. the number of tick marks equals ntick+1)
!     ndeci        - number of digits after decimal for axis numbers
!     label        - axis label
!     nchar        - number of characters in axis label
!     albht        - height of axis label
!
!     Note: amax must be greater than amin always
!
      real xtick(101),ytick(101)
      character label(nchar)
!
      whrat=0.75
      ticfac=1.
      ticlen=ticfac*abs(albht)
!
!     plot axis line
!
      call plot(xorig,yorig,3)
      if(theta.eq.0.) then
        call plot(xorig+amm,yorig,2)
        iplt=1
      else
        call plot(xorig,yorig+amm,2)
        iplt=2
      end if
!
      if(ascale.gt.0.) then
        aadj=amin
        aadjt=amint
      else
        aadj=amax
        aadjt=amaxt
      end if
!
!     plot tick marks
!
      ainc=(amaxt-amint)/(abs(ascale)*float(ntick))
      if(iplt.eq.1) then
        xot=(aadjt-aadj)/ascale+xorig
        do 10 i=1,ntick+1
           xtick(i)=xot+float(i-1)*ainc
           ypos=yorig
           call plot(xtick(i),ypos,3)
           ypos=yorig-float(iside)*ticlen
           call plot(xtick(i),ypos,2)
10      continue
      else
        yot=(aadjt-aadj)/ascale+yorig
        do 20 i=1,ntick+1
           ytick(i)=yot+float(i-1)*ainc
           xpos=xorig
           call plot(xpos,ytick(i),3)
           xpos=xorig-float(iside)*ticlen
           call plot(xpos,ytick(i),2)
20      continue
      end if
!
!     plot axis scale
!
      if(albht.lt.0.) return
      if(amaxt.ne.0.) then
        maxdig=int(alog10(abs(amaxt)))+ndeci+2
      else
        maxdig=1
      end if
      anlen=float(maxdig)*albht*whrat
      if(ascale.gt.0.) then
        ainc=(amaxt-amint)/float(ntick)
        a1=amint
      else
        ainc=(amint-amaxt)/float(ntick)
        a1=amaxt
      end if
      if(iplt.eq.1) then
        if(iside.eq.1) then
          shfctr=1.5
        else
          shfctr=.5
        end if
        ypos=ypos-float(iside)*albht*shfctr
        xadj=anlen/2.
        do 30 i=1,ntick+1
           value=a1+float(i-1)*ainc
           xpos=xtick(i)-xadj
           call number(xpos,ypos,albht,value,0.,ndeci)
30      continue
      else
        if(iside.eq.1) then
          shfctr=.5
        else
          shfctr=1.5
        end if
        xpos=xpos-float(iside)*albht*shfctr
        yadj=anlen/2.
        do 40 i=1,ntick+1
           value=a1+float(i-1)*ainc
           ypos=ytick(i)-yadj
           call number(xpos,ypos,albht,value,90.,ndeci)
40      continue
      end if
!
!     plot axis label
!
      alblen=float(nchar)*albht*whrat
      if(iplt.eq.1) then
        ypos=ypos-float(iside)*albht*1.5
        xpos=xorig+(amm-alblen)/2.
        call symbol(xpos,ypos,albht,label,0.,nchar)
      else
        xpos=xpos-float(iside)*albht*1.5
        ypos=yorig+(amm-alblen)/2.
        call symbol(xpos,ypos,albht,label,90.,nchar)
      end if
!
      return
      end
!
!     ----------------------------------------------------------------
!
      subroutine dashln(x,y,npts,dash)
!
!     connect the npts (x,y) points with dashed straight line segments
!     of length dash
!
      parameter (nmax=10000)
      real x(npts),y(npts),xp(nmax),yp(nmax)
!
      nplt=1
      xp(1)=x(1)
      yp(1)=y(1)
      rem=0.
      do 10 i=1,npts-1
         if(rem.gt.0.) then
           dx=x(i+1)-x(i)
           dy=y(i+1)-y(i)
           distxy=sqrt(dx**2+dy**2)
           if(distxy.lt..00001) go to 10
           fctr=rem/distxy
           if(fctr.le.1.) then
             nplt=nplt+1
             if(nplt.gt.nmax) go to 999
             xp(nplt)=x(i)+dx*fctr
             yp(nplt)=y(i)+dy*fctr
           else
             rem=rem-distxy
             go to 10
           end if
         end if
         dx=x(i+1)-xp(nplt)
         dy=y(i+1)-yp(nplt)
         distxy=sqrt(dx**2+dy**2)
         fctr=dash/distxy
         nptss=int(1./fctr+.00001)
         if(nptss.gt.0) then
           do 20 j=1,nptss
              nplt=nplt+1
              if(nplt.gt.nmax) go to 999
              xp(nplt)=xp(nplt-1)+dx*fctr
              yp(nplt)=yp(nplt-1)+dy*fctr
20         continue
         end if
         if(abs(1./fctr-float(nptss)).gt..00001) then
           rem=dash*(1.-1./fctr+float(nptss))
         else
           rem=0.
         end if
10    continue
!
      ipen=3
      do 30 i=1,nplt
         call plot(xp(i),yp(i),ipen)
         if(ipen.eq.3) then
           ipen=2
         else
           ipen=3
         end if
30    continue
      call plot(x(npts),y(npts),ipen)
      return
999   call line(x,y,npts)
!
      return
      end
!
!     ----------------------------------------------------------------
!
      subroutine box(x1,z1,x2,z2)
!
!     plot a box whose opposite corners are (x1,z1) and (x2,z2)
!
      call plot(x1,z1,3)
      call plot(x2,z1,2)
      call plot(x2,z2,2)
      call plot(x1,z2,2)
      call plot(x1,z1,2)
!
      return
      end
!
!     ----------------------------------------------------------------
!
      subroutine ssymbl(x,y,ht,isymbl)
!
!     plot special symbols centred at (x,y) of height ht
!
      ht2=ht/2.
!
      go to (100,200,300,400,500) isymbl
!
!     plot a "+"
!
100   call plot(x,y-ht2,3)
      call plot(x,y+ht2,2)
      call plot(x-ht2,y,3)
      call plot(x+ht2,y,2)
      return
!
!     plot a "x"
!
200   call plot(x-ht2,y-ht2,3)
      call plot(x+ht2,y+ht2,2)
      call plot(x-ht2,y+ht2,3)
      call plot(x+ht2,y-ht2,2)
      return
!
!     plot a "*"
!
300   call plot(x,y-ht2,3)
      call plot(x,y+ht2,2)
      call plot(x-ht2,y,3)
      call plot(x+ht2,y,2)
      call plot(x-ht2,y-ht2,3)
      call plot(x+ht2,y+ht2,2)
      call plot(x-ht2,y+ht2,3)
      call plot(x+ht2,y-ht2,2)
      return
!
!     plot a square
!
400   call plot(x-ht2,y-ht2,3)
      call plot(x+ht2,y-ht2,2)
      call plot(x+ht2,y+ht2,2)
      call plot(x-ht2,y+ht2,2)
      call plot(x-ht2,y-ht2,2)
      return
!
!     plot a diamond
!
500   call plot(x,y-ht2,3)
      call plot(x+ht2,y,2)
      call plot(x,y+ht2,2)
      call plot(x-ht2,y,2)
      call plot(x,y-ht2,2)
!
      return
      end
!
!     ----------------------------------------------------------------
!
      subroutine axtick(amin,amax,amint,amaxt,ntick,ndeci)
!
!     determine appropriate values for the minimum and maximum position
!     of tick marks, the number of tick marks, and the number of
!     digits after the decimal given the minimum and maximum axis
!     values
!
      parameter(ndiv=18)
      integer tcndec(ndiv)
      real tcmax(ndiv),tcinc(ndiv)
!
      data tcmax/.01,.02,.05,.1,.2,.5,1.,2.,5.,10.,20.,50.,         &
                100.,200.,500.,1000.,2000.,5000./,                  &
          tcinc/.001,.002,.005,.01,.02,.05,.1,.2,.5,1.,2.,5.,       &
                10.,20.,50.,100.,200.,500./,                        &
          tcndec/3,3,3,2,2,2,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1/
!
      alen=amax-amin
      do 10 i=1,ndiv
         if(alen.le.tcmax(i)) then
           ipos=i
           go to 20
         end if
10    continue
      ipos=ndiv
!
20    continue
      if(ndeci.lt.-1) ndeci=tcndec(ipos)
      if(amint.lt.-999998.) then
        amint=float(nint(amin/tcinc(ipos)))*tcinc(ipos)
        if(abs(amint-amin).lt..00001) amint=amin
        if(amint.lt.amin) amint=amint+tcinc(ipos)
      end if
      if(amaxt.lt.-999998.) then
        amaxt=float(nint(amax/tcinc(ipos)))*tcinc(ipos)
        if(abs(amaxt-amax).lt..00001) amaxt=amax
        if(amaxt.gt.amax) amaxt=amaxt-tcinc(ipos)
      end if
      if(ntick.le.0) ntick=nint((amaxt-amint)/tcinc(ipos))
      if(abs(amaxt-amint).lt..001) then
        amaxt=amax
        amint=amin
        ntick=1
        ndeci=0
      end if
!
      return
      end


end module

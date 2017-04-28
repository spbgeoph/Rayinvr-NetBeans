module RungeKutta


contains
    subroutine rngkta(x,z,y,f,h,hmin,e,func,g,s,t)
!
!     routine to solve a 2x2 system of first order o.d.e.'s
!     using the runge-kutta method with error control
!
        real y(2),f(2),t(2),s(2),g(2)
        logical*1 be,bh,br,bx
        logical bxx
        common /rkcS/ bxx
        !
        bh=.true.
        br=.true.
        bx=.true.
        bxx=.true.
        if(z > x .and. h < 0.) h=-h
        if(z < x.and.h > 0.) h=-h
        10    xs=x
        !
        g(1)=y(1)
        g(2)=y(2)
        !
        20    hs=h
        q=x+h-z
        be=.true.
        if((h > 0. .and. q >= 0.) .or. (h < 0. .and. q <= 0.)) then
            h=z-x
            br=.false.
        end if
        h3=h/3.
        !
        call func(x,y,f)
        !
        do i=1,2
            q=h3*f(i)
            t(i)=q
            r=q
            y(i)=g(i)+r
        end do

        x=x+h3

        call func(x,y,f)
!
        do i=1,2
            q=h3*f(i)
            r=.5*(q+t(i))
            y(i)=g(i)+r
        end do
        !
        call func(x,y,f)
        !
        do i=1,2
            q=h3*f(i)
            r=3.*q
            s(i)=r
            r=.375*(r+t(i))
            y(i)=g(i)+r
        end do
        x=x+.5*h3

        call func(x,y,f)

        do i=1,2
            q=h3*f(i)
            r=t(i)+4.*q
            t(i)=r
            r=1.5*(r-s(i))
            y(i)=g(i)+r
        end do
        x=x+.5*h

        call func(x,y,f)

        do i=1,2
            q=h3*f(i)
            r=.5*(q+t(i))
            q=abs(r+r-1.5*(q+s(i)))
            y(i)=g(i)+r
            r=abs(y(i))

            if(r < .001) then
                r=e
            else
                r=e*r
            end if

            if(q >= r.and.bx) then
                br=.true.
                bh=.false.
                h=.5*h
                if(abs(h) < hmin) then
                    sigh=1.
                    if(h < 0.) sigh=-1.
                    h=sigh*hmin
                    bx=.false.
                    bxx=.false.
                end if
                y(1)=g(1)
                y(2)=g(2)
                x=xs
                go to 20
            end if

            if(q >= .03125*r) be=.false.
        end do
        !
        if(be.and.bh.and.br) then
            h=h+h
            bx=.true.
        end if

        bh=.true.
        if(br) go to 10
        h=hs
        !
        return
        end subroutine


    subroutine rkdumb(y,x1,x2,derivs)

    !     routine to solve a 2x2 system of first order o.d.e.'s
    !     using a 4th-order runge-kutta method without error control

        dimension y(2),dydx(2),yt(2),dyt(2),dym(2)
        x=x1
        h=x2-x1
        if(x+h.eq.x) return
        !
        call derivs(x,y,dydx)
        !
        hh=h*0.5
        h6=h/6.
        xh=x+hh
        do i=1,2
            yt(i)=y(i)+hh*dydx(i)
        end do

        call derivs(xh,yt,dyt)

        do i=1,2
            yt(i)=y(i)+hh*dyt(i)
        end do

        call derivs(xh,yt,dym)

        do i=1,2
            yt(i)=y(i)+h*dym(i)
            dym(i)=dyt(i)+dym(i)
        end do

        call derivs(x+h,yt,dyt)

        do i=1,2
            y(i)=y(i)+h6*(dydx(i)+dyt(i)+2.*dym(i))
        end do

        return
    end subroutine

    subroutine odexfi(x,y,f)

!   pair of first order o.d.e.'s solved by runge kutta method
!   with x as independent variable

        include 'rayinvr.par'
        real y(2),f(2)
        include 'rayinvr.com'
        external blkdat

        sa=sign(1.,y(2))
        n1=int(sa*y(2)*factan)+1
        f(1)=mcotan(n1)*y(2)+sa*bcotan(n1)
        term1=c(layer,iblk,3)+c(layer,iblk,4)*x
        term2=x*(c(layer,iblk,1)+c(layer,iblk,2)*x)+        &
               term1*y(1)+c(layer,iblk,5)
        vxv=(x*(c(layer,iblk,8)+c(layer,iblk,9)*x)+         &
             c(layer,iblk,10)*y(1)+c(layer,iblk,11))/       &
            (term2*(c(layer,iblk,6)*x+c(layer,iblk,7)))
        vzv=term1/term2
        f(2)=vzv-vxv*f(1)

        return
    end subroutine

    subroutine odezfi(x,y,f)
!   pair of first order o.d.e.'s solved by runge kutta method
!   with z as independent variable
!
        include 'rayinvr.par'
        real y(2),f(2)
        include 'rayinvr.com'
        external blkdat

        !
        sa=sign(1.,y(2))
        n1=int(sa*y(2)*factan)+1
        f(1)=mtan(n1)*y(2)+sa*btan(n1)
        term1=c(layer,iblk,3)+c(layer,iblk,4)*y(1)
        term2=y(1)*(c(layer,iblk,1)+c(layer,iblk,2)*y(1))+      &
               term1*x+c(layer,iblk,5)

        vxv=(y(1)*(c(layer,iblk,8)+c(layer,iblk,9)*y(1))+       &
             c(layer,iblk,10)*x+c(layer,iblk,11))/              &
            (term2*(c(layer,iblk,6)*y(1)+c(layer,iblk,7)))
        vzv=term1/term2
        f(2)=vzv*f(1)-vxv
!
        return
    end subroutine


end module RungeKutta

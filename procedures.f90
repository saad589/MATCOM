module procedures
  ! implicit none

contains
    ! Generates evenly spaced numbers from `from` to `to` (inclusive).
    !
    ! Inputs:
    ! -------
    !
    ! from, to : the lower and upper boundaries of the numbers to generate
    !
    ! Outputs:
    ! -------
    !
    ! array : Array of evenly spaced numbers
    !
    ! Interface:
    ! ---------
    !
    ! interface
    !   subroutine linspace(from, to, array)
    !    real, intent(in) :: from, to
    !    real, intent(out) :: array(:)
    !    real :: range
    !    integer :: n, i
    !   end subroutine linspace
    ! end interface
    !
    !
    ! Example:
    ! -------
    ! real :: array(5)
    ! call linspace(from=0., to=1., array=array)
    !
    subroutine linspace(from, to, array)
        real, intent(in) :: from, to
        real, intent(out) :: array(:)
        real :: range
        integer :: n, i
        n = size(array)
        range = to - from
        if (n == 0) return

        if (n == 1) then
            array(1) = from
            return
        end if
        do i=1, n
            array(i) = from + range * (i - 1) / (n - 1)
        end do
    end subroutine linspace


    subroutine serpent_in_handle(iteration, nodes, f_temp, c_temp, w_temp, w_dens)
        integer, intent(in) :: iteration, nodes
        real, dimension(:), intent(inout) :: f_temp, c_temp, w_temp, w_dens

        character(len=256) :: filename

        ! debug variables
        integer :: ios ! anything other than 0 is an error
        character(len=256) :: iom = "successful"

        ! procedure interfaces
        !interface
        !    subroutine linspace(from, to, array)
        !        real, intent(in) :: from, to
        !        real, intent(out) :: array(:)
        !        real :: range
        !        integer :: n, i
        !    end subroutine linspace
        !end interface

        real :: z(nodes+1) ! axial nodes
        real :: bot=0.0, top=366.0 ! channel dimension

        write(filename,"(a,i0)") "./serpent_files/serp_inp_",iteration
        print "(2a)", "Trying to create ", trim(filename)
        open(unit=11, file=filename, iostat=ios, iomsg=iom)


        !
        ! deck of cell cards
        !
        write(unit=11,fmt="(a)") "                                                                                 "
        write(unit=11,fmt="(a)") "cell c99   0  outside    750                                                     "
        write(unit=11,fmt="(a)") "cell c98   0  fill 1    -750                                                     "
        write(unit=11,fmt="(a)") "                                                                                 "
        do i=1,nodes
            if (i==1) then
                write(unit=11,fmt="(a,i0,a,i0,a,i0)") "cell c",i," 1 fill p",i," -s",i+1
            else if (i==nodes) then
                write(unit=11,fmt="(a,i0,a,i0,a,i0)") "cell c",i," 1 fill p",i," s",i
            else
                write(unit=11,fmt="(a,i0,a,i0,a,i0,a,i0)") "cell c",i," 1 fill p",i," s",i," -s",i+1
            end if
        end do
        write(unit=11,fmt="(a)") "                                                                                 "

        !
        ! deck of surface cards
        !
        write(unit=11,fmt="(a)") "                                                                                 "
        write(unit=11,fmt="(a)") "surf 750  cuboid -0.6300 0.6300 -0.6300 0.6300 0.0  366.0"

        call linspace(from=bot, to=top, array=z)

        do i=1,size(z,1)
           write(unit=11,fmt="(a,i0,a,f11.5)") "surf s",i," pz ",z(i)
        end do
        write(unit=11,fmt="(a)") "                                                                                 "

        !
        ! deck of universe cards
        !
        write(unit=11,fmt="(a)") "                                                                                     "
        do i=1,nodes
            write(unit=11,fmt="(a,i0)") "pin p",i
            write(unit=11,fmt="(a,i0,a)") "f",i," 0.4000"
            write(unit=11,fmt="(a)") "void 0.4180"
            write(unit=11,fmt="(a,i0,a)") "c",i," 0.4750"
            write(unit=11,fmt="(a,i0)") "w",i
            write(unit=11,fmt="(a)") "	 																			   "
        end do


        !
        ! deck of material cards
        !
        write(unit=11,fmt="(a)") "                                                                                     "
        do i=1,nodes
            write(unit=11,fmt="(a,i0,a,f11.5)") "mat f",i," -10.5 tmp ",f_temp(i)
            write(unit=11,fmt="(a)") "92235.70c -0.79230402"
            write(unit=11,fmt="(a)") "92238.70c -0.08803378"
            write(unit=11,fmt="(a)") "8016.70c -0.11966221"
            write(unit=11,fmt="(a,i0,a,f11.5)") "mat c",i," -6.55 tmp ",c_temp(i)
            write(unit=11,fmt="(a)") "40090.70c -1.0"
            write(unit=11,fmt="(a,i0,a,f11.5,a,f11.5,a,i0)") "mat w",i," ",w_dens(i)," tmp ", &
                w_temp(i)," moder lwtr",i," 1001"
            write(unit=11,fmt="(a)") "1001.70c 2"
            write(unit=11,fmt="(a)") "8016.70c 1"
            write(unit=11,fmt="(a,i0,a,f11.5,a)") "therm lwtr",i," ",w_temp(i)," lwtr.15t lwtr.17t"
            write(unit=11,fmt="(a)") "																				   "
        end do

        !
        ! deck of control cards
        !
        write(unit=11,fmt="(a)") "                                                                                 "
        write(unit=11,fmt="(a)") "set pop    10000 300 50                                                        "
        write(unit=11,fmt="(a)") "set bc     2 2 1                                                                 "
        write(unit=11,fmt="(a)") "set acelib   ""/mnt/b/XSdata_endfb70/sss_endfb70.xsdata""                        "
        write(unit=11,fmt="(a)") "                                                                                 "
        write(unit=11,fmt="(a)") "plot 1 126 36600                                                                 "
        write(unit=11,fmt="(a)") "plot 3 1000 1000                                                                 "
        write(unit=11,fmt="(a)") "plot 3 1000 1000 0.0 -3.15 3.15 -3.15 3.15                                       "
        write(unit=11,fmt="(a)") "                                                                                 "
        write(unit=11,fmt="(a)") "set power 6.5159e+04                                                             "
        write(unit=11,fmt="(a)") "det power dr -8 void dm f1 dm f2 dm f3 dm f4 dm f5 dm f6 dm f7 dm f8 dm f9 dm f10"

        close(unit=11, iostat=ios, iomsg=iom)


    end subroutine serpent_in_handle




    subroutine serpent_out_handle(iteration, nodes, linear_pin_power)
        !implicit none
        integer, intent(in) :: iteration, nodes
        ! real, dimension(:), intent(inout) :: linear_pin_power
        real, dimension(1:nodes), intent(inout) :: linear_pin_power

        character(len=256) :: filename
        real, dimension(:,:), allocatable :: raw

        ! debug variables
        integer :: ios ! anything other than 0 is an error
        character(len=256) :: iom = "successful"


        !
        ! open Serpent output detector located at ./serpent_files/
        ! the file is named serp_inp_<i>_det0.m where <i> is the iteration number
        !
        write(filename,"(a,i0,a)") "./serpent_files/serp_inp_",iteration,"_det0.m"
        print "(2a)", "Trying to open ", trim(filename)
        open(unit=12, file=filename, status="old", iostat=ios, iomsg=iom)
        if(ios /= 0) then
            write(*,*) "Error ", trim(iom)
            stop
        end if
        print "(2a)", "File open ", iom

        !
        ! read the data (open the file manually to see what's actually in there)
        !
        read(12, *) ! advance file one record
        read(12, *) ! advance file one record

        allocate(raw(1:nodes,1:12))
        do i=1,nodes
            read(12, *) raw(i,:)
        end do

        print "(a)", "Reading done!"

        !
        ! close file
        !
        close(unit=12, iostat=ios, iomsg=iom)
            if(ios /= 0) then
            write(*,*) "Error ", trim(iom)
            stop
        end if
        print "(2a)", "File close ", iom

        !
        ! enact the side effect
        !
        linear_pin_power = raw(:,11)

    end subroutine serpent_out_handle


end module

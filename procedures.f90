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
        real, dimension(:), intent(in out) :: f_temp, c_temp, w_temp, w_dens

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
        write(unit=11,fmt="(a)") "set pop    100000 300 100                                                        "
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
        real, dimension(1:nodes), intent(in out) :: linear_pin_power

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
        linear_pin_power = raw(:,11)/0.366

    end subroutine serpent_out_handle


    subroutine cobra_in_handle(iteration, nodes, linear_pin_power)
        !implicit none
        integer, intent(in) :: iteration, nodes
        ! real, dimension(:), intent(inout) :: linear_pin_power
        real, dimension(1:nodes), intent(in) :: linear_pin_power
        character(len=256) :: filename
        logical :: ex ! existence
        character(len=256) :: cmd
        ! debug variables
        integer :: ios ! anything other than 0 is an error
        character(len=256) :: iom = "successful"

        real :: z(nodes) ! axial nodes
        real :: bot=0.13, top=3.53 ! channel dimension with padding (m)



        real :: DX, XTAB(nodes), QTAB(nodes), CHAR, CHPW, CHPH, DFUEL, TCLAD, &
            DROD, KFUEL, PEXIT
        integer :: NAXP, HRNUM, RFUEL, RCLAD, CFUEL, KCLAD, CCLAD, HGAP, HIN, &
            GIN

        DX = -0.366 ! Length of the axial XTAB
        NAXP = nodes ! Number of axial levels for power distribution

        call linspace(from=bot, to=top, array=z)
        XTAB = z ! Coordinate of axial level J (m)
        ! XTAB = 0.13:-DX:3.53
        QTAB = linear_pin_power !(1)Linear fission power (W/m)

        ! card_r_1
        CHAR = 8.7878E-05 ! (2)Channel flow area (m2)
        CHPW = 0.02985 ! (3)Channel wetted perimeter (m)
        CHPH = 0.02985 ! (4)Channel heated perimeter (m)
        HRNUM = 1 ! (4.5)no of heated rods in channel (just 1 pin in our channel)

        ! card_r_2
        DFUEL = 0.008 ! (5)Pellet diameter
        TCLAD = 0.00057 ! (6)Cladding thickness
        RFUEL = 10000 ! (7)Fuel density
        RCLAD = 6500 ! (8)Clad density
        DROD = 0.0095 ! (9)Rod diameter

        ! card_r_3
        KFUEL = 3.3 ! (10)Fuel thermal conductivity (W/m/K)
        CFUEL = 300 ! (11)Fuel specific heat (J/kg/K)
        KCLAD = 10 ! (12)Clad thermal conductivity (W/m/K)
        CCLAD = 500 ! (13)Clad specific heat (J/kg/K)
        HGAP = 9400 ! (14)Fuel-to-clad heat transfer coefficient (W/m2/K)

        ! card_r_13
        HIN = 563 ! (15)inlet temperature
        PEXIT = 15.5 ! (17)system exit pressure

        GIN = 4243 ! (16) Inlet mass flux (kg/m2s)

        !
        ! Generate input file
        !
        write(filename,"(a,i0)") "./cobra_files/cobra_inp_",iteration
        print "(2a)", "Trying to create ", trim(filename)
        open(unit=13, file=filename, iostat=ios, iomsg=iom)


        write(unit=13,fmt="(a)") " "
        write(unit=13,fmt="(a)") "     0     2     2     1"
        write(unit=13,fmt="(a,i0,a)") "     2     1     0    ",NAXP,"     1     0     0     5     0     0     0     1"
        write(unit=13,fmt="(a,f5.2)") "   ",DX
        write(unit=13,fmt="(a,i0)") "    ",NAXP


        do i = 1, size(QTAB,1)
            write(unit=13,fmt="(a,f6.4)") " ",XTAB(i)
            write(unit=13,fmt="(es10.4)") QTAB(i)
        end do


        write(unit=13,fmt="(a,es10.4,a,f7.5,a,f7.5,a,i0,a)") "     1  1.0   ",CHAR,"    ",CHPW,"     ",CHPH,"      ", &
            HRNUM,"        1      "
        write(unit=13,fmt="(a,f5.3,a,f7.5,a,i0,a,i0,a,f6.4)") "  ",DFUEL,"       ",TCLAD,"      ",RFUEL,"       ", &
            RCLAD,"        ",DROD
        write(unit=13,fmt="(a,f3.1,a,i0,a,i0,a,i0,a,i0)") "    ",KFUEL,"       ",CFUEL,"          ",KCLAD,"          ", &
            CCLAD,"         ",HGAP
        write(unit=13,fmt="(a)") "     0     0     1     1     0     1     1     0     1     0"
        write(unit=13,fmt="(a)") "     0"
        write(unit=13,fmt="(a)") "$            JSLIP"
        write(unit=13,fmt="(a)") "     0     0     0"
        write(unit=13,fmt="(a)") "     0     0     0     0     0     0"
        write(unit=13,fmt="(a)") " 10.   0.0   0.5  0.0"
        write(unit=13,fmt="(a)") "     3     0     0  0.0      0     0    0.    0.    0."
        write(unit=13,fmt="(a)") "$                  WERRX DAMPP DAMPF                         EPSP ISCHEM"
        write(unit=13,fmt="(a)") "     0     0    0.  0.     0.    0.  -.01     0.     0  0.     0.      2"
        write(unit=13,fmt="(a,i0,a,i0,a,f4.1,a)") "     1  ",HIN,"  ",GIN,"  ",PEXIT, &
            "    -20     1   0.  0.0   0.0  1600.   0.0  0.0"
        write(unit=13,fmt="(a)") "     0     0     0"
        write(unit=13,fmt="(a)") "     0     0     0     0     0"
        write(unit=13,fmt="(a)") "     0     5     3     0     0     0    -1"
        write(unit=13,fmt="(a)") "$EOD"


        close(unit=13)

        !
        ! copy the generated input file into the root directory
        ! and rename the file "INPFILE"
        !
        inquire (file="INPFILE", exist=ex)
        ! print *, ex

        if (ex) then ! if INPFILE exists
            call execute_command_line("rm ./INPFILE")
            print "(a)", "A previous copy of INPFILE exists in the root directory"
            print "(a)", "Removing the old copy of INPFILE..."
        else
            print "(a)", "INPFILE does not exist"
        end if

        write(cmd,"(a,a,a)") "cp ", trim(filename), " ./INPFILE"
        print "(a)", cmd
        call execute_command_line(cmd)
        print "(a)", "A new copy of INPFILE created in the root directory "

    end subroutine cobra_in_handle


    subroutine cobra_out_handle (iteration, nodes, &
        fuel_temp, clad_temp, coolant_temp, coolant_density)
        ! implicit none
        integer, intent(in) :: iteration, nodes
        real, dimension(:), intent(in out) :: fuel_temp, clad_temp, coolant_temp, coolant_density
        character(len=256) :: cmd
        character(len=256) :: filename
        real, dimension(:,:), allocatable :: raw
        integer :: steps

        character(len=256) :: line
        character(len=:), allocatable :: dummy_line_1, dummy_line_2


        ! debug variables
        integer :: ios ! anything other than 0 is an error
        character(len=256) :: iom = "successful"


        !
        ! open Serpent output detector located at ./serpent_files/
        ! the file is named serp_inp_<i>_det0.m where <i> is the iteration number
        !
        write(filename,"(a,i0)") "./cobra_files/cobra_out_",iteration
        write(cmd,"(a,a)") "cp ./OUTFILE ", trim(filename)
        call execute_command_line(cmd)
        print "(2a)", "Trying to open ", trim(filename)
        open(unit=14, file=filename, status="old", iostat=ios, iomsg=iom)

        steps = nodes
        dummy_line_1 = "ASSEMBLY AVERAGED RESULTS"
        dummy_line_2 = "T( 7)"


        do ! until out of words
            read (unit=14, fmt="(a)", iostat=ios) line

            if (index(line,dummy_line_1) /= 0) then
                print "(a)", "Match found!!!!"

                ! skip 4 records
                read (unit=14, fmt="(a)", iostat=ios)
                read (unit=14, fmt="(a)", iostat=ios)
                read (unit=14, fmt="(a)", iostat=ios)
                read (unit=14, fmt="(a)", iostat=ios)

                allocate(raw(1:nodes+1,1:10))
                do i=1,nodes+1
                    read(14, *) raw(i,:)
                end do

                !
                ! coolant temperature
                !
                print *
                print "(a)", "Coolant temperature at edge points: "
                print "(f11.5)", raw(:,4)

                print *
                print "(a)", "Coolant mean temperature: "
                ! calculate the mean node value by averaging the edge values
                coolant_temp = 0.5*(raw(1:nodes,4)+raw(2:nodes+1,4))
                print "(f11.5)",coolant_temp

                !
                ! Coolant density
                !
                print *
                print "(a)", "Coolant density at edge points: "
                print "(f11.5)", raw(:,5)

                print *
                print "(a)", "Coolant mean density: "
                ! calculate the mean node value by averaging the edge values
                coolant_density = 0.5*(raw(1:nodes,5)+raw(2:nodes+1,5))
                print "(f11.5)",coolant_density

            end if


            if (index(line,dummy_line_2) /= 0) then
                print "(a)", "Match found!!!!"

                ! skip 1 record
                read (unit=14, fmt="(a)", iostat=ios)

                deallocate(raw)
                allocate(raw(1:nodes,1:12))
                do i=1,nodes
                    read(14, *) raw(i,:)
                end do

                !
                ! Pellet mean temperature
                !
                print *
                print "(a)", "Pellet mean temperature: "
                fuel_temp = raw(:,7)
                print "(f11.5)", raw(:,7)

                !
                ! Cladding mean temperature
                !
                print *
                print "(a)", "Cladding mean temperature: "
                clad_temp = raw(:,11)
                print "(f11.5)", raw(:,11)

            end if


            if (ios < 0) exit ! End of file
            print *, trim(line)
        end do

        ! read(14, "(a)") line
        ! print "(a)", line



        close(unit=14)
















    end subroutine cobra_out_handle























end module

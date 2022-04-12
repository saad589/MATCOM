module cobraen_procedures
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


    ! Generates COBRA-EN input file for a given linear power profile.
    !
    ! Inputs:
    ! -------
    !
    ! interation : an integer to numerically tag the input file
    ! nodes : the number of axial nodes
    ! linear_pin_power : the linear power distribution corresponding to the nodes
    !
    ! Outputs:
    ! -------
    !
    ! (No side effects)
    !
    ! Interface:
    ! ---------
    !
    ! TBD
    !
    ! Example:
    ! -------
    ! 
    ! integer :: i, nodes 
    ! real :: array(:,:)
    ! call cobra_in_handle(i, nodes, array(:,i))
    !
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

    ! Read COBRA-EN output file and returns fuel, cladding, and water
    ! temperature profiles. Additionally retuns water density. 
    !
    ! Inputs:
    ! -------
    !
    ! interation : an integer to numerically tag the input file
    ! nodes : the number of axial nodes
    !
    ! Outputs:
    ! -------
    !
    ! fuel_temp : the fuel temperature distribution corresponding to the nodes
    ! clad_temp : the cladding temperature distribution corresponding to the nodes
    ! coolant_temp : the coolant temperature distribution corresponding to the nodes
    ! coolant_density : the coolant density distribution corresponding to the nodes
    !
    ! Interface:
    ! ---------
    !
    ! TBD
    !
    ! Example:
    ! -------
    ! 
    ! TBD 
    !
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
        character(len=:), allocatable :: search_string_1, search_string_2

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
        search_string_1 = "ASSEMBLY AVERAGED RESULTS"
        search_string_2 = "T( 7)"


        do ! until out of words
            read (unit=14, fmt="(a)", iostat=ios) line

            if (index(line,search_string_1) /= 0) then
				print *
                print "(a)", "Match found!!!!"
				print *

                ! skip 4 records
                read (unit=14, fmt="(a)", iostat=ios)
                read (unit=14, fmt="(a)", iostat=ios)
                read (unit=14, fmt="(a)", iostat=ios)
                read (unit=14, fmt="(a)", iostat=ios)

                allocate(raw(1:nodes+1,1:10)) ! no edge points -> no of nodes + 1 
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
                coolant_density = -1.0E-3*0.5*(raw(1:nodes,5)+raw(2:nodes+1,5))
                print "(f11.5)",coolant_density

            end if

            if (index(line,search_string_2) /= 0) then
				print *
                print "(a)", "Match found!!!!"
				print * 
				
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

end module cobraen_procedures

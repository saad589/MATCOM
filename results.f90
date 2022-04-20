module results
    ! implicit none

contains

    subroutine print_result(iteration, nodes, linear_pin_power_dist, &
        fuel_temp_dist, clad_temp_dist, coolant_temp_dist, coolant_density_dist)
        ! implicit none
        integer, intent(in) :: iteration, nodes
        real, dimension(:,:), intent(in) :: linear_pin_power_dist, fuel_temp_dist, &
            clad_temp_dist, coolant_temp_dist, coolant_density_dist
        character(len=256) :: cmd
        character(len=256) :: filename
         ! debug variables
        integer :: ios ! anything other than 0 is an error
        character(len=256) :: iom = "successful"

        character(len=32) :: my_fmt
        ! my_fmt = "(10f11.5)"
        ! write(my_fmt,"(a)") "(10f11.5)"
        write(my_fmt,"(a,i0,a)") "(",nodes,"f13.5)"



        write(filename,"(a)") "matcom_result.txt"
        print "(2a)", "Writing results to ", trim(filename)
        open(unit=69, file=filename, iostat=ios, iomsg=iom)




        write(unit=69,fmt="(a,i0)") "Number of nodes: ", nodes
        write(unit=69,fmt="(a,i0)") "Number of iterations: ", iteration

        write(unit=69,fmt="(a)") "Linear pin power distribution (W/m): "
        write(unit=69,fmt=my_fmt) linear_pin_power_dist
        write(unit=69,fmt="(a)") "Axial fuel temperature distribution (K): "
        ! do i=1,size(fuel_temp_dist,1)
        !     write(unit=69,fmt="(6f11.5)") fuel_temp_dist(i,:)
        ! end do
        write(unit=69,fmt=my_fmt) fuel_temp_dist
        write(unit=69,fmt="(a)") "Axial cladding temperature distribution (K): "
        write(unit=69,fmt=my_fmt) clad_temp_dist
        write(unit=69,fmt="(a)") "Axial coolant temperature distribution (K): "
        write(unit=69,fmt=my_fmt) coolant_temp_dist
        write(unit=69,fmt="(a)") "Axial coolant density distribution (K): "
        write(unit=69,fmt=my_fmt) coolant_density_dist



        close(unit=69)

    end subroutine print_result



    subroutine print_result_matlab(iteration, nodes, linear_pin_power_dist, &
        fuel_temp_dist, clad_temp_dist, coolant_temp_dist, coolant_density_dist)
        ! implicit none
        integer, intent(in) :: iteration, nodes
        real, dimension(:,:), intent(in) :: linear_pin_power_dist, fuel_temp_dist, &
            clad_temp_dist, coolant_temp_dist, coolant_density_dist
        character(len=256) :: cmd
        character(len=256) :: filename
         ! debug variables
        integer :: ios ! anything other than 0 is an error
        character(len=256) :: iom = "successful"

        character(len=32) :: my_fmt
        ! my_fmt = "(10f11.5)"
        ! write(my_fmt,"(a)") "(10f11.5)"
        write(my_fmt,"(a,i0,a)") "(",nodes,"f13.5)"



        write(filename,"(a)") "matcom_res.m"
        print "(2a)", "Writing results to ", trim(filename)
        open(unit=69, file=filename, iostat=ios, iomsg=iom)



        write(unit=69,fmt="(a,i0,a)") "N_NODE = ", nodes, ";"
        write(unit=69,fmt="(a,i0,a)") "N_ITERATION = ", iteration, ";"

        write(unit=69,fmt=*) " " ! indent

        write(unit=69,fmt="(a)") "% Linear pin power distribution (W/m)"
        write(unit=69,fmt="(a)") "LIN_POWER = ["
        write(unit=69,fmt=my_fmt) linear_pin_power_dist
        write(unit=69,fmt="(a)") "];"

        write(unit=69,fmt=*) " " ! indent

        write(unit=69,fmt="(a)") "% Axial fuel temperature distribution (K)"
        write(unit=69,fmt="(a)") "FUEL_TEMP = ["
        write(unit=69,fmt=my_fmt) fuel_temp_dist
        write(unit=69,fmt="(a)") "];"
        ! do i=1,size(fuel_temp_dist,1)
        !     write(unit=69,fmt="(6f11.5)") fuel_temp_dist(i,:)
        ! end do

        write(unit=69,fmt=*) " " ! indent

        write(unit=69,fmt="(a)") "% Axial cladding temperature distribution (K)"
        write(unit=69,fmt="(a)") "CLAD_TEMP = ["
        write(unit=69,fmt=my_fmt) clad_temp_dist
        write(unit=69,fmt="(a)") "];"

        write(unit=69,fmt=*) " " ! indent

        write(unit=69,fmt="(a)") "% Axial coolant temperature distribution (K)"
        write(unit=69,fmt="(a)") "COOLANT_TEMP = ["
        write(unit=69,fmt=my_fmt) coolant_temp_dist
        write(unit=69,fmt="(a)") "];"

        write(unit=69,fmt=*) " " ! indent

        write(unit=69,fmt="(a)") "% Axial coolant density distribution (K)"
        write(unit=69,fmt="(a)") "COOLANT_DENSITY = ["
        write(unit=69,fmt=my_fmt) coolant_density_dist
        write(unit=69,fmt="(a)") "];"



        close(unit=69)

    end subroutine print_result_matlab



    subroutine print_result_csv(iteration, nodes, linear_pin_power_dist, &
        fuel_temp_dist, clad_temp_dist, coolant_temp_dist, coolant_density_dist)
        ! implicit none
        integer, intent(in) :: iteration, nodes
        real, dimension(:,:), intent(in) :: linear_pin_power_dist, fuel_temp_dist, &
            clad_temp_dist, coolant_temp_dist, coolant_density_dist
        character(len=256) :: cmd
        character(len=256) :: filename
         ! debug variables
        integer :: ios ! anything other than 0 is an error
        character(len=256) :: iom = "successful"

        character(len=32) :: my_fmt
        ! my_fmt = "(10f11.5)"
        ! write(my_fmt,"(a)") "(10f11.5)"
        write(my_fmt,"(a,i0,a)") "(",nodes,"f13.5)"



        write(filename,"(a)") "matcom_result.csv"
        print "(2a)", "Writing results to ", trim(filename)
        open(unit=69, file=filename, iostat=ios, iomsg=iom)




        write(unit=69,fmt="(a,i0)") "Number of nodes: ", nodes
        write(unit=69,fmt="(a,i0)") "Number of iterations: ", iteration

        write(unit=69,fmt="(a)") "Linear pin power distribution (W/m): "
        write(unit=69,fmt=my_fmt) linear_pin_power_dist
        write(unit=69,fmt="(a)") "Axial fuel temperature distribution (K): "
        ! do i=1,size(fuel_temp_dist,1)
        !     write(unit=69,fmt="(6f11.5)") fuel_temp_dist(i,:)
        ! end do
        write(unit=69,fmt=my_fmt) fuel_temp_dist
        write(unit=69,fmt="(a)") "Axial cladding temperature distribution (K): "
        write(unit=69,fmt=my_fmt) clad_temp_dist
        write(unit=69,fmt="(a)") "Axial coolant temperature distribution (K): "
        write(unit=69,fmt=my_fmt) coolant_temp_dist
        write(unit=69,fmt="(a)") "Axial coolant density distribution (K): "
        write(unit=69,fmt=my_fmt) coolant_density_dist



        close(unit=69)

    end subroutine print_result_csv










end module results

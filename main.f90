program main
! This is the head file of the coupling interface
! Written by SAAD ISLAM on 08 MAR 2022

use serpent_procedures
use cobraen_procedures
use results

! implicit none
! integer, parameter :: nodes = 10
! integer, parameter :: iteration = 5
! real, dimension(nodes,iteration) :: linear_pin_power_dist ! Serpent output
! real, dimension(nodes,iteration+1) :: fuel_temp_dist, clad_temp_dist, &
! 	coolant_temp_dist, coolant_density_dist
	
integer :: nodes, iteration
real, dimension(:,:), allocatable :: linear_pin_power_dist, fuel_temp_dist, clad_temp_dist, &
	coolant_temp_dist, coolant_density_dist

logical :: ex ! existence
character(len=256) :: cmd



character(len=256) :: dummy_char
integer :: dummy_int
real :: dummy_real

!
! ASCII art
!
print "(a)", " __  __    _  _____ ____ ___  __  __ "
print "(a)", "|  \/  |  / \|_   _/ ___/ _ \|  \/  |"
print "(a)", "| |\/| | / _ \ | || |  | | | | |\/| |"
print "(a)", "| |  | |/ ___ \| || |__| |_| | |  | |"
print "(a)", "|_|  |_/_/   \_\_| \____\___/|_|  |_|"
print "(a)", "MATCOM is an external neutonics/TH coupling interface"
print "(a)", "developed by SAAD ISLAM AMEI"
print *
print *

!
! Read the configuration file 
!
open(unit=50, file="matcom.conf")
print *, iom
read(50, *) nodes
read(50, *) iteration
read(50, *) ! skip
close(unit=50)

allocate(linear_pin_power_dist(nodes,iteration))
allocate(fuel_temp_dist(nodes,iteration+1))
allocate(clad_temp_dist(nodes,iteration+1))
allocate(coolant_temp_dist(nodes,iteration+1))
allocate(coolant_density_dist(nodes,iteration+1))

!
! Create Serpent directory to store I/O files
!
inquire (file="serpent_files", exist=ex)
! print *, ex
if (.not.ex) then ! if does not exist
    call execute_command_line("mkdir serpent_files")
    print "(a)", "Serpent dir cleated successfully "
else
    print "(a)", "Serpent dir exists"
end if

!
! Create COBRA-EN directory to store I/O files
!
inquire (file="cobra_files", exist=ex)
! print *, ex
if (.not.ex) then ! if does not exist
    call execute_command_line("mkdir cobra_files")
    print "(a)", "COBRA-EN dir cleated successfully "
else
    print "(a)", "COBRA-EN dir exists"
end if

! Initial temperate and density distribution
fuel_temp_dist(:,1) = 900.0 ! fuel temperature (K) at nodes
clad_temp_dist(:,1) = 600.0 ! cladding temperature (K) at nodes
coolant_temp_dist(:,1) = 600.0 ! water temperature (K) at nodes
coolant_density_dist(:,1) = -0.6 ! water density (gm/cm3) at nodes as a function of node temperature

do i=1,iteration
    
	! Print the current iteration number
	print *
    print "(a,i0)", "iteration no: ", i
    print *

    ! Generate Serpent input file
    call serpent_in_handle(i, nodes, fuel_temp_dist(:,i), clad_temp_dist(:,i), &
        coolant_temp_dist(:,i), coolant_density_dist(:,i))

    ! Run Serpent
    write(cmd,"(a,i0,a)") "sss2 ./serpent_files/serp_inp_",i," -omp 8"
    print "(a)", cmd
    call execute_command_line(cmd)

    ! Read Serpent output
    call serpent_out_handle(i, nodes, linear_pin_power_dist(:,i))

    ! generate COBRA-EN input
    call cobra_in_handle(i, nodes, linear_pin_power_dist(:,i))

    ! Run COBRA-EN
    write(cmd,"(a)") "./cobraen"
    print "(a)", cmd
    call execute_command_line(cmd)

    ! Read COBRA-EN output
    call cobra_out_handle(i, nodes, fuel_temp_dist(:,i+1), &
        clad_temp_dist(:,i+1), coolant_temp_dist(:,i+1), coolant_density_dist(:,i+1))

end do

! print *, size(linear_pin_power_dist,1), size(linear_pin_power_dist,2)
! print "(f11.5)", linear_pin_power_dist(:,3)

!
! Export results 
!
call print_result_matlab(iteration, nodes, linear_pin_power_dist, &
        fuel_temp_dist, clad_temp_dist, coolant_temp_dist, coolant_density_dist)


end program main

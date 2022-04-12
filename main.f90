program master
! This is the head file of the coupling interface
! Written by SAAD ISLAM on 08 MAR 2022

use procedures

! implicit none
integer, parameter :: nodes = 10
integer, parameter :: iteration = 5
real, dimension(nodes,iteration) :: linear_pin_power_dist ! Serpent output
real, dimension(nodes,iteration+1) :: fuel_temp_dist, clad_temp_dist, &
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
! Create Serpent directory to store I/O files
!
inquire (file="serpent_files", exist=ex)
! print *, ex

if (.not.ex) then ! if not exists
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

if (.not.ex) then ! if not exists
    call execute_command_line("mkdir cobra_files")
    print "(a)", "COBRA-EN dir cleated successfully "
else
    print "(a)", "COBRA-EN dir exists"
end if


fuel_temp_dist(:,1) = 900.0 ! fuel temperature (K) at nodes
clad_temp_dist(:,1) = 600.0 ! cladding temperature (K) at nodes
coolant_temp_dist(:,1) = 600.0 ! water temperature (K) at nodes
coolant_density_dist(:,1) = -0.6 ! water density (gm/cm3) at nodes as a function of node temperature

do i=1,iteration
    write(cmd,"(a,i0,a)") "sss2 ./serpent_files/serp_inp_",i," -omp 8"
    ! write(cmd,"(a)") "pwd"
    print "(a)", cmd
    call execute_command_line(cmd)
    print *, "iteration no: ", i
    call serpent_in_handle(i, nodes, fuel_temp_dist(:,1), clad_temp_dist(:,1), &
        coolant_temp_dist(:,1), coolant_density_dist(:,1))
    call serpent_out_handle(i, nodes, linear_pin_power_dist(:,i))
end do

print *, size(linear_pin_power_dist,1), size(linear_pin_power_dist,2)

print "(f11.5)", linear_pin_power_dist(:,3)







end program master

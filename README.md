# MATCOM

![Fortran](https://img.shields.io/badge/Fortran-%23734F96.svg?style=for-the-badge&logo=fortran&logoColor=white)
![Linux](https://img.shields.io/badge/Linux-FCC624?style=for-the-badge&logo=linux&logoColor=black)

MATCOM is a simple interface for coupled neutron transport (Monte Carlo)
and thermo-hydraulics (subchannel) external iterative calculation.
The scheme obtains results of the coupled solutions with Serpent/COBRA-EN coupled codes.


This repository contains:

1. Fortran source files.
2. A CMake profile. 
3. Readme and licence declaration.

Both Serpent and COBRA-EN are distribution controlled nuclear codes and must be obtained though OECD/NEA or RSICC (or similar entities).  

## Adapting this Repository

To use the MATCOM repository, run the following commands:

```sh
$ git clone --branch master https://github.com/saad589/matcom.git MATCOM
$ cd MATCOM
$ rm -rf .git/
$ git init
```

## CMake Build

This project uses [CMake](https://cmake.org/) to control the compilation process. GNU Fortran compilers should be present in the system. Generate the configuration files with the following commands: 

```sh
$ cmake -E chdir ./Build cmake ./..
```

After generating the makefiles, compile MATCOM using the following command: 
```sh
$ make -C Build 
```

This will build the libraries and the executable called "matcom" inside the Build directory. Additionally, as bash script is provided to automate the build process (deprecated). 

## Usage

Serpent sould be present in the cluster as symlink. The executable of COBRA-EN should be placed on the same directory as with the executable of MATCOM or the directory where MATCOM is invoked. Run MATCOM by simply typing:  

```sh
$ matcom 
```
MATCOM will create new directories and accumulate results of the iterative calculation. 


## Maintainers

[@saad589](https://github.com/saad589).

## Contributing

Feel free to dive in! [Open an issue](https://github.com/saad589/matcom/issues/new) or submit PRs.

MATCOM follows the [Contributor Covenant](http://contributor-covenant.org/version/1/3/0/) Code of Conduct.


## License

[GPL](LICENSE) © Saad Islam

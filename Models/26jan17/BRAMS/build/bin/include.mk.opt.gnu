#Makefile include include.mk.opt
############################## Change Log ##################################
# 1.0.0.2
#
# 000908 MJB include.mk-mrc ##
#            Added MAKE environment varaible.
#            Added free format option to F_OPTS for some platforms. ##
# 000907 MJB include.mk-mrc ##
#            Changed the defualts to no NCAR Graphics and no parallel.
#            Also commented out the machine specifics to force the user to
#            select the appropriate machine for them. ##
# 000823 MJB include.mk-mrc ##
#            New - defines all make environment varaibles and is included
#            in all make files. ##
#
############################################################################

# Define make (gnu make works best).

MAKE=/usr/bin/make

# libraries.

BASE=$(BRAMS_ROOT)/build/
LIBUTILS=$(BASE)/libutils-$(UTILS_VERSION)-opt.a

# Activate appropriate parts below, comment out others.

# NCAR Graphics.

#---------------------------------------------------------------
# If you are using a standard installation of NCAR Graphics, set:
#       LOADER=ncargf90
# in the machine-dependent sections below
#LIBNCARG=
#---------------------------------------------------------------
# If you are using the NCAR dummy libraries...

NCARG_DIR=$(BASE)
#LIBNCARG=-L$(NCARG_DIR) -lncarg-$(UTILS_VERSION) -lncarg_c-$(UTILS_VERSION) \
#                        -lncarg_gks-$(UTILS_VERSION)
#LIBNCARG=-L$(NCARG_DIR) -lncarg-$(UTILS_VERSION) 
LIBNCARG=$(BASE)/libncarg-$(UTILS_VERSION).a
#---------------------------------------------------------------
# If you are using a real distribution of NCAR Graphics...
#NCARG_DIR=/usr/local/ncarg-4.3.0/lib
#LIBNCARG=-L$(NCARG_DIR) -lncarg -lncarg_gks -lncarg_c -L/usr/X11R6/lib -lX11 -ldl
#---------------------------------------------------------------

# HDF libraries-------------------------------------------------
# HDF4 library: Instructions to install: www.cptec.inpe.br/brams
# If you run "configure" script, you don't need to edit line bellow
USE_HDF4=0
HDF4_LIBS=-L./.hdf4_libs -lmfhdf -ldf -lz -ljpeg -lsz
# --------------------------------------------------------------

# MPI_Wtime. ---------------------------------------------------
# If USE_MPIWTIME=1, then it will use MPI libraries to compute
# the wall time (the only double-precision intrinsic).  In case
# you don't have it, leave USE_MPIWTIME=0, in which case it will
# use a simpler, single-precision function.
USE_MPIWTIME=0


#----------------- SGI -n32 ------------------------------------
#CMACH=SGI
#F_COMP=f90
#F_OPTS=-n32 -mips4 -r10000 -O2 -freeform \
#       -OPT:IEEE_arithmetic=3:roundoff=3 -OPT:fold_arith_limit=3200
#C_COMP=cc
#C_OPTS=-O -n32 -mips4 -DUNDERSCORE
#LOADER=f90
#LOADER_OPTS=-n32
#LIBS=-lm
#-----------------------------------------------------------------

#----------------- IBM ------------------------------------
#CMACH=IBM
#F_COMP=f77
#F_OPTS=-O -NA18000 -NQ30000 -qcharlen=10000 -k
#C_COMP=cc
#C_OPTS=-O
#C_PP_OPTS=-P
#LOADER=f77
#LOADER_OPTS=
#LIBS=
#-----------------------------------------------------------------

#----------------- IBM mpxl ------------------------------------
#CMACH=IBM
#F_COMP=mpxlf90
#F_OPTS=-O3 -qstrict -qtune=pwr3 -qarch=pwr3 -NA18000 -NQ30000 -qcharlen=10000 \
#       -qmaxmem=-1
#C_COMP=mpcc
#C_OPTS=-w -O
#C_PP_OPTS=-P
#LOADER=mpxlf90
#LOADER_OPTS=-bmaxdata:1024000000 -bmaxstack:1024000000 -lmassts
#LIBS=
#-----------------------------------------------------------------

#----------------- Sun -------------------------------------------
#CMACH=SUN
#F_COMP=f90
#F_OPTS=-O4
#C_COMP=cc
#C_OPTS=
#LOADER=f90
#LOADER_OPTS=
#LIBS=
#-----------------------------------------------------------------

#----------------- HP/Exemplar ------------------------------------
#CMACH=HP
#F_COMP=f90
#F_OPTS=+O2 +source=free +U77 +Olibcalls +Odataprefetch +Ofltacc
#C_COMP=cc
#C_OPTS=-O
#LOADER=f90
#LOADER_OPTS=
#LIBS=-Wl,-aarchive_shared -lm -lcnx_syscall /lib/libail.sl -lU77 -lc      
#-----------------------------------------------------------------

#----------------- HP/K460 ---------------------------------------
#CMACH=HP
#F_COMP=f90
#F_OPTS=+O2 +U77 +Olibcalls +Odataprefetch +Ofltacc +DA2.0 +DS2.0
#C_COMP=/usr/bin/cc
#C_OPTS=-O -Aa +e -D_HPUX_SOURCE
#LOADER=f90
#LOADER_OPTS=
#LIBS=-lm
#-----------------------------------------------------------------

#----------------------DEC/Compaq Alpha--------------------------
#CMACH=ALPHA
#F_COMP=f90
#F_OPTS=-O -fpe4 -NA18000 -NQ30000 -qcharlen=10000
#C_COMP=cc
#C_OPTS=-DUNDERSCORE -DLITTLE
#LOADER=f90
#LOADER_OPTS=
#LIBS=
#-----------------------------------------------------------------

#-----------------  LINUX Portland Group pgf77/gcc ---------------
#CMACH=PC_LINUX1
#F_COMP=mpif90
#C_COMP=mpicc
#LOADER=mpif90
#C_LOADER=mpicc
#LIBS=
#MOD_EXT=mod
#Compiler options:
#F_OPTS=-Mvect=cachesize:262144,sse -Munroll -Mnoframe -O2 -pc 64 -Mfree 
#If using AMD Athlon
# -tp athlonxp -fastsse
#C_OPTS=-O3 -DUNDERSCORE -DLITTLE
#LOADER_OPTS=-v -Wl,-static
#-----------------------------------------------------------------

#----------------  NEC-SX6 ---------------
#CMACH=NEC_SX
#F_COMP=sxmpif90
#C_COMP=sxmpic++
#LOADER=sxmpif90
#C_LOADER=sxmpic++
#LIBS=
#MOD_EXT=mod
#Compiler options:
#F_OPTS=-eC -C debug -Wf "-cont -init stack=nan heap=nan"
#F_OPTS=-ftrace
#F_OPTS=-C ssafe
#F_OPTS=
#C_OPTS=-f90lib
#C_OPTS=-f90lib -C ssafe
#C_OPTS=-f90lib -ftrace
#LOADER_OPTS=-eC -C debug -f90lib -Wf "-cont -init stack=nan heap=nan"
#LOADER_OPTS=-C debug -f90lib
#LOADER_OPTS=-ftrace
#LOADER_OPTS=
#-----------------------------------------------------------------

#-----------------  LINUX INTEL FORTRAN-95 Compiler/GCC  ---------
#CMACH=PC_LINUX1
#F_COMP=mpif90
#C_COMP=mpicc
#LOADER=mpif90
#C_LOADER=mpicc
#LIBS=
#MOD_EXT=mod
#Compiler options
#F_OPTS=-tpp6 -FR -O3 -Vaxlib -static
#C_OPTS=-tpp6 -O3 -DLITTLE
#LOADER_OPTS=-tpp6 -O3 -static -Vaxlib
#C_LOADER_OPTS=-v
#-----------------------------------------------------------------

#-----------------  LINUX G95 compiler g95/gcc  ------------------
CMACH=PC_LINUX1
F_COMP=mpif90
C_COMP=mpicc
LOADER=mpif90
C_LOADER=mpicc
LIBS=
MOD_EXT=mod
#Compiler options
F_OPTS=-O3 -static -fno-second-underscore
C_OPTS=-O3 -DLITTLE
LOADER_OPTS=-O3 -static -fno-second-underscore
C_LOADER_OPTS=-v -static
#-----------------------------------------------------------------

# If compiling for a single-CPU platform only (without MPI):

#-----------------------------------------------------------------
#PAR_LIBS=
#PAR_DEFS=
#-----------------------------------------------------------------

# Else if using MPI libraries:

#---------------SGI-----------------------------------------------
#with mpich parallel stuff
#MPI_PATH=/usr/local/mpich
#PAR_INCS=-I$(MPI_PATH)/include
#PAR_LIBS=-L$(MPI_PATH)/lib/IRIXN32/ch_shmem -lmpi
#  or with SGI Parallel stuff
#PAR_LIBS=-L/usr/lib32 -lmpi
#  need this for both
#PAR_DEFS=-DRAMS_MPI
#-----------------------------------------------------------------

#---------------IBM-----------------------------------------------
#MPI_PATH=/usr/local/mpich
#PAR_INCS=-I$(MPI_PATH)/include
#PAR_LIBS=-L$(MPI_PATH)/lib/rs6000/ch_p4 -lmpi 
#PAR_DEFS=-DRAMS_MPI
#-----------------------------------------------------------------

#---------------Sun-----------------------------------------------
#MPI_PATH=/usr/local/mpich
#PAR_INCS=-I$(MPI_PATH)/include
#PAR_LIBS=-L$(MPI_PATH)/lib/solaris/ch_p4 -lmpi 
#PAR_DEFS=-DRAMS_MPI
#-----------------------------------------------------------------

#---------------HP-Exemplar---------------------------------------
#MPI_PATH=/opt/mpi
#PAR_INCS=-I$(MPI_PATH)/include
#PAR_LIBS=$(MPI_PATH)/lib/pa1.1/libmpi.a
#PAR_DEFS=-DRAMS_MPI
#-----------------------------------------------------------------

#---------------LINUX Portland Group pgf77/gcc--------------------
#MPI_PATH=/usr/local/mpich
#PAR_INCS=-I$(MPI_PATH)/include
#PAR_LIBS=-L$(MPI_PATH)/lib/LINUX/ch_p4 -lmpich
#PAR_DEFS=-DRAMS_MPI
#MPI_PATH=/shared/tools/mpichp4-1.2.4
#PAR_INCS=-I$(MPI_PATH)/include
#PAR_LIBS=-L$(MPI_PATH)/lib -lmpich -lpmpich
#PAR_DEFS=-DRAMS_MPI
#-----------------------------------------------------------------

#---------------If using scritps 'mpicc' e 'mpif90'---------------'
PAR_DEFS=-DRAMS_MPI
#-----------------------------------------------------------------

# For IBM,HP,SGI,ALPHA,LINUX use these:
ARCHIVE=ar rs
# For NEC SX-6
#ARCHIVE=sxar rs
# For SUN,CONVEX
#ARCHIVE=ar r'


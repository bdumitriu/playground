####### kdevelop will overwrite this part!!! (begin)##########
bin_PROGRAMS = so

## INCLUDES were found outside kdevelop specific part

so_SOURCES = main.cpp 
so_LDADD   = ./driver/libdriver.a ./exceptions/libexceptions.a ./utils/libutils.a 

SUBDIRS = driver exceptions include utils 

EXTRA_DIST = main.cpp 

####### kdevelop will overwrite this part!!! (end)############
# set the include path found by configure
INCLUDES= $(all_includes)
 
# the library search path.
so_LDFLAGS = $(all_libraries) 
                                                                                

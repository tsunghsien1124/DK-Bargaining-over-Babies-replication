# this is the makefile to compile and run the fortran program

# define the output file
# executable = program  # for unix based machines
# executable = program.exe  # for windows machines
ifeq ($(OS),Windows_NT)     # is Windows_NT on XP, 2000, 7, Vista, 10...
    executable = program.exe
else
    executable = program
endif

# define the compiler (default is gfortran)
f90comp = gfortran

# define the compiler switches (-ffree-line-length-0 -fopenmp needed for correct execution)
switches = -O3 -ffree-line-length-0 -fopenmp

exec/$(executable) : exec/globals.o exec/toolbox.o exec/output.o exec/wage_distribution.o exec/model.o exec/simulated_annealing.o exec/estimation.o exec/policy_analysis.o exec/fertdec.o 
	$(f90comp) -J./exec -I./exec -o exec/$(executable) $(switches) exec/globals.o exec/toolbox.o exec/output.o exec/wage_distribution.o exec/model.o exec/simulated_annealing.o exec/estimation.o exec/policy_analysis.o exec/fertdec.o 
	
exec/globals.o : src/globals.f90
	$(f90comp) -c $(switches) src/globals.f90 -J./exec -I./exec -o exec/globals.o
	
exec/toolbox.o : src/toolbox.f90
	$(f90comp) -c $(switches) src/toolbox.f90 -J./exec -I./exec -o exec/toolbox.o
	
exec/output.o : src/output.f90 exec/globals.o
	$(f90comp) -c $(switches) src/output.f90 -J./exec -I./exec -o exec/output.o
	
exec/wage_distribution.o : src/wage_distribution.f90 exec/toolbox.o
	$(f90comp) -c $(switches) src/wage_distribution.f90 -J./exec -I./exec -o exec/wage_distribution.o
	
exec/model.o : src/model.f90 exec/globals.o exec/toolbox.o exec/wage_distribution.o
	$(f90comp) -c $(switches) src/model.f90 -J./exec -I./exec -o exec/model.o	
	
exec/simulated_annealing.o : src/simulated_annealing.f90 exec/output.o exec/toolbox.o exec/model.o
	$(f90comp) -c $(switches) src/simulated_annealing.f90 -J./exec -I./exec -o exec/simulated_annealing.o	
	
exec/policy_analysis.o : src/policy_analysis.f90 exec/globals.o exec/toolbox.o exec/model.o
	$(f90comp) -c $(switches) src/policy_analysis.f90 -J./exec -I./exec -o exec/policy_analysis.o	
	
exec/estimation.o : src/estimation.f90 exec/globals.o exec/output.o exec/model.o exec/simulated_annealing.o
	$(f90comp) -c $(switches) src/estimation.f90 -J./exec -I./exec -o exec/estimation.o
	
exec/fertdec.o : src/fertdec.f90 exec/globals.o exec/toolbox.o exec/output.o exec/wage_distribution.o exec/model.o exec/simulated_annealing.o exec/estimation.o exec/policy_analysis.o
	$(f90comp) -c $(switches) src/fertdec.f90 -J./exec -I./exec -o exec/fertdec.o	
	
clean :
	rm exec/*
	rm output/*

	
run : exec/$(executable)
	exec/$(executable)
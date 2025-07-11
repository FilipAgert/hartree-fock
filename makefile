# Paths
DSRC = src
DOBJ = build
DEXE = app
DTEST = tests
DMOD = mod
DLIB = /lib/x86_64-linux-gnu
EXEN = main.exe
TEST_EXE = test.exe

# Flags
LIBS = -llapack -lblas -fopenmp
FLAGS = -O3 -I$(DOBJ) -I$(DMOD) -ffree-line-length-none -fcheck=all -fbacktrace -g -fimplicit-none -fno-omit-frame-pointer
CC = gfortran $(FLAGS) -J$(DMOD) $(LIBS) -L$(DLIB) -c
CCL = gfortran -o

# Objects
OBJECTS = $(DOBJ)/constants.o $(DOBJ)/ho.o $(DOBJ)/integrate.o $(DOBJ)/quadrule.o  $(DOBJ)/hf.o  $(DOBJ)/geom.o $(DOBJ)/pot.o $(DOBJ)/sort.o
TEST_OBJECTS = $(DOBJ)/test_geom.o $(DOBJ)/test_utils.o $(DOBJ)/test_sort.o
MAIN_OBJ = $(DOBJ)/main.o
TEST_OBJ = $(DOBJ)/run_all_tests.o 

VPATH = $(DSRC):$(DTEST):$(DSRC)/$(DSH)

# Dependencies (force recompilation when modules change)
$(DOBJ)/main.o: $(DMOD)/constants.mod $(DMOD)/ho.mod $(DMOD)/hf.mod $(DMOD)/pot.mod
$(DOBJ)/ho.o: $(DMOD)/constants.mod $(DMOD)/geom.mod $(DMOD)/integrate.mod
$(DOBJ)/hf.o: $(DMOD)/constants.mod
$(DOBJ)/geom.o: $(DMOD)/constants.mod
$(DOBJ)/sort.o: $(DMOD)/constants.mod
$(DOBJ)/integrate.o: $(DMOD)/constants.mod $(DMOD)/quadrule.mod
$(DOBJ)/pot.o: $(DMOD)/constants.mod $(DMOD)/ho.mod $(DMOD)/geom.mod $(DMOD)/integrate.mod $(DMOD)/sort.mod
$(DOBJ)/test_utils.o: $(DMOD)/constants.mod
$(DOBJ)/test_geom.o: $(DMOD)/geom.mod $(DMOD)/test_utils.mod
$(DOBJ)/test_sort.o: $(DMOD)/sort.mod $(DMOD)/test_utils.mod
$(DOBJ)/run_all_tests.o: $(DMOD)/test_geom.mod $(DMOD)/test_sort.mod




# Default target
all: main

$(DOBJ)/%.o: %.f90 | $(DOBJ) $(DMOD)
	$(CC) $< -o $@



# Ensure required directories exist
$(DOBJ) $(DEXE) $(DMOD) $(DTEST):
	mkdir -p $@

# Targets
$(DEXE)/$(EXEN): $(MAIN_OBJ) $(OBJECTS) | $(DEXE)
	$(CCL) $@ $(MAIN_OBJ) $(OBJECTS) $(LIBS)

$(DEXE)/$(TEST_EXE): $(TEST_OBJ) $(OBJECTS) $(TEST_OBJECTS) | $(DEXE)
	$(CCL) $@ $(TEST_OBJ) $(OBJECTS) $(TEST_OBJECTS) $(LIBS)

main: $(DEXE)/$(EXEN)

run: $(DEXE)/$(EXEN)
	$(DEXE)/$(EXEN)

test: $(DEXE)/$(TEST_EXE)
	$(DEXE)/$(TEST_EXE)

clean:
	rm -rf $(DOBJ)/*.o $(DEXE)/*.exe $(DMOD)/*.mod

.PHONY: clean run fit runfit main all

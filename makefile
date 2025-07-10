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
FLAGS = -O3 -I$(DOBJ) -ffree-line-length-none -fcheck=all -fbacktrace -g -fimplicit-none -fno-omit-frame-pointer
CC = gfortran $(FLAGS) -J$(DMOD) $(LIBS) -L$(DLIB) -c
CCL = gfortran -o


# Objects
OBJECTS = $(DOBJ)/constants.o $(DOBJ)/ho.o $(DOBJ)/integrate.o $(DOBJ)/quadrule.o  $(DOBJ)/hf.o  $(DOBJ)/geom.o $(DOBJ)/pot.o
TEST_OBJECTS = $(DOBJ)/test_geom.o $(DOBJ)/test_utils.o
MAIN_OBJ = $(DOBJ)/main.o
TEST_OBJ = $(DOBJ)/run_all_tests.o
VPATH = $(DSRC):$(DTEST):$(DSRC)/$(DSH)

$(DMOD)/constants.mod: $(DSRC)/constants.f90
$(DMOD)/geom.mod: $(DSRC)/geom.f90
$(DMOD)/ho.mod: $(DSRC)/ho.f90
$(DMOD)/hf.mod: $(DSRC)/hf.f90
$(DMOD)/integrate.mod: $(DSRC)/integrate.f90
$(DMOD)/quadrule.mod: $(DSRC)/quadrule.f90
$(DMOD)/pot.mod: $(DSRC)/pot.f90

$(DSRC)/main.f90: $(DMOD)/constants.mod $(DMOD)/ho.mod $(DMOD)/hf.mod 
$(DTEST)/run_all_tests.f90:$(DMOD)/test_geom.mod 
$(DSRC)/ho.f90: $(DMOD)/constants.mod $(DMOD)/geom.mod
$(DSRC)/hf.f90: $(DMOD)/constants.mod
$(DSRC)/geom.f90: $(DMOD)/constants.mod
$(DSRC)/integrate.f90: $(DMOD)/quadrule.mod $(DMOD)/constants.mod
$(DSRC)/pot.f90: $(DMOD)/ho.mod $(DMOD)/constants.mod $(DMOD)/geom.mod $(DMOD)/integrate.mod


$(DTEST)/test_geom.f90: $(DMOD)/geom.mod $(DMOD)/test_utils.mod
$(DTEST)/test_utils.f90: $(DMOD)/constants.mod 
# Default target
all: main fit

# Ensure required directories exist
$(DOBJ) $(DEXE) $(DMOD) $(DTEST):
	mkdir -p $@

# Build rules
$(DOBJ)/%.o: %.f90 | $(DOBJ) $(DMOD)
	$(CC) $< -o $@


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

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
OBJECTS = $(DOBJ)/constants.o
TEST_OBJECTS = $(DOBJ)/
MAIN_OBJ = $(DOBJ)/main.o
VPATH = $(DSRC):$(DTEST):$(DSRC)/$(DSH)

# Default target
all: main fit

$(DOBJ)/main.o: $(DSRC)/main.f90 $(DOBJ)/constants.o 
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

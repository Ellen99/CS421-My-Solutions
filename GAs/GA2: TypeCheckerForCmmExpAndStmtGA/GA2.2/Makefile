MPVERSION=cmm_typed_stmt_of_untyped_stmt
STUDENTSRC=$(MPVERSION).ml
MODULE_COMMON=common
PLSOLUTION=plsolution

OCAMLC=ocamlc
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc
GMAKE=make
RM=rm
CP=cp
LN=ln
MV=mv
TAR=tar
GZIP=gzip
MKDIR=mkdir
LATEX=pdflatex

GRADER_NAME=grader

LIBRARY_GRADER=lib/grader.cma
MODULE_STUDENT=student
MODULE_SOLUTION=solution
MODULE_RUBRIC=rubric

OBJLANG=cmm

#######################################################################
# DISTFILES define what goes into mpNtest.tgz distributions
#######################################################################

all: $(GRADER_NAME)

skeleton-$(MPVERSION).ml: $(STUDENTSRC)
	$(CP) $(STUDENTSRC) skeleton-$(MPVERSION).ml

$(MPVERSION).pdf: ../$(MPVERSION).tex
	(cd ..; $(LATEX) $(MPVERSION).tex; $(LATEX) $(MPVERSION).tex)
	$(CP) ../$(MPVERSION).pdf .


########################################################################
# basic case, assumes no common.ml or common.cmo exists
########################################################################

DISTFILES_SOURCE=pre-rubric.c tests Makefile $(STUDENTSRC)
DISTFILES_OBJECT=$(MODULE_SOLUTION).cmo $(MODULE_SOLUTION).cmi $(PLSOLUTION).cmo $(PLSOLUTION).cmi

IMPLEMENTATIONS=$(MODULE_STUDENT).cmo $(MODULE_SOLUTION).cmo 


########################################################################
# if common.ml exists, add it
########################################################################
ifeq "$(wildcard $(MODULE_COMMON).ml)" "$(MODULE_COMMON).ml" 
DISTFILES_SOURCE=pre-rubric.c tests Makefile $(STUDENTSRC) #$(MODULE_COMMON).ml
DISTFILES_OBJECT=$(MODULE_COMMON).cmo $(MODULE_COMMON).cmi $(MODULE_SOLUTION).cmo $(MODULE_SOLUTION).cmi $(PLSOLUTION).cmo $(PLSOLUTION).cmi
IMPLEMENTATIONS=$(MODULE_COMMON).cmo $(PLSOLUTION).cmo $(MODULE_STUDENT).cmo $(MODULE_SOLUTION).cmo 
$(MODULE_COMMON).cmo: $(MODULE_COMMON).ml 
	$(OCAMLC) -c $(MODULE_COMMON).ml
endif

########################################################################
# if mlXcommon.cmo exists, add it
########################################################################
ifeq "$(wildcard $(MODULE_COMMON).cmo)" "$(MODULE_COMMON).cmo"
IMPLEMENTATIONS=$(MODULE_COMMON).cmo $(PLSOLUTION).cmo $(MODULE_STUDENT).cmo $(MODULE_SOLUTION).cmo
endif

DISTFILES_OTHER=README skeleton-$(MPVERSION).ml #.ocamlinit
DISTFILES=$(DISTFILES_SOURCE) $(DISTFILES_OBJECT) $(DISTFILES_OTHER)

OBJECTS=$(IMPLEMENTATIONS) $(MODULE_RUBRIC).cmo

STUDENT_CLEAN=$(MODULE_STUDENT).cm? $(MODULE_STUDENT).ml $(MODULE_RUBRIC).cm? util.o $(GRADER_NAME)

$(GRADER_NAME): $(LIBRARY_GRADER) $(OBJECTS)
	$(OCAMLC) -o $(GRADER_NAME) $(LIBRARY_GRADER) $(OBJECTS) 

$(LIBRARY_GRADER):
	$(GMAKE) -C lib
	$(LN) -s lib/util.o .

$(MODULE_STUDENT).cmo: $(STUDENTSRC) 
	$(CP) $(STUDENTSRC) $(MODULE_STUDENT).ml
	$(OCAMLC) -c $(MODULE_STUDENT).ml

########################################################################
# compiling plsolution to give a  separate function for each clause    #
########################################################################

$(PLSOLUTION).cmo:
		$(OCAMLC) -c $(PLSOLUTION).ml

########################################################################
# if solution.ml exists, compile it.  otherwise assume solution.cm{o,i}
# exist.
########################################################################
ifeq "$(wildcard $(MODULE_SOLUTION).ml)" "$(MODULE_SOLUTION).ml"
$(MODULE_SOLUTION).cmo: $(MODULE_SOLUTION).ml
	$(OCAMLC) -c $(MODULE_SOLUTION).ml
endif

$(MODULE_RUBRIC).cmo: pre-$(MODULE_RUBRIC).c tests $(IMPLEMENTATIONS) $(LIBRARY_GRADER)
	gcc -E pre-$(MODULE_RUBRIC).c | grep -E -v "#" > $(MODULE_RUBRIC).ml
	$(OCAMLC) -c -I lib $(MODULE_RUBRIC).ml
	$(RM) -f $(MODULE_RUBRIC).ml

clean:
	$(GMAKE) -C lib clean
	$(RM) -f $(STUDENT_CLEAN)

##########################################################################
#these targets are used by staff
##########################################################################

TESTNAME=$(MPVERSION)

dist: $(GRADER_NAME) $(DISTFILES)
	$(RM) -rf $(TESTNAME)
	$(MKDIR) $(TESTNAME)
	$(MKDIR) $(TESTNAME)/lib
	$(CP) lib/Makefile lib/*.ml $(TESTNAME)/lib
	$(CP) $(DISTFILES) $(TESTNAME)
	$(TAR) cpf $(TESTNAME).tar $(TESTNAME)
	$(RM) -rf $(TESTNAME)
	$(GZIP) -9 $(TESTNAME).tar

#if you are a student, do not make dist-clean.  it will delete
#your copy of solution.cmo and you will need to download a new
#copy.
dist-clean: clean
	$(RM) -f $(DISTFILES_OBJECT) $(MODULE_STUDENT).*  $(MODULE_COMMON).cm? $(MODULE_RUBRIC).ml 


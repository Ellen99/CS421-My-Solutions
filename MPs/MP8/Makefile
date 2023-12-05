MPVERSION=token
STUDENTSRC=$(MPVERSION).mll
MODULE_COMMON=common

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

#######################################################################
# DISTFILES define what goes into mpNtest.tgz distributions
#######################################################################

all: $(GRADER_NAME) 

$(MPVERSION)-skeleton.ml: $(STUDENTSRC)
	$(CP) $(STUDENTSRC) $(MPVERSION)-skeleton.ml

$(MPVERSION).pdf: ../$(MPVERSION).tex
	(cd ..; $(LATEX) $(MPVERSION).tex; $(LATEX) $(MPVERSION).tex)
	$(CP) ../$(MPVERSION).pdf .

DISTFILES_SOURCE=pre-rubric.c tests Makefile $(STUDENTSRC) $(MPVERSION)-skeleton.mll
DISTFILES_OBJECT=$(MODULE_SOLUTION).cmo $(MODULE_SOLUTION).cmi

IMPLEMENTATIONS=$(MODULE_STUDENT).cmo $(MODULE_SOLUTION).cmo


########################################################################
# if mpXcommon.ml exists, add it
########################################################################
ifeq "$(wildcard $(MODULE_COMMON).ml)" "$(MODULE_COMMON).ml" 
DISTFILES_SOURCE=pre-rubric.c tests Makefile $(STUDENTSRC) $(MPVERSION)-skeleton.mll $(MODULE_COMMON).ml 
DISTFILES_OBJECT=$(MODULE_SOLUTION).cmo $(MODULE_SOLUTION).cmi $(MODULE_COMMON).cmo $(MODULE_COMMON).cmi
IMPLEMENTATIONS=$(MODULE_COMMON).cmo $(MODULE_STUDENT).cmo $(MODULE_SOLUTION).cmo
$(MODULE_COMMON).cmo: $(MODULE_COMMON).ml 
	$(OCAMLC) -c $(MODULE_COMMON).ml
endif

DISTFILES_OTHER=README $(MPVERSION).pdf
DISTFILES=$(DISTFILES_SOURCE) $(DISTFILES_OBJECT) $(DISTFILES_OTHER)

OBJECTS=$(IMPLEMENTATIONS) $(MODULE_RUBRIC).cmo

STUDENT_CLEAN=$(MODULE_STUDENT).cm? $(MODULE_RUBRIC).cm? util.o \
		$(GRADER_NAME)

$(GRADER_NAME): $(LIBRARY_GRADER) $(OBJECTS)
	$(OCAMLC) -o $(GRADER_NAME) $(LIBRARY_GRADER) $(OBJECTS) 

$(LIBRARY_GRADER):
	$(GMAKE) -C lib
#	$(LN) -s lib/util.o .

$(MODULE_STUDENT).cmo: $(MODULE_COMMON).cmo $(STUDENTSRC)
	$(CP) $(STUDENTSRC) $(MODULE_STUDENT).mll
	$(OCAMLLEX) $(MODULE_STUDENT).mll
	$(OCAMLC) -c $(MODULE_STUDENT).ml


########################################################################
# if solution.mll exists, compile it.  otherwise assume solution.cm{o,i}
# exist.
########################################################################
ifeq "$(wildcard $(MODULE_SOLUTION).mll)" "$(MODULE_SOLUTION).mll"
$(MODULE_SOLUTION).cmo: $(MODULE_COMMON).cmo $(MODULE_SOLUTION).mll
	$(OCAMLLEX) $(MODULE_SOLUTION).mll 
	$(OCAMLC) -c $(MODULE_SOLUTION).ml
endif

$(MODULE_RUBRIC).cmo: pre-$(MODULE_RUBRIC).c tests $(IMPLEMENTATIONS) $(LIBRARY_GRADER)
	gcc -E pre-$(MODULE_RUBRIC).c | grep -E -v "#" > $(MODULE_RUBRIC).ml
	$(OCAMLC) -c -I lib $(MODULE_RUBRIC).ml
	$(RM) -f $(MODULE_RUBRIC).ml

$(MPVERSION)-skeleton.mll: $(STUDENTSRC)
	$(CP) $(STUDENTSRC) $(MPVERSION)-skeleton.mll
clean:
	$(GMAKE) -C lib clean
	$(RM) -f $(STUDENT_CLEAN)

##########################################################################
#these targets are used by staff
##########################################################################

TESTNAME=$(MPVERSION)

dist: $(DISTFILES)
	$(RM) -rf $(TESTNAME)
	$(MKDIR) $(TESTNAME)
	$(MKDIR) $(TESTNAME)/lib
#	$(CP) lib/Makefile lib/*.ml lib/*.c $(TESTNAME)/lib
	$(CP) lib/Makefile lib/*.ml $(TESTNAME)/lib
	$(CP) $(DISTFILES) $(TESTNAME)
	$(TAR) cpf $(TESTNAME).tar $(TESTNAME)
	$(RM) -rf $(TESTNAME)
	$(GZIP) -9 $(TESTNAME).tar

#if you are a student, do not make dist-clean.  it will delete
#your copy of solution.cmo and you will need to download a new
#copy.
dist-clean: clean
	$(RM) -f $(DISTFILES_OBJECT) $(MODULE_STUDENT).ml $(MODULE_STUDENT).mll $(MODULE_COMMON).cm?


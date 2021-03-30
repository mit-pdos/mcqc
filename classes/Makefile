all: *.v
	coqc MShow.v -Q ${PWD} Mcqc
	coqc MIO.v -Q ${PWD} Mcqc
	coqc MArray.v -Q ${PWD} Mcqc

clean:
	-rm -f *.vo *.glob *.json

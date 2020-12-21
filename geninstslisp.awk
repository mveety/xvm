# geninstslisp.awk -- generates the instructions list

BEGIN {
	tinst = 0
	mcoderev = 0
	printf ";;; autogenerated file! do not edit!\n\n"
	printf "(in-package :xvmasm)\n\n"
	printf "(defparameter *inst-list*\n"
	printf "  '("
}

$1 == "revision" {
	mcoderev = $2
}

$1 != "#" && $0 != "" && $1 != "revision" {
	if(tinst != 0){
		printf "\n    "
	}
	if($4 == "1"){
		MWIDTH = "t"
	} else {
		MWIDTH = "nil"
	}
	if($5 == "1"){
		MMEM = "t"
	} else {
		MMEM = "nil"
	}
	if($6 == "1"){
		REV = "t"
	} else {
		REV = "nil"
	}
	OPCODE = $2
	sub("0x", "#16r", OPCODE)
	NAME = toupper($1)
	printf "(:NAME %s :STRING \"%s\" :OPCODE %s :LENGTH %s :MODWIDTH %s :MODMEM %s :REVERSE %s :REGARGS %s :CONSTARG %s)",
		   NAME, NAME, OPCODE, ($3 + 2), MWIDTH, MMEM, REV, $7, $8
	tinst = tinst + 1
}

END {
	printf "))\n\n"
	printf "(defparameter +microcode-revision+ %s)\n\n", mcoderev
	printf ";;; %s instructions processsed\n", tinst
}


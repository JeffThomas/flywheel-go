package flywheel

type PrecedenceType int

const (
	PRECEDENCE_NONE        PrecedenceType = iota
	PRECEDENCE_ASSIGNMENT  PrecedenceType = iota
	PRECEDENCE_CONDITIONAL PrecedenceType = iota
	PRECEDENCE_SUM         PrecedenceType = iota
	PRECEDENCE_PRODUCT     PrecedenceType = iota
	PRECEDENCE_EXPONENT    PrecedenceType = iota
	PRECEDENCE_PREFIX      PrecedenceType = iota
	PRECEDENCE_POSTFIX     PrecedenceType = iota
	PRECEDENCE_CALL        PrecedenceType = iota
)

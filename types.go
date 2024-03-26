package flywheel

import (
	"github.com/JeffThomas/lexx-go/matchers"
)

type Number float64

type ExecutionContext interface{}

type Instruction interface {
	Execute(ExecutionContext) (Instruction, error)
}

type Location struct {
	Line   int
	Column int
	Script string
}

type CompileContext interface{}

type Compiler interface {
	Compile(CompileContext) (Instruction, error)
	SetNext(Compiler)
}

type PrefixCompilerBuilder func(pe *ParseEnvironment, t *matchers.Token, l *Location, right Compiler) (Compiler, error)
type InfixCompilerBuilder func(pe *ParseEnvironment, t *matchers.Token, l *Location, left Compiler, right Compiler) (Compiler, error)

type PrefixParsletParse func(*ParseEnvironment, PrecedenceType) (Compiler, error)
type PrefixParsletMatch func(*matchers.Token, PrecedenceType) PrefixParsletParse
type PrefixParsletBuild func([]string, matchers.TokenType, PrecedenceType, PrefixCompilerBuilder) PrefixParsletMatch

type InfixParsletParse func(*ParseEnvironment, PrecedenceType, Compiler) (Compiler, error)
type InfixParsletMatch func(*matchers.Token, PrecedenceType) InfixParsletParse
type InfixParsletBuild func([]string, matchers.TokenType, PrecedenceType, InfixCompilerBuilder) InfixParsletMatch

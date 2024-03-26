package tests

import (
	"flywheel"
	"github.com/JeffThomas/lexx-go/lexx"
	"github.com/JeffThomas/lexx-go/matchers"
	"testing"
)

type CompilerSimple struct {
	Next     flywheel.Compiler
	Left     flywheel.Compiler
	Right    flywheel.Compiler
	Token    *matchers.Token
	Location *flywheel.Location
}

func (ct *CompilerSimple) Compile(ce flywheel.CompileContext) (flywheel.Instruction, error) {
	var in flywheel.Instruction = nil
	if ct.Left != nil {
		in, _ = ct.Left.Compile(ce)
	}
	if ct.Right != nil {
		r, _ := ct.Right.Compile(ce)
		in = addToSimpleLinkedList(in.(*Instruction), r.(*Instruction))
	}
	me := Instruction{
		InstructionNext: nil,
		Token:           ct.Token,
	}
	if in == nil {
		return &me, nil
	}
	in = addToSimpleLinkedList(in.(*Instruction), &me)
	return in, nil
}

func (ct *CompilerSimple) SetNext(next flywheel.Compiler) {}

func BuildSimplePrefixCompilerTest(pe *flywheel.ParseEnvironment, t *matchers.Token, l *flywheel.Location, right flywheel.Compiler) (flywheel.Compiler, error) {
	return &CompilerSimple{
		Next:     nil,
		Token:    t,
		Location: l,
		Right:    right,
	}, nil
}

func BuildSimpleInfixCompilerSimpleTest(pe *flywheel.ParseEnvironment, t *matchers.Token, l *flywheel.Location, left flywheel.Compiler, right flywheel.Compiler) (flywheel.Compiler, error) {
	return &CompilerSimple{
		Next:     nil,
		Token:    t,
		Location: l,
		Left:     left,
		Right:    right,
	}, nil
}

func addToSimpleLinkedList(root *Instruction, newItem *Instruction) *Instruction {
	if root == nil {
		return newItem
	}
	next := root
	for next.InstructionNext != nil {
		next = next.InstructionNext.(*Instruction)
	}
	next.InstructionNext = newItem
	return root
}

type ExecutionContextSimple struct {
	out string
}

type Instruction struct {
	InstructionNext flywheel.Instruction
	Token           *matchers.Token
}

func (it *Instruction) Execute(ctxi flywheel.ExecutionContext) (flywheel.Instruction, error) {
	ctx, _ := ctxi.(*ExecutionContextSimple)
	ctx.out += it.Token.Value
	if it.InstructionNext != nil {
		ctx.out += ","
	}
	return it.InstructionNext, nil
}

type CompileSimpleContext struct{}

func TestSimpleParser_Parsing(t *testing.T) {
	l := lexx.BuildLexxWithString("2", []matchers.LexxMatcherInitialize{
		matchers.StartIntegerMatcher,
	})
	parser := flywheel.NewParseEnvironment(l)

	parser.AddPrefixParser(flywheel.PrefixParsletBuilder(matchers.INTEGER, flywheel.PRECEDENCE_NONE, BuildSimplePrefixCompilerTest))

	resultc, err := flywheel.ParseElement(parser, flywheel.PRECEDENCE_NONE)
	if err != nil {
		t.Error(err)
	}

	resulti, err := resultc.Compile(&CompileSimpleContext{})
	if err != nil {
		t.Error(err)
	}

	ctx := &ExecutionContextSimple{
		out: "",
	}

	for resulti != nil {
		resulti, err = resulti.Execute(ctx)
	}

	if ctx.out != "2" {
		t.Errorf("Did not get expected results, expected 2 got %s\n", ctx.out)
	}
}

func TestSimpleParser_InfixParsing(t *testing.T) {
	l := lexx.BuildLexxWithString("2 + 1", []matchers.LexxMatcherInitialize{
		matchers.StartIntegerMatcher,
		matchers.StartWhitespaceMatcher,
		matchers.ConfigOperatorMatcher([]string{"+"}),
	})
	parser := flywheel.NewParseEnvironment(l)

	parser.AddPrefixParser(flywheel.PrefixParsletBuilder(matchers.INTEGER, flywheel.PRECEDENCE_NONE, BuildSimplePrefixCompilerTest))
	parser.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"+"}, matchers.OPERATOR, flywheel.PRECEDENCE_SUM, BuildSimpleInfixCompilerSimpleTest))

	compilers, err := flywheel.ParseElement(parser, flywheel.PRECEDENCE_NONE)
	if err != nil {
		t.Error(err)
	}

	instructions, err := compilers.Compile(&CompileSimpleContext{})
	if err != nil {
		t.Error(err)
	}

	ctx := &ExecutionContextSimple{
		out: "",
	}

	for instructions != nil {
		instructions, err = instructions.Execute(ctx)
	}

	if ctx.out != "2,1,+" {
		t.Errorf("Did not get expected results from, expected \"2,1,+\" got %s\n", ctx.out)
	}
}

func TestSimpleParser_PrecedenceParsing(t *testing.T) {
	l := lexx.BuildLexxWithString("2 * 1 + 5", []matchers.LexxMatcherInitialize{
		matchers.StartIntegerMatcher,
		matchers.StartWhitespaceMatcher,
		matchers.ConfigOperatorMatcher([]string{"+", "*"}),
	})
	parser := flywheel.NewParseEnvironment(l)

	parser.AddPrefixParser(flywheel.PrefixParsletBuilder(matchers.INTEGER, flywheel.PRECEDENCE_NONE, BuildSimplePrefixCompilerTest))
	parser.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"+"}, matchers.OPERATOR, flywheel.PRECEDENCE_SUM, BuildSimpleInfixCompilerSimpleTest))
	parser.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"*"}, matchers.OPERATOR, flywheel.PRECEDENCE_PRODUCT, BuildSimpleInfixCompilerSimpleTest))

	resultc, err := flywheel.ParseElement(parser, flywheel.PRECEDENCE_NONE)
	if err != nil {
		t.Error(err)
	}

	resulti, err := resultc.Compile(&CompileSimpleContext{})
	if err != nil {
		t.Error(err)
	}

	ctx := &ExecutionContextSimple{
		out: "",
	}

	for resulti != nil {
		resulti, err = resulti.Execute(ctx)
	}

	if ctx.out != "2,1,*,5,+" {
		t.Errorf("Did not get expected results from, expected \"2,1,*,5,+\" got %s\n", ctx.out)
	}
}

func TestSimpleParser_PrecedenceParsing2(t *testing.T) {
	l := lexx.BuildLexxWithString("2 + 3 * 5", []matchers.LexxMatcherInitialize{
		matchers.StartIntegerMatcher,
		matchers.StartWhitespaceMatcher,
		matchers.ConfigOperatorMatcher([]string{"+", "*"}),
	})
	parser := flywheel.NewParseEnvironment(l)

	parser.AddPrefixParser(flywheel.PrefixParsletBuilder(matchers.INTEGER, flywheel.PRECEDENCE_NONE, BuildSimplePrefixCompilerTest))
	parser.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"+"}, matchers.OPERATOR, flywheel.PRECEDENCE_SUM, BuildSimpleInfixCompilerSimpleTest))
	parser.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"*"}, matchers.OPERATOR, flywheel.PRECEDENCE_PRODUCT, BuildSimpleInfixCompilerSimpleTest))

	resultc, err := flywheel.ParseElement(parser, flywheel.PRECEDENCE_NONE)
	if err != nil {
		t.Error(err)
	}

	resulti, err := resultc.Compile(&CompileSimpleContext{})
	if err != nil {
		t.Error(err)
	}

	ctx := &ExecutionContextSimple{
		out: "",
	}

	for resulti != nil {
		resulti, err = resulti.Execute(ctx)
	}

	if ctx.out != "2,3,5,*,+" {
		t.Errorf("Did not get expected results, expected \"2,3,5,*,+\" got %s\n", ctx.out)
	}
}

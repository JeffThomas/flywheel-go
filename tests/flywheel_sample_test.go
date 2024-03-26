package tests

import (
	"flywheel"
	"github.com/JeffThomas/lexx-go/lexx"
	"github.com/JeffThomas/lexx-go/matchers"
	"testing"
)

type CompilerSample struct {
	Next     flywheel.Compiler
	Left     flywheel.Compiler
	Right    flywheel.Compiler
	Token    *matchers.Token
	Location *flywheel.Location
}

func (ct *CompilerSample) Compile(ce flywheel.CompileContext) (flywheel.Instruction, error) {
	var in flywheel.Instruction = nil
	if ct.Left != nil {
		in, _ = ct.Left.Compile(ce)
	}
	if ct.Right != nil {
		r, _ := ct.Right.Compile(ce)
		if in == nil {
			in = r
		} else {
			in.(SampleInstructionInterface).SetNext(r.(SampleInstructionInterface))
		}
	}
	me := &SampleInstruction{
		InstructionNext: nil,
		Token:           ct.Token,
	}
	if in == nil {
		return me, nil
	}
	in.(SampleInstructionInterface).SetNext(me)
	return in, nil
}

func (ct *CompilerSample) SetNext(next flywheel.Compiler) {}

func BuildSamplePrefixCompilerTest(pe *flywheel.ParseEnvironment, t *matchers.Token, l *flywheel.Location, right flywheel.Compiler) (flywheel.Compiler, error) {
	return &CompilerSample{
		Next:     nil,
		Token:    t,
		Location: l,
		Right:    right,
	}, nil
}

func BuildSampleInfixCompilerTest(pe *flywheel.ParseEnvironment, t *matchers.Token, l *flywheel.Location, left flywheel.Compiler, right flywheel.Compiler) (flywheel.Compiler, error) {
	return &CompilerSample{
		Next:     nil,
		Token:    t,
		Location: l,
		Left:     left,
		Right:    right,
	}, nil
}

type SampleExecutionContext struct {
	out string
}

type SampleInstructionInterface interface {
	Execute(flywheel.ExecutionContext) (flywheel.Instruction, error)
	SetNext(SampleInstructionInterface)
}

type SampleInstruction struct {
	InstructionNext SampleInstructionInterface
	Token           *matchers.Token
}

func (it *SampleInstruction) Execute(ctxi flywheel.ExecutionContext) (flywheel.Instruction, error) {
	ctx, _ := ctxi.(*SampleExecutionContext)
	ctx.out += it.Token.Value
	if it.InstructionNext != nil {
		ctx.out += ","
	}
	return it.InstructionNext, nil
}

func (it *SampleInstruction) SetNext(newItem SampleInstructionInterface) {
	next := it
	for next.InstructionNext != nil {
		next = next.InstructionNext.(*SampleInstruction)
	}
	next.InstructionNext = newItem
}

type CompileContext struct{}

func TestSampleParser_Parsing(t *testing.T) {

	lex := lexx.NewLexx([]matchers.LexxMatcherInitialize{
		matchers.StartIntegerMatcher,
	})
	lex.SetStringInput("2")

	pe := flywheel.NewParseEnvironment(lex)

	pe.AddPrefixParser(flywheel.PrefixParsletBuilder(matchers.INTEGER, flywheel.PRECEDENCE_NONE, BuildSamplePrefixCompilerTest))

	resultc, err := flywheel.ParseElement(pe, flywheel.PRECEDENCE_NONE)
	if err != nil {
		t.Error(err)
	}

	resulti, err := resultc.Compile(&CompileContext{})
	if err != nil {
		t.Error(err)
	}

	ctx := &SampleExecutionContext{
		out: "",
	}

	for resulti != nil {
		resulti, err = resulti.Execute(ctx)
	}

	if ctx.out != "2" {
		t.Errorf("Did not get expected results, expected 2 got %s\n", ctx.out)
	}
}

func TestSampleParser_InfixParsing(t *testing.T) {

	lex := lexx.NewLexx([]matchers.LexxMatcherInitialize{
		matchers.StartIntegerMatcher,
		matchers.StartWhitespaceMatcher,
		matchers.ConfigOperatorMatcher([]string{"+"}),
	})
	lex.SetStringInput("2 + 1")

	pe := flywheel.NewParseEnvironment(lex)

	pe.AddPrefixParser(flywheel.PrefixParsletBuilder(matchers.INTEGER, flywheel.PRECEDENCE_NONE, BuildSamplePrefixCompilerTest))
	pe.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"+"}, matchers.OPERATOR, flywheel.PRECEDENCE_SUM, BuildSampleInfixCompilerTest))

	compilers, err := flywheel.ParseElement(pe, flywheel.PRECEDENCE_NONE)
	if err != nil {
		t.Error(err)
	}

	instructions, err := compilers.Compile(&CompileContext{})
	if err != nil {
		t.Error(err)
	}

	ctx := &SampleExecutionContext{
		out: "",
	}

	for instructions != nil {
		instructions, err = instructions.Execute(ctx)
	}

	if ctx.out != "2,1,+" {
		t.Errorf("Did not get expected results from, expected \"2,1,+\" got %s\n", ctx.out)
	}
}

func TestSampleParser_PrecedenceParsing(t *testing.T) {
	lex := lexx.NewLexx([]matchers.LexxMatcherInitialize{
		matchers.StartIntegerMatcher,
		matchers.StartWhitespaceMatcher,
		matchers.ConfigOperatorMatcher([]string{"+", "*"}),
	})
	lex.SetStringInput("2 * 1 + 5")

	pe := flywheel.NewParseEnvironment(lex)

	pe.AddPrefixParser(flywheel.PrefixParsletBuilder(matchers.INTEGER, flywheel.PRECEDENCE_NONE, BuildSamplePrefixCompilerTest))
	pe.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"+"}, matchers.OPERATOR, flywheel.PRECEDENCE_SUM, BuildSampleInfixCompilerTest))
	pe.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"*"}, matchers.OPERATOR, flywheel.PRECEDENCE_PRODUCT, BuildSampleInfixCompilerTest))

	resultc, err := flywheel.ParseElement(pe, flywheel.PRECEDENCE_NONE)
	if err != nil {
		t.Error(err)
	}

	resulti, err := resultc.Compile(&CompileContext{})
	if err != nil {
		t.Error(err)
	}

	ctx := &SampleExecutionContext{
		out: "",
	}

	for resulti != nil {
		resulti, err = resulti.Execute(ctx)
	}

	if ctx.out != "2,1,*,5,+" {
		t.Errorf("Did not get expected results from, expected \"2,1,*,5,+\" got %s\n", ctx.out)
	}
}

func TestSampleParser_PrecedenceParsing2(t *testing.T) {
	lex := lexx.NewLexx([]matchers.LexxMatcherInitialize{
		matchers.StartIntegerMatcher,
		matchers.StartWhitespaceMatcher,
		matchers.ConfigOperatorMatcher([]string{"+", "*"}),
	})
	lex.SetStringInput("2 + 3 * 5")

	pe := flywheel.NewParseEnvironment(lex)

	pe.AddPrefixParser(flywheel.PrefixParsletBuilder(matchers.INTEGER, flywheel.PRECEDENCE_NONE, BuildSamplePrefixCompilerTest))
	pe.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"+"}, matchers.OPERATOR, flywheel.PRECEDENCE_SUM, BuildSampleInfixCompilerTest))
	pe.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"*"}, matchers.OPERATOR, flywheel.PRECEDENCE_PRODUCT, BuildSampleInfixCompilerTest))

	resultc, err := flywheel.ParseElement(pe, flywheel.PRECEDENCE_NONE)
	if err != nil {
		t.Error(err)
	}

	resulti, err := resultc.Compile(&CompileContext{})
	if err != nil {
		t.Error(err)
	}

	ctx := &SampleExecutionContext{
		out: "",
	}

	for resulti != nil {
		resulti, err = resulti.Execute(ctx)
	}

	if ctx.out != "2,3,5,*,+" {
		t.Errorf("Did not get expected results, expected \"2,3,5,*,+\" got %s\n", ctx.out)
	}
}

func TestParser_SampleSubSubPrecedenceParsing(t *testing.T) {
	lex := lexx.NewLexx([]matchers.LexxMatcherInitialize{
		matchers.StartIntegerMatcher,
		matchers.StartWhitespaceMatcher,
		matchers.ConfigOperatorMatcher([]string{"(", ")", "+", "*", "-"}),
	})
	lex.SetStringInput("1 + 4*((2-3) * 2 + 6) - 3")

	pe := flywheel.NewParseEnvironment(lex)

	pe.AddPrefixParser(flywheel.PrefixParsletBuilder(matchers.INTEGER, flywheel.PRECEDENCE_NONE, BuildSamplePrefixCompilerTest))
	pe.AddPrefixParser(flywheel.PrefixParsletSubBuilder(&matchers.Token{Type: matchers.OPERATOR, Value: "("}, &matchers.Token{Type: matchers.OPERATOR, Value: ")"}, BuildSamplePrefixCompilerTest))
	pe.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"+", "-"}, matchers.OPERATOR, flywheel.PRECEDENCE_SUM, BuildSampleInfixCompilerTest))
	pe.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"*"}, matchers.OPERATOR, flywheel.PRECEDENCE_PRODUCT, BuildSampleInfixCompilerTest))

	resultc, err := flywheel.ParseElement(pe, flywheel.PRECEDENCE_NONE)
	if err != nil {
		t.Error(err)
	}

	resulti, err := resultc.Compile(&CompileContext{})
	if err != nil {
		t.Error(err)
	}

	ctx := &SampleExecutionContext{
		out: "",
	}

	for resulti != nil {
		resulti, err = resulti.Execute(ctx)
	}

	if ctx.out != "1,4,2,3,-,(,2,*,6,+,(,*,+,3,-" {
		t.Errorf("Did not get expected results from fly(), expected \"1,4,2,3,-,(,2,*,6,+,(,*,+,3,-\" got %s\n", ctx.out)
	}
}

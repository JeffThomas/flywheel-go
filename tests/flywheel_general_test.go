package tests

import (
	"flywheel"
	"github.com/JeffThomas/lexx-go/lexx"
	"github.com/JeffThomas/lexx-go/matchers"
	"testing"
)

type ContextTest struct {
	out string
}

type CompileEnvironmentTest struct{}

type InstructionTest struct {
	InstructionNext flywheel.Instruction
	Token           *matchers.Token
}

func (it *InstructionTest) Execute(env flywheel.ExecutionContext) (flywheel.Instruction, error) {
	envt, _ := env.(*ContextTest)
	envt.out += it.Token.Value
	if it.InstructionNext != nil {
		envt.out += ","
	}
	return it.InstructionNext, nil
}

func InstructionBuilder(next flywheel.Instruction, t *matchers.Token) flywheel.Instruction {
	return &InstructionTest{
		InstructionNext: next,
		Token:           t,
	}
}

type CompTest struct {
	Next               flywheel.Compiler
	Left               flywheel.Compiler
	Right              flywheel.Compiler
	Token              *matchers.Token
	Location           *flywheel.Location
	InstructionBuilder func(flywheel.Instruction, *matchers.Token) flywheel.Instruction
}

func (ct *CompTest) Compile(cs flywheel.CompileContext) (flywheel.Instruction, error) {
	var in flywheel.Instruction = nil
	if ct.Left != nil {
		in, _ = ct.Left.Compile(cs)
	}
	if ct.Right != nil {
		r, _ := ct.Right.Compile(cs)
		if in == nil {
			in = r
		} else {
			i, _ := in.(*InstructionTest)
			for i.InstructionNext != nil {
				i = i.InstructionNext.(*InstructionTest)
			}
			i.InstructionNext = r
		}
	}
	me := ct.InstructionBuilder(nil, ct.Token)
	if in == nil {
		return me, nil
	}
	i, _ := in.(*InstructionTest)
	for i.InstructionNext != nil {
		i = i.InstructionNext.(*InstructionTest)
	}
	i.InstructionNext = me
	return in, nil
}

func (ct *CompTest) SetNext(next flywheel.Compiler) {

}

func (ct *CompTest) SetLeft(next flywheel.Compiler) {

}

func (ct *CompTest) SetRight(next flywheel.Compiler) {

}

func InitPrefixCompilerBuilder(instructionBuilder func(flywheel.Instruction, *matchers.Token) flywheel.Instruction) func(*flywheel.ParseEnvironment, *matchers.Token, *flywheel.Location, flywheel.Compiler) (flywheel.Compiler, error) {
	return func(pe *flywheel.ParseEnvironment, t *matchers.Token, l *flywheel.Location, right flywheel.Compiler) (flywheel.Compiler, error) {
		return &CompTest{
			Next:               nil,
			Token:              t,
			Location:           l,
			Right:              right,
			InstructionBuilder: instructionBuilder,
		}, nil
	}
}

func InitInfixCompilerBuilder(instructionBuilder func(flywheel.Instruction, *matchers.Token) flywheel.Instruction) func(*flywheel.ParseEnvironment, *matchers.Token, *flywheel.Location, flywheel.Compiler, flywheel.Compiler) (flywheel.Compiler, error) {
	return func(pe *flywheel.ParseEnvironment, t *matchers.Token, l *flywheel.Location, left flywheel.Compiler, right flywheel.Compiler) (flywheel.Compiler, error) {
		return &CompTest{
			Next:               nil,
			Token:              t,
			Location:           l,
			Left:               left,
			Right:              right,
			InstructionBuilder: instructionBuilder,
		}, nil
	}
}

func makeParser(script string, matchersToAdd []matchers.LexxMatcherInitialize) *flywheel.ParseEnvironment {
	l := lexx.BuildLexxWithString(script, matchersToAdd)
	return flywheel.NewParseEnvironment(l)
}

func fly(t *testing.T, p *flywheel.ParseEnvironment) flywheel.ExecutionContext {
	resultc, err := flywheel.ParseElement(p, flywheel.PRECEDENCE_NONE)
	if err != nil {
		t.Error(err)
	}

	resulti, err := resultc.Compile(&CompileEnvironmentTest{})
	if err != nil {
		t.Error(err)
	}

	ctx := &ContextTest{
		out: "",
	}

	for resulti != nil {
		resulti, err = resulti.Execute(ctx)
	}

	return ctx
}

func TestParser_BasicParsing(t *testing.T) {
	parser := makeParser("2", []matchers.LexxMatcherInitialize{matchers.StartIntegerMatcher})

	parser.AddPrefixParser(flywheel.PrefixParsletBuilder(matchers.INTEGER, flywheel.PRECEDENCE_NONE, InitPrefixCompilerBuilder(InstructionBuilder)))

	ct := fly(t, parser)

	c, _ := ct.(*ContextTest)

	if c.out != "2" {
		t.Errorf("Did not get expected results from fly(), expected 2 got %s\n", c.out)
	}
}

func TestParser_BasicInfixParsing(t *testing.T) {
	parser := makeParser("2 + 1", []matchers.LexxMatcherInitialize{
		matchers.StartIntegerMatcher,
		matchers.StartWhitespaceMatcher,
		matchers.ConfigOperatorMatcher([]string{"+"}),
	})

	parser.AddPrefixParser(flywheel.PrefixParsletBuilder(matchers.INTEGER, flywheel.PRECEDENCE_NONE, InitPrefixCompilerBuilder(InstructionBuilder)))
	parser.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"+"}, matchers.OPERATOR, flywheel.PRECEDENCE_SUM, InitInfixCompilerBuilder(InstructionBuilder)))

	ct := fly(t, parser)

	c, _ := ct.(*ContextTest)

	if c.out != "2,1,+" {
		t.Errorf("Did not get expected results from fly(), expected \"2,1,+\" got %s\n", c.out)
	}
}

func TestParser_BasicPrecedenceParsing(t *testing.T) {
	parser := makeParser("2 * 1 + 5", []matchers.LexxMatcherInitialize{
		matchers.StartIntegerMatcher,
		matchers.StartWhitespaceMatcher,
		matchers.ConfigOperatorMatcher([]string{"+", "*"}),
	})

	parser.AddPrefixParser(flywheel.PrefixParsletBuilder(matchers.INTEGER, flywheel.PRECEDENCE_NONE, InitPrefixCompilerBuilder(InstructionBuilder)))
	parser.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"+"}, matchers.OPERATOR, flywheel.PRECEDENCE_SUM, InitInfixCompilerBuilder(InstructionBuilder)))
	parser.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"*"}, matchers.OPERATOR, flywheel.PRECEDENCE_PRODUCT, InitInfixCompilerBuilder(InstructionBuilder)))

	ct := fly(t, parser)

	c, _ := ct.(*ContextTest)

	if c.out != "2,1,*,5,+" {
		t.Errorf("Did not get expected results from fly(), expected \"2,1,*,5,+\" got %s\n", c.out)
	}
}

func TestParser_BasicPrecedenceParsing2(t *testing.T) {
	parser := makeParser("2 + 1 * 5", []matchers.LexxMatcherInitialize{
		matchers.StartIntegerMatcher,
		matchers.StartWhitespaceMatcher,
		matchers.ConfigOperatorMatcher([]string{"+", "*"}),
	})

	parser.AddPrefixParser(flywheel.PrefixParsletBuilder(matchers.INTEGER, flywheel.PRECEDENCE_NONE, InitPrefixCompilerBuilder(InstructionBuilder)))
	parser.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"+"}, matchers.OPERATOR, flywheel.PRECEDENCE_SUM, InitInfixCompilerBuilder(InstructionBuilder)))
	parser.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"*"}, matchers.OPERATOR, flywheel.PRECEDENCE_PRODUCT, InitInfixCompilerBuilder(InstructionBuilder)))

	ct := fly(t, parser)

	c, _ := ct.(*ContextTest)

	if c.out != "2,1,5,*,+" {
		t.Errorf("Did not get expected results from fly(), expected \"2,1,5,*,+\" got %s\n", c.out)
	}
}

func TestParser_BasicSubPrecedenceParsing(t *testing.T) {
	parser := makeParser("(2 + 1) * 5", []matchers.LexxMatcherInitialize{
		matchers.StartIntegerMatcher,
		matchers.StartWhitespaceMatcher,
		matchers.ConfigOperatorMatcher([]string{"(", ")", "+", "*"}),
	})

	parser.AddPrefixParser(flywheel.PrefixParsletBuilder(matchers.INTEGER, flywheel.PRECEDENCE_NONE, InitPrefixCompilerBuilder(InstructionBuilder)))
	parser.AddPrefixParser(flywheel.PrefixParsletSubBuilder(&matchers.Token{Type: matchers.OPERATOR, Value: "("}, &matchers.Token{Type: matchers.OPERATOR, Value: ")"}, InitPrefixCompilerBuilder(InstructionBuilder)))
	parser.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"+"}, matchers.OPERATOR, flywheel.PRECEDENCE_SUM, InitInfixCompilerBuilder(InstructionBuilder)))
	parser.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"*"}, matchers.OPERATOR, flywheel.PRECEDENCE_PRODUCT, InitInfixCompilerBuilder(InstructionBuilder)))

	ct := fly(t, parser)

	c, _ := ct.(*ContextTest)

	if c.out != "2,1,+,(,5,*" {
		t.Errorf("Did not get expected results from fly(), expected \"2,1,+,(,5,*\" got %s\n", c.out)
	}
}

func TestParser_BasicSubSubPrecedenceParsing(t *testing.T) {
	parser := makeParser("1 + 4*((2-3) * 2 + 6) - 3", []matchers.LexxMatcherInitialize{
		matchers.StartIntegerMatcher,
		matchers.StartWhitespaceMatcher,
		matchers.ConfigOperatorMatcher([]string{"(", ")", "+", "*", "-"}),
	})

	parser.AddPrefixParser(flywheel.PrefixParsletBuilder(matchers.INTEGER, flywheel.PRECEDENCE_NONE, InitPrefixCompilerBuilder(InstructionBuilder)))
	parser.AddPrefixParser(flywheel.PrefixParsletSubBuilder(&matchers.Token{Type: matchers.OPERATOR, Value: "("}, &matchers.Token{Type: matchers.OPERATOR, Value: ")"}, InitPrefixCompilerBuilder(InstructionBuilder)))
	parser.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"+", "-"}, matchers.OPERATOR, flywheel.PRECEDENCE_SUM, InitInfixCompilerBuilder(InstructionBuilder)))
	parser.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"*"}, matchers.OPERATOR, flywheel.PRECEDENCE_PRODUCT, InitInfixCompilerBuilder(InstructionBuilder)))

	ct := fly(t, parser)

	c, _ := ct.(*ContextTest)

	if c.out != "1,4,2,3,-,(,2,*,6,+,(,*,+,3,-" {
		t.Errorf("Did not get expected results from fly(), expected \"1,4,2,3,-,(,2,*,6,+,(,*,+,3,-\" got %s\n", c.out)
	}
}

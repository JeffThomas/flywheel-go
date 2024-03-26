package tests

import (
	"flywheel"
	"github.com/JeffThomas/lexx-go/lexx"
	"github.com/JeffThomas/lexx-go/matchers"
	"testing"
)

type CompileEnvironmentBasic struct {
	out string
}

type CompBasicTest struct {
	Next     flywheel.Compiler
	Left     flywheel.Compiler
	Right    flywheel.Compiler
	Token    *matchers.Token
	Location *flywheel.Location
}

func (ct *CompBasicTest) Compile(ce flywheel.CompileContext) (flywheel.Instruction, error) {
	environmentBasic, _ := ce.(*CompileEnvironmentBasic)
	if ct.Left != nil {
		ct.Left.Compile(ce)
		environmentBasic.out += ","
	}
	if ct.Right != nil {
		ct.Right.Compile(ce)
		environmentBasic.out += ","
	}
	environmentBasic.out += ct.Token.Value
	return nil, nil
}

func (ct *CompBasicTest) SetNext(next flywheel.Compiler) {}

func BuildBasicPrefixCompilerTest(pe *flywheel.ParseEnvironment, t *matchers.Token, l *flywheel.Location, right flywheel.Compiler) (flywheel.Compiler, error) {
	return &CompBasicTest{
		Next:     nil,
		Token:    t,
		Location: l,
		Right:    right,
	}, nil
}

func BuildInfixCompilerTest(pe *flywheel.ParseEnvironment, t *matchers.Token, l *flywheel.Location, left flywheel.Compiler, right flywheel.Compiler) (flywheel.Compiler, error) {
	return &CompBasicTest{
		Next:     nil,
		Token:    t,
		Location: l,
		Left:     left,
		Right:    right,
	}, nil
}

func TestSimpleParser_BasicParsing(t *testing.T) {
	l := lexx.BuildLexxWithString("2", []matchers.LexxMatcherInitialize{
		matchers.StartIntegerMatcher,
	})
	parser := flywheel.NewParseEnvironment(l)

	parser.AddPrefixParser(flywheel.PrefixParsletBuilder(matchers.INTEGER, flywheel.PRECEDENCE_NONE, BuildBasicPrefixCompilerTest))

	resultc, err := flywheel.ParseElement(parser, flywheel.PRECEDENCE_NONE)
	if err != nil {
		t.Error(err)
	}

	ctx := CompileEnvironmentBasic{
		out: "",
	}

	_, err = resultc.Compile(&ctx)
	if err != nil {
		t.Error(err)
	}

	if ctx.out != "2" {
		t.Errorf("Did not get expected results, expected 2 got %s\n", ctx.out)
	}
}

func TestSimpleParser_BasicInfixParsing(t *testing.T) {
	l := lexx.BuildLexxWithString("2 + 1", []matchers.LexxMatcherInitialize{
		matchers.StartIntegerMatcher,
		matchers.StartWhitespaceMatcher,
		matchers.ConfigOperatorMatcher([]string{"+"}),
	})
	parser := flywheel.NewParseEnvironment(l)

	parser.AddPrefixParser(flywheel.PrefixParsletBuilder(matchers.INTEGER, flywheel.PRECEDENCE_NONE, BuildBasicPrefixCompilerTest))
	parser.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"+"}, matchers.OPERATOR, flywheel.PRECEDENCE_SUM, BuildInfixCompilerTest))

	compilers, err := flywheel.ParseElement(parser, flywheel.PRECEDENCE_NONE)
	if err != nil {
		t.Error(err)
	}

	ctx := CompileEnvironmentBasic{
		out: "",
	}

	_, err = compilers.Compile(&ctx)
	if err != nil {
		t.Error(err)
	}

	if ctx.out != "2,1,+" {
		t.Errorf("Did not get expected results from, expected \"2,1,+\" got %s\n", ctx.out)
	}
}

func TestSimpleParser_BasicPrecedenceParsing(t *testing.T) {
	l := lexx.BuildLexxWithString("2 * 1 + 5", []matchers.LexxMatcherInitialize{
		matchers.StartIntegerMatcher,
		matchers.StartWhitespaceMatcher,
		matchers.ConfigOperatorMatcher([]string{"+", "*"}),
	})
	parser := flywheel.NewParseEnvironment(l)

	parser.AddPrefixParser(flywheel.PrefixParsletBuilder(matchers.INTEGER, flywheel.PRECEDENCE_NONE, BuildBasicPrefixCompilerTest))
	parser.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"+"}, matchers.OPERATOR, flywheel.PRECEDENCE_SUM, BuildInfixCompilerTest))
	parser.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"*"}, matchers.OPERATOR, flywheel.PRECEDENCE_PRODUCT, BuildInfixCompilerTest))

	compilers, err := flywheel.ParseElement(parser, flywheel.PRECEDENCE_NONE)
	if err != nil {
		t.Error(err)
	}

	ctx := CompileEnvironmentBasic{
		out: "",
	}

	_, err = compilers.Compile(&ctx)
	if err != nil {
		t.Error(err)
	}

	if ctx.out != "2,1,*,5,+" {
		t.Errorf("Did not get expected results from, expected \"2,1,*,5,+\" got %s\n", ctx.out)
	}
}

func TestSimpleParser_BasicPrecedenceParsing2(t *testing.T) {
	l := lexx.BuildLexxWithString("2 + 3 * 5", []matchers.LexxMatcherInitialize{
		matchers.StartIntegerMatcher,
		matchers.StartWhitespaceMatcher,
		matchers.ConfigOperatorMatcher([]string{"+", "*"}),
	})
	parser := flywheel.NewParseEnvironment(l)

	parser.AddPrefixParser(flywheel.PrefixParsletBuilder(matchers.INTEGER, flywheel.PRECEDENCE_NONE, BuildBasicPrefixCompilerTest))
	parser.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"+"}, matchers.OPERATOR, flywheel.PRECEDENCE_SUM, BuildInfixCompilerTest))
	parser.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"*"}, matchers.OPERATOR, flywheel.PRECEDENCE_PRODUCT, BuildInfixCompilerTest))

	compilers, err := flywheel.ParseElement(parser, flywheel.PRECEDENCE_NONE)
	if err != nil {
		t.Error(err)
	}

	ctx := CompileEnvironmentBasic{
		out: "",
	}

	_, err = compilers.Compile(&ctx)
	if err != nil {
		t.Error(err)
	}

	if ctx.out != "2,3,5,*,+" {
		t.Errorf("Did not get expected results, expected \"2,3,5,*,+\" got %s\n", ctx.out)
	}
}

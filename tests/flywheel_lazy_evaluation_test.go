package tests

import (
	"errors"
	"flywheel"
	"github.com/JeffThomas/lexx-go/lexx"
	"github.com/JeffThomas/lexx-go/matchers"
	"strconv"
	"sync"
	"testing"
)

type LazyExecutionContext struct {
	out   string
	stack funcStack
}

type LazyLinkedInstructionInterface interface {
	Execute(flywheel.ExecutionContext) (flywheel.Instruction, error)
	SetNext(LazyLinkedInstructionInterface)
}

type LazyRPNInstruction struct {
	InstructionNext LazyLinkedInstructionInterface
	Token           *matchers.Token
}

func (it *LazyRPNInstruction) Execute(ctxi flywheel.ExecutionContext) (flywheel.Instruction, error) {
	ctx, _ := ctxi.(*LazyExecutionContext)
	ctx.out += it.Token.Value
	if it.InstructionNext != nil {
		ctx.out += ","
	}
	return it.InstructionNext, nil
}

func (it *LazyRPNInstruction) SetNext(newItem LazyLinkedInstructionInterface) {
	next := it
	for next.InstructionNext != nil {
		next = next.InstructionNext.(*LazyRPNInstruction)
	}
	next.InstructionNext = newItem
}

type LazyNumberInstruction struct {
	InstructionNext LazyLinkedInstructionInterface
	Token           *matchers.Token
}

func (it *LazyNumberInstruction) Execute(ctxi flywheel.ExecutionContext) (flywheel.Instruction, error) {
	ctx, _ := ctxi.(*LazyExecutionContext)
	ctx.stack.Push(func() int {
		num, _ := strconv.Atoi(it.Token.Value)
		return num
	})
	return it.InstructionNext, nil
}

func (it *LazyNumberInstruction) SetNext(newItem LazyLinkedInstructionInterface) {
	if it.InstructionNext == nil {
		it.InstructionNext = newItem
		return
	}
	it.InstructionNext.SetNext(newItem)
}

type LazyOperatorInstruction struct {
	InstructionNext LazyLinkedInstructionInterface
	Token           *matchers.Token
}

func (it *LazyOperatorInstruction) Execute(ctxi flywheel.ExecutionContext) (flywheel.Instruction, error) {
	ctx, _ := ctxi.(*LazyExecutionContext)
	second, _ := ctx.stack.Pop()
	first, _ := ctx.stack.Pop()
	switch it.Token.Value {
	case "+":
		ctx.stack.Push(func() int {
			return first() + second()
		})
	case "-":
		ctx.stack.Push(func() int {
			return first() - second()
		})
	case "*":
		ctx.stack.Push(func() int {
			return first() * second()
		})
	case "/":
		ctx.stack.Push(func() int {
			return first() / second()
		})
	}
	return it.InstructionNext, nil
}

func (it *LazyOperatorInstruction) SetNext(newItem LazyLinkedInstructionInterface) {
	if it.InstructionNext == nil {
		it.InstructionNext = newItem
		return
	}
	it.InstructionNext.SetNext(newItem)
}

type LazyCompiler struct {
	Next     flywheel.Compiler
	Left     flywheel.Compiler
	Right    flywheel.Compiler
	Token    *matchers.Token
	Location *flywheel.Location
}

func (ct *LazyCompiler) Compile(ce flywheel.CompileContext) (flywheel.Instruction, error) {
	var in flywheel.Instruction = nil
	if ct.Left != nil {
		in, _ = ct.Left.Compile(ce)
	}
	if ct.Right != nil {
		r, _ := ct.Right.Compile(ce)
		if in == nil {
			in = r // prefix handling
		} else {
			in.(LazyLinkedInstructionInterface).SetNext(r.(LazyLinkedInstructionInterface))
		}
	}
	var me LazyLinkedInstructionInterface = nil
	switch ct.Token.Type {
	case matchers.INTEGER:
		me = &LazyNumberInstruction{
			InstructionNext: nil,
			Token:           ct.Token,
		}
	case matchers.OPERATOR:
		me = &LazyOperatorInstruction{
			InstructionNext: nil,
			Token:           ct.Token,
		}
	}
	if in == nil {
		return me, nil
	}
	in.(LazyLinkedInstructionInterface).SetNext(me)
	return in, nil
}

func (ct *LazyCompiler) SetNext(next flywheel.Compiler) {}

type LazyBlockCompiler struct {
	Next     flywheel.Compiler
	Left     flywheel.Compiler
	Right    flywheel.Compiler
	Token    *matchers.Token
	Location *flywheel.Location
}

func (ct *LazyBlockCompiler) Compile(ce flywheel.CompileContext) (flywheel.Instruction, error) {
	var in flywheel.Instruction = nil
	if ct.Right != nil {
		in, _ = ct.Right.Compile(ce)
	}
	if in == nil {
		return nil, nil
	}
	return in, nil
}

func (ct *LazyBlockCompiler) SetNext(next flywheel.Compiler) {}

func BuildLazyPrefixCompilerTest(pe *flywheel.ParseEnvironment, t *matchers.Token, l *flywheel.Location, right flywheel.Compiler) (flywheel.Compiler, error) {
	return &LazyCompiler{
		Next:     nil,
		Token:    t,
		Location: l,
		Right:    right,
	}, nil
}

func BuildLazyInfixCompilerTest(pe *flywheel.ParseEnvironment, t *matchers.Token, l *flywheel.Location, left flywheel.Compiler, right flywheel.Compiler) (flywheel.Compiler, error) {
	return &LazyCompiler{
		Next:     nil,
		Token:    t,
		Location: l,
		Left:     left,
		Right:    right,
	}, nil
}

func BuildLazyPrefixBlockCompilerTest(pe *flywheel.ParseEnvironment, t *matchers.Token, l *flywheel.Location, right flywheel.Compiler) (flywheel.Compiler, error) {
	return &LazyBlockCompiler{
		Next:     nil,
		Token:    t,
		Location: l,
		Right:    right,
	}, nil
}

type LazyCompileContext struct{}

func TestLazyParser_Parsing(t *testing.T) {
	l := lexx.BuildLexxWithString("2", []matchers.LexxMatcherInitialize{
		matchers.StartIntegerMatcher,
	})
	parser := flywheel.NewParseEnvironment(l)

	parser.AddPrefixParser(flywheel.PrefixParsletBuilder(matchers.INTEGER, flywheel.PRECEDENCE_NONE, BuildLazyPrefixCompilerTest))

	resultc, err := flywheel.ParseElement(parser, flywheel.PRECEDENCE_NONE)
	if err != nil {
		t.Error(err)
	}

	resulti, err := resultc.Compile(&LazyCompileContext{})
	if err != nil {
		t.Error(err)
	}

	ctx := &LazyExecutionContext{
		out: "",
	}

	for resulti != nil {
		resulti, err = resulti.Execute(ctx)
	}

	lazy, _ := ctx.stack.Pop()

	if lazy() != 2 {
		t.Errorf("Did not get expected results, expected 2 got %d\n", lazy())
	}
}

func TestLazyParser_InfixParsing(t *testing.T) {
	l := lexx.BuildLexxWithString("2 + 1", []matchers.LexxMatcherInitialize{
		matchers.StartIntegerMatcher,
		matchers.StartWhitespaceMatcher,
		matchers.ConfigOperatorMatcher([]string{"+"}),
	})
	parser := flywheel.NewParseEnvironment(l)

	parser.AddPrefixParser(flywheel.PrefixParsletBuilder(matchers.INTEGER, flywheel.PRECEDENCE_NONE, BuildLazyPrefixCompilerTest))
	parser.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"+"}, matchers.OPERATOR, flywheel.PRECEDENCE_SUM, BuildLazyInfixCompilerTest))

	compilers, err := flywheel.ParseElement(parser, flywheel.PRECEDENCE_NONE)
	if err != nil {
		t.Error(err)
	}

	instructions, err := compilers.Compile(&LazyCompileContext{})
	if err != nil {
		t.Error(err)
	}

	ctx := &LazyExecutionContext{
		out: "",
	}

	for instructions != nil {
		instructions, err = instructions.Execute(ctx)
	}

	lazy, _ := ctx.stack.Pop()

	if lazy() != 3 {
		t.Errorf("Did not get expected results, expected 3 got %d\n", lazy())
	}
}

func TestLazyParser_PrecedenceParsing(t *testing.T) {
	l := lexx.BuildLexxWithString("2 * 1 + 5", []matchers.LexxMatcherInitialize{
		matchers.StartIntegerMatcher,
		matchers.StartWhitespaceMatcher,
		matchers.ConfigOperatorMatcher([]string{"+", "*"}),
	})
	parser := flywheel.NewParseEnvironment(l)

	parser.AddPrefixParser(flywheel.PrefixParsletBuilder(matchers.INTEGER, flywheel.PRECEDENCE_NONE, BuildLazyPrefixCompilerTest))
	parser.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"+"}, matchers.OPERATOR, flywheel.PRECEDENCE_SUM, BuildLazyInfixCompilerTest))
	parser.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"*"}, matchers.OPERATOR, flywheel.PRECEDENCE_PRODUCT, BuildLazyInfixCompilerTest))

	resultc, err := flywheel.ParseElement(parser, flywheel.PRECEDENCE_NONE)
	if err != nil {
		t.Error(err)
	}

	resulti, err := resultc.Compile(&LazyCompileContext{})
	if err != nil {
		t.Error(err)
	}

	ctx := &LazyExecutionContext{
		out: "",
	}

	for resulti != nil {
		resulti, err = resulti.Execute(ctx)
	}

	lazy, _ := ctx.stack.Pop()

	if lazy() != 7 {
		t.Errorf("Did not get expected results, expected 7 got %d\n", lazy())
	}
}

func TestLazyParser_PrecedenceParsing2(t *testing.T) {
	l := lexx.BuildLexxWithString("2 + 3 * 5", []matchers.LexxMatcherInitialize{
		matchers.StartIntegerMatcher,
		matchers.StartWhitespaceMatcher,
		matchers.ConfigOperatorMatcher([]string{"+", "*"}),
	})
	parser := flywheel.NewParseEnvironment(l)

	parser.AddPrefixParser(flywheel.PrefixParsletBuilder(matchers.INTEGER, flywheel.PRECEDENCE_NONE, BuildLazyPrefixCompilerTest))
	parser.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"+"}, matchers.OPERATOR, flywheel.PRECEDENCE_SUM, BuildLazyInfixCompilerTest))
	parser.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"*"}, matchers.OPERATOR, flywheel.PRECEDENCE_PRODUCT, BuildLazyInfixCompilerTest))

	resultc, err := flywheel.ParseElement(parser, flywheel.PRECEDENCE_NONE)
	if err != nil {
		t.Error(err)
	}

	resulti, err := resultc.Compile(&LazyCompileContext{})
	if err != nil {
		t.Error(err)
	}

	ctx := &LazyExecutionContext{
		out: "",
	}

	for resulti != nil {
		resulti, err = resulti.Execute(ctx)
	}

	lazy, _ := ctx.stack.Pop()

	if lazy() != 17 {
		t.Errorf("Did not get expected results, expected 17 got %d\n", lazy())
	}
}

func TestLazyParser_PrecedenceParsingSub(t *testing.T) {
	l := lexx.BuildLexxWithString("2 + 3 * 5 + 7", []matchers.LexxMatcherInitialize{
		matchers.StartIntegerMatcher,
		matchers.StartWhitespaceMatcher,
		matchers.ConfigOperatorMatcher([]string{"(", ")", "+", "*"}),
	})
	parser := flywheel.NewParseEnvironment(l)

	parser.AddPrefixParser(flywheel.PrefixParsletSubBuilder(&matchers.Token{Type: matchers.OPERATOR, Value: "("}, &matchers.Token{Type: matchers.OPERATOR, Value: ")"}, BuildLazyPrefixCompilerTest))
	parser.AddPrefixParser(flywheel.PrefixParsletBuilder(matchers.INTEGER, flywheel.PRECEDENCE_NONE, BuildLazyPrefixCompilerTest))
	parser.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"+"}, matchers.OPERATOR, flywheel.PRECEDENCE_SUM, BuildLazyInfixCompilerTest))
	parser.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"*"}, matchers.OPERATOR, flywheel.PRECEDENCE_PRODUCT, BuildLazyInfixCompilerTest))

	resultc, err := flywheel.ParseElement(parser, flywheel.PRECEDENCE_NONE)
	if err != nil {
		t.Error(err)
	}

	resulti, err := resultc.Compile(&LazyCompileContext{})
	if err != nil {
		t.Error(err)
	}

	ctx := &LazyExecutionContext{
		out: "",
	}

	for resulti != nil {
		resulti, err = resulti.Execute(ctx)
	}

	lazy, _ := ctx.stack.Pop()

	if lazy() != 24 {
		t.Errorf("Did not get expected results, expected 24 got %d\n", lazy())
	}
}

func TestLazyParser_PrecedenceSubParsingSub(t *testing.T) {
	l := lexx.BuildLexxWithString("1 + 4*((2-3) * 2 + 6) - 3", []matchers.LexxMatcherInitialize{
		matchers.StartIntegerMatcher,
		matchers.StartWhitespaceMatcher,
		matchers.ConfigOperatorMatcher([]string{"(", ")", "+", "*", "-"}),
	})
	parser := flywheel.NewParseEnvironment(l)

	parser.AddPrefixParser(flywheel.PrefixParsletSubBuilder(&matchers.Token{Type: matchers.OPERATOR, Value: "("}, &matchers.Token{Type: matchers.OPERATOR, Value: ")"}, BuildLazyPrefixBlockCompilerTest))
	parser.AddPrefixParser(flywheel.PrefixParsletBuilder(matchers.INTEGER, flywheel.PRECEDENCE_NONE, BuildLazyPrefixCompilerTest))
	parser.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"+"}, matchers.OPERATOR, flywheel.PRECEDENCE_SUM, BuildLazyInfixCompilerTest))
	parser.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"-"}, matchers.OPERATOR, flywheel.PRECEDENCE_SUM, BuildLazyInfixCompilerTest))
	parser.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"*"}, matchers.OPERATOR, flywheel.PRECEDENCE_PRODUCT, BuildLazyInfixCompilerTest))

	resultc, err := flywheel.ParseElement(parser, flywheel.PRECEDENCE_NONE)
	if err != nil {
		t.Error(err)
	}

	resulti, err := resultc.Compile(&LazyCompileContext{})
	if err != nil {
		t.Error(err)
	}

	ctx := &LazyExecutionContext{
		out: "",
	}

	for resulti != nil {
		resulti, err = resulti.Execute(ctx)
	}

	lazy, _ := ctx.stack.Pop()

	if lazy() != 14 {
		t.Errorf("Did not get expected results, expected 14 got %d\n", lazy())
	}
}

type funcStack struct {
	lock sync.Mutex // you don't have to do this if you don't want thread safety
	s    []func() int
}

func NewStack() *funcStack {
	return &funcStack{sync.Mutex{}, make([]func() int, 0)}
}

func (s *funcStack) Push(v func() int) {
	s.lock.Lock()
	defer s.lock.Unlock()

	s.s = append(s.s, v)
}

func (s *funcStack) Pop() (func() int, error) {
	s.lock.Lock()
	defer s.lock.Unlock()

	l := len(s.s)
	if l == 0 {
		return nil, errors.New("Empty Stack")
	}

	res := s.s[l-1]
	s.s = s.s[:l-1]
	return res, nil
}

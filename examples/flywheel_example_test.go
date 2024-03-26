package examples

import (
	"errors"
	"flywheel"
	"github.com/JeffThomas/lexx-go/lexx"
	"github.com/JeffThomas/lexx-go/matchers"
	"strconv"
	"sync"
	"testing"
)

func Test_Example(t *testing.T) {
	l := lexx.NewLexx([]matchers.LexxMatcherInitialize{
		matchers.StartIntegerMatcher,
		matchers.StartWhitespaceMatcher,
		matchers.ConfigOperatorMatcher([]string{"+", "*"}),
	})
	l.SetStringInput("2 * 3 + 5")

	parser := flywheel.NewParseEnvironment(l)
	parser.AddPrefixParser(flywheel.PrefixParsletBuilder(matchers.INTEGER, flywheel.PRECEDENCE_NONE, BuildExamplePrefixCompilerTest))
	parser.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"+"}, matchers.OPERATOR, flywheel.PRECEDENCE_SUM, BuildExampleInfixCompilerTest))
	parser.AddInfixParser(flywheel.InfixSymbolParsletBuilder([]string{"*"}, matchers.OPERATOR, flywheel.PRECEDENCE_PRODUCT, BuildExampleInfixCompilerTest))

	resultc, err := flywheel.ParseElement(parser, flywheel.PRECEDENCE_NONE)
	if err != nil {
		t.Error(err)
	}

	resulti, err := resultc.Compile(&ExampleCompileContext{})
	if err != nil {
		t.Error(err)
	}

	ctx := &ExampleExecutionContext{
		out: "",
	}

	for resulti != nil {
		resulti, err = resulti.Execute(ctx)
	}

	number, _ := ctx.stack.Pop()

	if number != 11 {
		t.Errorf("Did not get expected results, expected 11 got %d\n", number)
	}
}

type ExampleCompiler struct {
	Next     flywheel.Compiler
	Left     flywheel.Compiler
	Right    flywheel.Compiler
	Token    *matchers.Token
	Location *flywheel.Location
}

func (ct *ExampleCompiler) Compile(ce flywheel.CompileContext) (flywheel.Instruction, error) {
	var in flywheel.Instruction = nil
	if ct.Left != nil {
		in, _ = ct.Left.Compile(ce)
	}
	if ct.Right != nil {
		r, _ := ct.Right.Compile(ce)
		if in == nil {
			in = r // prefix handling
		} else {
			in.(ExampleLinkedInstructionInterface).SetNext(r.(ExampleLinkedInstructionInterface))
		}
	}
	var me ExampleLinkedInstructionInterface = nil
	switch ct.Token.Type {
	case matchers.INTEGER:
		me = &ExampleNumberInstruction{
			InstructionNext: nil,
			Token:           ct.Token,
		}
	case matchers.OPERATOR:
		me = &ExampleOperatorInstruction{
			InstructionNext: nil,
			Token:           ct.Token,
		}
	}
	if in == nil {
		return me, nil
	}
	in.(ExampleLinkedInstructionInterface).SetNext(me)
	return in, nil
}

func (ct *ExampleCompiler) SetNext(next flywheel.Compiler) {}

type ExampleBlockCompiler struct {
	Next     flywheel.Compiler
	Left     flywheel.Compiler
	Right    flywheel.Compiler
	Token    *matchers.Token
	Location *flywheel.Location
}

func (ct *ExampleBlockCompiler) Compile(ce flywheel.CompileContext) (flywheel.Instruction, error) {
	var in flywheel.Instruction = nil
	if ct.Right != nil {
		in, _ = ct.Right.Compile(ce)
	}
	if in == nil {
		return nil, nil
	}
	return in, nil
}

func (ct *ExampleBlockCompiler) SetNext(next flywheel.Compiler) {}

func BuildExamplePrefixCompilerTest(pe *flywheel.ParseEnvironment, t *matchers.Token, l *flywheel.Location, right flywheel.Compiler) (flywheel.Compiler, error) {
	return &ExampleCompiler{
		Next:     nil,
		Token:    t,
		Location: l,
		Right:    right,
	}, nil
}

func BuildExampleInfixCompilerTest(pe *flywheel.ParseEnvironment, t *matchers.Token, l *flywheel.Location, left flywheel.Compiler, right flywheel.Compiler) (flywheel.Compiler, error) {
	return &ExampleCompiler{
		Next:     nil,
		Token:    t,
		Location: l,
		Left:     left,
		Right:    right,
	}, nil
}

func BuildExamplePrefixBlockCompilerTest(pe *flywheel.ParseEnvironment, t *matchers.Token, l *flywheel.Location, right flywheel.Compiler) (flywheel.Compiler, error) {
	return &ExampleBlockCompiler{
		Next:     nil,
		Token:    t,
		Location: l,
		Right:    right,
	}, nil
}

type ExampleCompileContext struct{}

type ExampleExecutionContext struct {
	out   string
	stack funcIntStack
}

type ExampleLinkedInstructionInterface interface {
	Execute(flywheel.ExecutionContext) (flywheel.Instruction, error)
	SetNext(ExampleLinkedInstructionInterface)
}

type ExampleNumberInstruction struct {
	InstructionNext ExampleLinkedInstructionInterface
	Token           *matchers.Token
}

func (it *ExampleNumberInstruction) Execute(ctxi flywheel.ExecutionContext) (flywheel.Instruction, error) {
	ctx, _ := ctxi.(*ExampleExecutionContext)
	i, _ := strconv.Atoi(it.Token.Value)
	ctx.stack.Push(i)
	return it.InstructionNext, nil
}

func (it *ExampleNumberInstruction) SetNext(newItem ExampleLinkedInstructionInterface) {
	if it.InstructionNext == nil {
		it.InstructionNext = newItem
		return
	}
	it.InstructionNext.SetNext(newItem)
}

type ExampleOperatorInstruction struct {
	InstructionNext ExampleLinkedInstructionInterface
	Token           *matchers.Token
}

func (it *ExampleOperatorInstruction) Execute(ctxi flywheel.ExecutionContext) (flywheel.Instruction, error) {
	ctx, _ := ctxi.(*ExampleExecutionContext)
	second, _ := ctx.stack.Pop()
	first, _ := ctx.stack.Pop()
	switch it.Token.Value {
	case "+":
		ctx.stack.Push(first + second)
	case "-":
		ctx.stack.Push(first - second)
	case "*":
		ctx.stack.Push(first * second)
	case "/":
		ctx.stack.Push(first / second)
	}
	return it.InstructionNext, nil
}

func (it *ExampleOperatorInstruction) SetNext(newItem ExampleLinkedInstructionInterface) {
	if it.InstructionNext == nil {
		it.InstructionNext = newItem
		return
	}
	it.InstructionNext.SetNext(newItem)
}

type funcIntStack struct {
	lock sync.Mutex // you don't have to do this if you don't want thread safety
	s    []int
}

func NewStack() *funcIntStack {
	return &funcIntStack{sync.Mutex{}, make([]int, 0)}
}

func (s *funcIntStack) Push(v int) {
	s.lock.Lock()
	defer s.lock.Unlock()

	s.s = append(s.s, v)
}

func (s *funcIntStack) Pop() (int, error) {
	s.lock.Lock()
	defer s.lock.Unlock()

	l := len(s.s)
	if l == 0 {
		return 0, errors.New("Empty Stack")
	}

	res := s.s[l-1]
	s.s = s.s[:l-1]
	return res, nil
}

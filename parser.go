package flywheel

import (
	"errors"
	"github.com/JeffThomas/lexx-go/matchers"
	"strconv"
)

func matchPrefixParser(pe *ParseEnvironment, t *matchers.Token, precedence PrecedenceType) PrefixParsletParse {
	for _, match := range pe.PrefixParsers {
		parse := match(t, precedence)
		if parse != nil {
			return parse
		}
	}
	return nil
}

func matchInfixParser(pe *ParseEnvironment, t *matchers.Token, precedence PrecedenceType) InfixParsletParse {
	for _, match := range pe.InfixParsers {
		parse := match(t, precedence)
		if parse != nil {
			return parse
		}
	}
	return nil
}

func ParseElement(pe *ParseEnvironment, precedence PrecedenceType) (Compiler, error) {
	currentToken, err := pe.Lexx.GetNextToken()
	if currentToken != nil && currentToken.Type == matchers.WHITESPACE {
		currentToken, err = pe.Lexx.GetNextToken()
	}

	if err != nil || currentToken != nil && currentToken.Type == matchers.SYSTEM && currentToken.Value == "EOF" {
		return nil, err
	}

	if currentToken == nil {
		return nil, errors.New("could not match token '" + string(pe.Lexx.State.CurrentText) + "' at " + strconv.Itoa(pe.Lexx.State.Line) + ", " + strconv.Itoa(pe.Lexx.State.Column))
	}

	var left Compiler = nil
	err = nil

	prefixP := matchPrefixParser(pe, currentToken, precedence)
	if prefixP == nil {
		pe.Lexx.PushToken()
		return nil, nil
	}

	pe.Line = pe.Lexx.State.Line
	pe.Column = pe.Lexx.State.Column
	left, err = prefixP(pe, precedence)
	if err != nil {
		return nil, err
	}

	for {
		currentToken, err = pe.Lexx.GetNextToken()
		if currentToken != nil && currentToken.Type == matchers.WHITESPACE {
			currentToken, err = pe.Lexx.GetNextToken()
		}
		if err == nil && currentToken == nil {
			return nil, errors.New("could not match token '" + string(pe.Lexx.State.CurrentText) + "' at " + strconv.Itoa(pe.Lexx.State.LineNext) + ", " + strconv.Itoa(pe.Lexx.State.ColumnNext))
		}
		if err != nil || currentToken.Type == matchers.SYSTEM && currentToken.Value == "EOF" {
			return left, nil
		}

		infixP := matchInfixParser(pe, currentToken, precedence)
		if infixP == nil {
			pe.Lexx.PushToken()
			return left, nil
		} else {
			pe.Line = pe.Lexx.State.Line
			pe.Column = pe.Lexx.State.Column
			element, err := infixP(pe, precedence, left)
			if err != nil {
				return nil, err
			}
			if element == nil {
				return left, nil
			} else {
				left = element
			}
		}
	}
}

func ParseBlock(pe *ParseEnvironment, blockEndToken *matchers.Token) (Compiler, error) { // parse
	var head Compiler
	var tail Compiler

	startLine := pe.Line
	startColumn := pe.Column

	for {
		t, err := pe.Lexx.GetNextToken()

		if err != nil {
			return nil, err
		}
		if t == nil {
			return nil, errors.New("Could not match token " + pe.Script + " '" + string(pe.Lexx.State.CurrentText) + "' at " + strconv.Itoa(pe.Line) + ", " + strconv.Itoa(pe.Column) + "\n")
		}
		if (t.Type == matchers.SYSTEM && t.Value == "EOF") || blockEndToken != nil && blockEndToken.Equals(t) {
			if t.Value == "EOF" && blockEndToken != nil {
				return nil, errors.New("unexpected EOF, did not find end of Block started at " + strconv.Itoa(startLine) + ", " + strconv.Itoa(startColumn))
			}
			return head, err
		}

		pe.Lexx.PushToken()
		right, err := ParseElement(pe, PRECEDENCE_NONE)
		if err != nil {
			return nil, err
		}
		if right == nil {
			continue
		}
		if head == nil {
			head = right
			tail = right
		} else {
			tail.SetNext(right)
			tail = right
		}
	}
}

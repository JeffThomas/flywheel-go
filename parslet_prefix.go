package flywheel

import (
	"github.com/JeffThomas/lexx-go/matchers"
)

func PrefixParsletBuilder(tokenType matchers.TokenType, precedenceLocal PrecedenceType, compilerBuilder PrefixCompilerBuilder) PrefixParsletMatch { // Build
	return func(t *matchers.Token, precedence PrecedenceType) PrefixParsletParse { // match
		if t.Type != tokenType {
			return nil
		}
		return func(pe *ParseEnvironment, precedence PrecedenceType) (Compiler, error) { // parse
			t := pe.Lexx.Token
			right, err := ParseElement(pe, precedence)
			if err != nil {
				return nil, err
			}
			return compilerBuilder(pe, t, &Location{Line: t.Line, Column: t.Column, Script: pe.Script}, right)
		}
	}
}

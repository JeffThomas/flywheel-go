package flywheel

import (
	"github.com/JeffThomas/lexx-go/matchers"
)

func InfixSymbolParsletBuilder(matches []string, tokenType matchers.TokenType, precedenceLocal PrecedenceType, infixCompilerBuilder InfixCompilerBuilder) InfixParsletMatch { // build
	return func(t *matchers.Token, precedence PrecedenceType) InfixParsletParse { // match
		if precedence >= precedenceLocal {
			return nil
		}
		if t.Type != tokenType {
			return nil
		}
		for _, s := range matches {
			if t.Value == s {
				return func(pe *ParseEnvironment, precedence PrecedenceType, left Compiler) (Compiler, error) { // parse
					t := pe.Lexx.Token
					right, err := ParseElement(pe, precedenceLocal)
					if err != nil {
						return nil, err
					}
					return infixCompilerBuilder(pe, t, &Location{Line: t.Line, Column: t.Column, Script: pe.Script}, left, right)
				}
			}
		}
		return nil
	}
}

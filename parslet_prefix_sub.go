package flywheel

import "github.com/JeffThomas/lexx-go/matchers"

func PrefixParsletSubBuilder(blockStartToken *matchers.Token, blockEndToken *matchers.Token, prefixCompilerBuilder PrefixCompilerBuilder) PrefixParsletMatch { // initialize
	return func(t *matchers.Token, precedence PrecedenceType) PrefixParsletParse { // match
		if blockStartToken == nil || blockStartToken.Equals(t) {
			return func(pe *ParseEnvironment, precedence PrecedenceType) (Compiler, error) { // parse

				head, err := ParseBlock(pe, blockEndToken)
				if err != nil {
					return nil, err
				}

				if prefixCompilerBuilder != nil {
					result, err := prefixCompilerBuilder(pe, blockStartToken, &Location{Line: t.Line, Column: t.Column, Script: pe.Script}, head)
					return result, err
				} else {
					return head, nil
				}
			}
		}
		return nil
	}
}

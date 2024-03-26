package flywheel

import "github.com/JeffThomas/lexx-go/lexx"

type ParseEnvironment struct {
	Lexx          *lexx.Lexx
	PrefixParsers []PrefixParsletMatch
	InfixParsers  []InfixParsletMatch
	Line          int
	Column        int
	Script        string
}

func (pe *ParseEnvironment) AddPrefixParser(parslet PrefixParsletMatch) {
	pe.PrefixParsers = append(pe.PrefixParsers, parslet)
}

func (pe *ParseEnvironment) AddInfixParser(parslet InfixParsletMatch) {
	pe.InfixParsers = append(pe.InfixParsers, parslet)
}

func NewParseEnvironment(lex *lexx.Lexx) *ParseEnvironment {
	return &ParseEnvironment{
		PrefixParsers: make([]PrefixParsletMatch, 0),
		InfixParsers:  make([]InfixParsletMatch, 0),
		Lexx:          lex,
	}
}

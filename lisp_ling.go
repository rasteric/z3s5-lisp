package z3s5

import (
	ling "github.com/jamesturk/go-jellyfish"
	"github.com/nukata/goarith"
)

func (interp *Interp) Define_Ling() {

	// (ling.soundex s) => str compute the soundex representation of s
	interp.Def("ling.soundex", 1, func(a []any) any {
		return ling.Soundex(a[0].(string))
	})

	// (ling.metaphone s) => str compute the metaphone representation of s
	interp.Def("ling.metaphone", 1, func(a []any) any {
		return ling.Metaphone(a[0].(string))
	})

	// (ling.nysiis s) => str compute the nysiis representation of s
	interp.Def("ling.nysiis", 1, func(a []any) any {
		return ling.Nysiis(a[0].(string))
	})

	// (ling.porter s) => str compute the stem of s using the Porter stemming algorithm
	interp.Def("ling.porter", 1, func(a []any) any {
		return ling.Porter(a[0].(string))
	})

	// (ling.match-rating-codex s) => str compute the match-rating codex of s
	interp.Def("ling.match-rating-codex", 1, func(a []any) any {
		return ling.MatchRatingCodex(a[0].(string))
	})

	// (ling.damerau-levenshtein s1 s2) => num compute the Damerau-Levenshtein distance between s1 and s2
	interp.Def("ling.damerau-levenshtein", 2, func(a []any) any {
		return goarith.AsNumber(ling.DamerauLevenshtein(a[0].(string), a[1].(string)))
	})

	// (ling.hamming s1 s2) => num compute the Hamming distance between s1 and s2
	interp.Def("ling.hamming", 2, func(a []any) any {
		return goarith.AsNumber(ling.Hamming(a[0].(string), a[1].(string)))
	})

	// (ling.jaro s1 s2) => num compute the Jaro distance between s1 and s2
	interp.Def("ling.jaro", 2, func(a []any) any {
		return goarith.AsNumber(ling.Jaro(a[0].(string), a[1].(string)))
	})

	// (ling.jaro-winkler s1 s2) => num compute the Jaro-Winkler distance between s1 and s2
	interp.Def("ling.jaro-winkler", 2, func(a []any) any {
		return goarith.AsNumber(ling.JaroWinkler(a[0].(string), a[1].(string)))
	})

	// (ling.levenshtein s1 s2) => num compute the Levenshtein distance between s1 and s2
	interp.Def("ling.levenshtein", 2, func(a []any) any {
		return goarith.AsNumber(ling.Levenshtein(a[0].(string), a[1].(string)))
	})

	// (ling.match-rating-compare s1 s2) => bool return true if s1 and s2 are equal according to the Match-Rating algorithm, nil otherwise.
	interp.Def("ling.match-rating-compare", 2, func(a []any) any {
		return AsLispBool(ling.MatchRatingComparison(a[0].(string), a[1].(string)))
	})
}
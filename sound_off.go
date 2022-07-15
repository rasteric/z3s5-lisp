//go:build nosound
// +build nosound

package z3s5

func getSoundData(snd int) []byte {
	return nil
}

func playSystemSound(snd int) {
	// do nothing
}

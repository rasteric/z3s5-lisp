//go:build nosound
// +build nosound

package z3s5

const supportsSound = false

func getSoundData(snd int) []byte {
	return nil
}

func playSystemSound(snd int) {
	// do nothing
}

func setVolume(vol float64) {
	// do nothing
}

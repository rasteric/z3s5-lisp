//go:build !nosound
// +build !nosound

package z3s5

import (
	"bytes"
	_ "embed"
	"io"
	"log"
	"time"

	"github.com/faiface/beep/speaker"
	ogg "github.com/faiface/beep/vorbis"
)

//go:embed sounds/click.ogg
var clickData []byte

//go:embed sounds/confirm.ogg
var confirmData []byte

//go:embed sounds/error.ogg
var errorData []byte

//go:embed sounds/info.ogg
var infoData []byte

//go:embed sounds/okay.ogg
var okayData []byte

//go:embed sounds/ready.ogg
var readyData []byte

//go:embed sounds/start.ogg
var startData []byte

// getSoundData returns the sound data associated with a sound constant.
func getSoundData(snd int) []byte {
	switch snd {
	case SND_START:
		return startData
	case SND_CLICK:
		return clickData
	case SND_CONFIRM:
		return confirmData
	case SND_INFO:
		return infoData
	case SND_OKAY:
		return okayData
	case SND_READY:
		return readyData
	default:
		return errorData
	}
}

var soundInited bool

func playSystemSound(snd int) {
	reader := bytes.NewReader(getSoundData(snd))
	data := io.NopCloser(reader)
	streamer, format, err := ogg.Decode(data)
	if err != nil {
		log.Fatal(err)
	}
	if !soundInited {
		speaker.Init(format.SampleRate, format.SampleRate.N(time.Second/10))
		soundInited = true
	}
	speaker.Play(streamer)
}

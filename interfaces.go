package z3s5

type Editing interface {
	StartInput(cb func())
	EndInput() (string, bool)
	EndInputCallback() func()
	Print(s string) (int, int)
	SetColor(bg bool, r, g, b, a uint8)
	Color(bg bool) (uint8, uint8, uint8, uint8)
}

type Audio interface {
	SystemSound(id int)
	SetVolume(fl float64)
}

type Runtime interface {
	EditorInterface() Editing
	SoundInterface() Audio
	Perm() Permissions
	SetPerm(Permissions) error
	RequestShutdown(errcode int)
	Enqueue(func())
}

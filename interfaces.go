package z3s5

type Editing interface {
	StartInput()
	EndInput() (string, bool)
	Print(s string)
}

type Audio interface {
	SystemSound(id int)
}

type Runtime interface {
	Editor() Editing
	Sound() Audio
	Perm() Permissions
	RequestShutdown(errcode int)
	Enqueue(func())
}

package gui

import (
	"fmt"

	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/container"
	"fyne.io/fyne/v2/layout"
	"fyne.io/fyne/v2/theme"
	"fyne.io/fyne/v2/widget"
	z3 "github.com/rasteric/z3s5-lisp"
)

var BoxedApp = z3.NewSym("gui.app")
var BoxedWindow = z3.NewSym("gui.window")
var BoxedLabel = z3.NewSym("gui.label")
var BoxedSize = z3.NewSym("gui.size")
var BoxedButton = z3.NewSym("gui.button")
var BoxedSpacer = z3.NewSym("gui.spacer")
var BoxedContainer = z3.NewSym("gui.container")
var BoxedLayout = z3.NewSym("gui.box-layout")
var BoxedResource = z3.NewSym("gui.resource")
var BoxedTabItem = z3.NewSym("gui.tabitem")

func DefGUI(interp *z3.Interp) {

	interp.DefBoxed(BoxedApp)
	interp.DefBoxed(BoxedWindow)
	interp.DefBoxed(BoxedLabel)
	interp.DefBoxed(BoxedSize)
	interp.DefBoxed(BoxedButton)
	interp.DefBoxed(BoxedSpacer)
	interp.DefBoxed(BoxedContainer)
	interp.DefBoxed(BoxedLayout)

	// register this module
	reflect, ok := interp.GetGlobalVar(z3.ReflectSym)
	if !ok {
		reflect = z3.Nil
	}
	interp.SetGlobalVar(z3.ReflectSym, &z3.Cell{Car: z3.NewSym("gui"), Cdr: reflect})

	// the new commands

	// fyne.App
	interp.Def("gui.new-app", 1, func(a []any) any {
		apl := app.NewWithID(a[0].(string))
		return &z3.Boxed{Datum: apl, Sort: BoxedApp, Valid: true}
	})

	interp.Def("gui.app-run", 1, func(a []any) any {
		apl := z3.MustGetBoxed("gui.appp-run", a[0], BoxedApp)
		apl.Datum.(fyne.App).Run()
		return z3.Void
	})

	// fyne.Window
	interp.Def("gui.new-window", 2, func(a []any) any {
		apl := z3.MustGetBoxed("gui.new-window", a[0], BoxedApp)
		title := a[1].(string)
		win := apl.Datum.(fyne.App).NewWindow(title)
		return &z3.Boxed{Datum: win, Sort: BoxedWindow, Valid: true}
	})

	interp.Def("gui.window-set-content", 2, func(a []any) any {
		win := z3.MustGetBoxed("gui.window-set-content", a[0], BoxedWindow)
		content := a[1].(*z3.Boxed)
		win.Datum.(fyne.Window).SetContent(content.Datum.(fyne.CanvasObject))
		return z3.Void
	})

	interp.Def("gui.window-show-and-run", 1, func(a []any) any {
		win := z3.MustGetBoxed("gui.window-show-and-run", a[0], BoxedWindow)
		win.Datum.(fyne.Window).ShowAndRun()
		return z3.Void
	})

	interp.Def("gui.window-close", 1, func(a []any) any {
		win := z3.MustGetBoxed("gui.window-close", a[0], BoxedWindow)
		win.Datum.(fyne.Window).Close()
		return z3.Void
	})

	interp.Def("gui.window-show", 1, func(a []any) any {
		win := z3.MustGetBoxed("gui.window-show", a[0], BoxedWindow)
		win.Datum.(fyne.Window).Show()
		return z3.Void
	})

	interp.Def("gui.window-set-master", 1, func(a []any) any {
		win := z3.MustGetBoxed("gui.window-set-master", a[0], BoxedWindow)
		win.Datum.(fyne.Window).SetMaster()
		return z3.Void
	})

	interp.Def("gui.window-resize", 2, func(a []any) any {
		win := z3.MustGetBoxed("gui.window-resize", a[0], BoxedWindow)
		size := z3.MustGetBoxed("gui.window-resize", a[1], BoxedSize)
		win.Datum.(fyne.Window).Resize(size.Datum.(fyne.Size))
		return z3.Void
	})

	// fyne.Size
	interp.Def("gui.new-size", 2, func(a []any) any {
		return &z3.Boxed{Datum: fyne.NewSize(float32(z3.ToFloat64(a[0])), float32(z3.ToFloat64(a[1]))),
			Sort: BoxedSize, Valid: true}
	})

	// WIDGETS

	// widget.Label
	interp.Def("gui.new-label", 1, func(a []any) any {
		l := widget.NewLabel(a[0].(string))
		return &z3.Boxed{Datum: l, Sort: BoxedLabel, Valid: true}
	})

	// widget.Button
	interp.Def("gui.new-button", 2, func(a []any) any {
		box := &z3.Boxed{Datum: nil, Sort: BoxedButton, Valid: false}
		proc := a[1].(*z3.Closure)
		b := widget.NewButton(a[0].(string), func() {
			li2 := z3.Nil
			li := &z3.Cell{Car: proc, Cdr: li2}
			interp.Eval(li, z3.Nil)
		})
		box.Datum = b
		box.Valid = true
		return box
	})

	// LAYOUT
	interp.Def("gui.new-spacer", 0, func(a []any) any {
		return &z3.Boxed{Datum: layout.NewSpacer(), Sort: BoxedSpacer, Valid: true}
	})

	interp.Def("gui.new-hbox-layout", 0, func(a []any) any {
		return &z3.Boxed{Datum: layout.NewHBoxLayout(), Sort: BoxedLayout, Valid: true}
	})

	interp.Def("gui.new-vbox-layout", 0, func(a []any) any {
		return &z3.Boxed{Datum: layout.NewVBoxLayout(), Sort: BoxedLayout, Valid: true}
	})

	interp.Def("gui.new-grid-layout", 1, func(a []any) any {
		n := z3.ToInt64("gui.new-grid-layout", a[0])
		return &z3.Boxed{Datum: layout.NewGridLayout(int(n)), Sort: BoxedLayout, Valid: true}
	})

	interp.Def("gui.new-grid-wrap-layout", 1, func(a []any) any {
		boxed := z3.MustGetBoxed("gui.new-grid-wrap-layout", a[0], BoxedSize)
		size := boxed.Datum.(fyne.Size)
		return &z3.Boxed{Datum: layout.NewGridWrapLayout(size), Sort: BoxedLayout, Valid: true}
	})

	interp.Def("gui.new-form-layout", 0, func(a []any) any {
		return &z3.Boxed{Datum: layout.NewFormLayout(), Sort: BoxedLayout, Valid: true}
	})

	interp.Def("gui.new-center-layout", 0, func(a []any) any {
		return &z3.Boxed{Datum: layout.NewCenterLayout(), Sort: BoxedLayout, Valid: true}
	})

	interp.Def("gui.new-max-layout", 0, func(a []any) any {
		return &z3.Boxed{Datum: layout.NewMaxLayout(), Sort: BoxedLayout, Valid: true}
	})

	// CONTAINER

	interp.Def("gui.new-container", -1, func(a []any) any {
		li := a[0].(*z3.Cell)
		layout := z3.MustGetBoxed("gui.new-container", li.Car, BoxedLayout)
		li = li.CdrCell()
		objs := make([]fyne.CanvasObject, 0, len(a)-1)
		for li != z3.Nil {
			objs = append(objs, li.Car.(*z3.Boxed).Datum.(fyne.CanvasObject))
			li = li.CdrCell()
		}
		c := container.New(layout.Datum.(fyne.Layout), objs...)
		return &z3.Boxed{Datum: c, Sort: BoxedContainer, Valid: true}
	})

	interp.Def("gui.new-border", -1, func(a []any) any {
		arr := make([]fyne.CanvasObject, 0)
		args := z3.ListToArray(a[0].(*z3.Cell))
		if len(args) < 4 {
			panic(fmt.Sprintf("gui.new-border requires at least top, bottom, left, right arguments, given: %v",
				a[0].(*z3.Cell)))
		}
		for _, arg := range args {
			if cell, ok := arg.(*z3.Cell); ok {
				if cell != z3.Nil {
					panic(fmt.Sprintf("gui.new-border expected a valid canvas object or nil, given a non-nil list: %v",
						z3.Str(arg)))
				}
				arr = append(arr, nil)
				continue
			}
			if b, ok := arg.(*z3.Boxed); ok {
				if canvas, ok := b.Datum.(fyne.CanvasObject); ok {
					arr = append(arr, canvas)
					continue
				}
				panic(fmt.Sprintf("gui.new-border expected valid canvas object or nil, given a boxed object that is not a valid canvas: %v", z3.Str(arg)))
			}
			panic(fmt.Sprintf("gui.new-border expected a valid canvas object or nil, given: %v", z3.Str(arg)))
		}
		return &z3.Boxed{Datum: container.NewBorder(arr[0], arr[1], arr[2], arr[3], arr[4:]...),
			Sort: BoxedContainer, Valid: true}
	})

	interp.Def("gui.new-tabitem", 2, func(a []any) any {
		title := a[0].(string)
		arg, ok := a[1].(*z3.Boxed)
		if !ok {
			panic(fmt.Sprintf("gui.new-tabitem expected valid boxed canvas object, given %v", z3.Str(a[1])))
		}
		obj, ok := arg.Datum.(fyne.CanvasObject)
		if !ok {
			panic(fmt.Sprintf("gui.new-tabitem argument must be a canvas object, given %v", z3.Str(arg)))
		}
		return &z3.Boxed{Datum: container.NewTabItem(title, obj), Sort: BoxedTabItem, Valid: true}
	})

	interp.Def("gui.new-tabitem-with-icon", 3, func(a []any) any {
		title := a[0].(string)
		icon := z3.MustGetBoxed("gui.new-tabitem-with-icon", a[1], BoxedResource)
		ics, ok := icon.Datum.(fyne.Resource)
		if !ok {
			panic(fmt.Sprintf("gui.new-tabitem-with-icon expected an icon resource as second argument, received: %v", z3.Str(a[1])))
		}
		canvas, ok := a[2].(*z3.Boxed)
		if !ok {
			panic(fmt.Sprintf("gui.new-tabitem-with-icon expected a container resource as second argument, received: %v", z3.Str(a[2])))
		}
		return &z3.Boxed{Datum: container.NewTabItemWithIcon(title, ics, canvas.Datum.(fyne.CanvasObject)),
			Sort: BoxedTabItem, Valid: true}
	})

	// container.AppTabs
	interp.Def("gui.new-app-tabs", -1, func(a []any) any {
		li := a[0].(*z3.Cell)
		arr := make([]*container.TabItem, 0)
		for li != z3.Nil {
			tab := z3.MustGetBoxed("gui.new-app-tabs", li.Car, BoxedTabItem)
			arr = append(arr, tab.Datum.(*container.TabItem))
			li = li.CdrCell()
		}
		return &z3.Boxed{Datum: container.NewAppTabs(arr...), Sort: BoxedContainer, Valid: true}
	})

	// container.DocTabs
	interp.Def("gui.new-doc-tabs", -1, func(a []any) any {
		li := a[0].(*z3.Cell)
		arr := make([]*container.TabItem, 0)
		for li != z3.Nil {
			tab := z3.MustGetBoxed("gui.new-doc-tabs", li.Car, BoxedTabItem)
			arr = append(arr, tab.Datum.(*container.TabItem))
			li = li.CdrCell()
		}
		return &z3.Boxed{Datum: container.NewDocTabs(arr...), Sort: BoxedContainer, Valid: true}
	})

	// RESOURCES

	// THEME ICONS
	interp.Def("gui.theme-icon", 1, func(a []any) any {
		var name string
		if sym, ok := a[0].(*z3.Sym); ok {
			name = sym.String()
		} else {
			name = a[0].(string)
		}
		var res fyne.Resource
		switch name {
		case "account", "AccountIcon":
			res = theme.AccountIcon()
			// The following seem to be misdocumented:
		// case "arrow-drop-down", "ArrowDropDownIcon":
		// 	res = theme.Icon(theme.IconNameArrowDropDown)
		// case "arrow-drop-up", "ArrowDropUpIcon":
		// 	res = theme.ArrowDropUpIcon()
		case "cancel", "CancelIcon":
			res = theme.CancelIcon()
		case "check-button-checked", "CheckButtonChecked":
			res = theme.CheckButtonCheckedIcon()
		case "check-button", "CheckButtonIcon":
			res = theme.CheckButtonIcon()
		case "color-achromatic", "ColorAchromaticIcon":
			res = theme.ColorAchromaticIcon()
		case "color-chromatic", "ColorChromaticIcon":
			res = theme.ColorChromaticIcon()
		case "color-palette", "ColorPaletteIcon":
			res = theme.ColorPaletteIcon()
		case "computer", "ComputerIcon":
			res = theme.ComputerIcon()
		case "confirm", "ConfirmIcon":
			res = theme.ConfirmIcon()
		case "content-add", "ContentAddIcon":
			res = theme.ContentAddIcon()
		case "content-clear", "ContentClearIcon":
			res = theme.ContentClearIcon()
		case "content-copy", "ContentCopyIcon":
			res = theme.ContentCopyIcon()
		case "content-cut", "ContentCutIcon":
			res = theme.ContentCutIcon()
		case "content-paste", "ContentPasteIcon":
			res = theme.ContentPasteIcon()
		case "content-redo", "ContentRedoIcon":
			res = theme.ContentRedoIcon()
		case "content-remove", "ContentRemoveIcon":
			res = theme.ContentRemoveIcon()
		case "content-undo", "ContentUndoIcon":
			res = theme.ContentUndoIcon()
		case "delete", "DeleteIcon":
			res = theme.DeleteIcon()
		case "document-create", "DocumentCreateIcon":
			res = theme.DocumentCreateIcon()
		case "document-print", "DocumentPrintIcon":
			res = theme.DocumentPrintIcon()
		case "document", "DocumentIcon":
			res = theme.DocumentIcon()
		case "download", "DownloadIcon":
			res = theme.DownloadIcon()
		case "error", "ErrorIcon":
			res = theme.ErrorIcon()
		case "file-application", "FileApplicationIcon":
			res = theme.FileApplicationIcon()
		case "file-audio", "FileAudioIcon":
			res = theme.FileAudioIcon()
		case "file-image", "FileImageIcon":
			res = theme.FileImageIcon()
		case "file-text", "FileTextIcon":
			res = theme.FileTextIcon()
		case "file-video", "FileVideoIcon":
			res = theme.FileVideoIcon()
		case "file", "FileIcon":
			res = theme.FileIcon()
		case "folder-new", "FolderNewIcon":
			res = theme.FolderNewIcon()
		case "folder-open", "FolderOpenIcon":
			res = theme.FolderOpenIcon()
		case "folder", "FolderIcon":
			res = theme.FolderIcon()
		case "grid", "GridIcon":
			res = theme.GridIcon()
		case "help", "HelpIcon":
			res = theme.HelpIcon()
		case "history", "HistoryIcon":
			res = theme.HistoryIcon()
		case "home", "HomeIcon":
			res = theme.HomeIcon()
		case "info", "InfoIcon":
			res = theme.InfoIcon()
		case "list", "ListIcon":
			res = theme.ListIcon()
		case "login", "LoginIcon":
			res = theme.LoginIcon()
		case "logout", "LogoutIcon":
			res = theme.LogoutIcon()
		case "mail-attachment", "MailAttachmentIcon":
			res = theme.MailAttachmentIcon()
		case "mail-compose", "MailComposeIcon":
			res = theme.MailComposeIcon()
		case "mail-forward", "MailForwardIcon":
			res = theme.MailForwardIcon()
		case "mail-reply-all", "MailReplyAllIcon":
			res = theme.MailReplyAllIcon()
		case "mail-reply", "MailReplyIcon":
			res = theme.MailReplyIcon()
		case "mail-send", "MailSendIcon":
			res = theme.MailSendIcon()
		case "media-fast-forward", "MediaFastForwardIcon":
			res = theme.MediaFastForwardIcon()
		case "media-fast-rewind", "MediaFastRewindIcon":
			res = theme.MediaFastRewindIcon()
		case "media-music", "MediaMusicIcon":
			res = theme.MediaMusicIcon()
		case "media-pause", "MediaPauseIcon":
			res = theme.MediaPauseIcon()
		case "media-photo", "MediaPhotoIcon":
			res = theme.MediaPhotoIcon()
		case "media-play", "MediaPlayIcon":
			res = theme.MediaPlayIcon()
		case "media-record", "MediaRecordIcon":
			res = theme.MediaRecordIcon()
		case "media-replay", "MediaReplayIcon":
			res = theme.MediaReplayIcon()
		case "media-skip-next", "MediaSkipNextIcon":
			res = theme.MediaSkipNextIcon()
		case "media-skip-previous", "MediaSkipPreviousIcon":
			res = theme.MediaSkipPreviousIcon()
		case "media-stop", "MediaStopIcon":
			res = theme.MediaStopIcon()
		case "media-video", "MediaVideoIcon":
			res = theme.MediaVideoIcon()
		case "menu-expand", "MenuExpandIcon":
			res = theme.MenuExpandIcon()
		case "menu", "MenuIcon":
			res = theme.MenuIcon()
		case "more-horizontal", "MoreHorizontalIcon":
			res = theme.MoreHorizontalIcon()
		case "more-vertical", "MoreVerticalIcon":
			res = theme.MoreVerticalIcon()
		case "move-down", "MoveDownIcon":
			res = theme.MoveDownIcon()
		case "move-up", "MoveUpIcon":
			res = theme.MoveUpIcon()
		case "navigate-back", "NavigateBackIcon":
			res = theme.NavigateBackIcon()
		case "navigate-next", "NavigateNextIcon":
			res = theme.NavigateNextIcon()
		case "question", "QuestionIcon":
			res = theme.QuestionIcon()
		case "radio-button-checked", "RadioButtonChecked":
			res = theme.RadioButtonCheckedIcon()
		case "radio-button", "RadioButtonIcon":
			res = theme.RadioButtonIcon()
		case "search-replace", "SearchReplaceIcon":
			res = theme.SearchReplaceIcon()
		case "search", "SearchIcon":
			res = theme.SearchIcon()
		case "settings", "SettingsIcon":
			res = theme.SettingsIcon()
		case "storage", "StorageIcon":
			res = theme.StorageIcon()
		case "upload", "UploadIcon":
			res = theme.UploadIcon()
		case "view-full-screen", "ViewFullScreenIcon":
			res = theme.ViewFullScreenIcon()
		case "view-refresh", "ViewRefreshIcon":
			res = theme.ViewRefreshIcon()
		case "view-restore", "ViewRestoreIcon":
			res = theme.ViewRestoreIcon()
			// The following seem to be unavailable / misdocumented:
		// case "view-zoom-fit", "ViewZoomFitIcon":
		// 	res = theme.ViewZoomFitIcon()
		// case "view-zoom-in", "ViewZoomInIcon":
		// 	res = theme.ViewZoomInIcon()
		// case "view-zoom-out", "ViewZoomOutIcon":
		// 	res = theme.ViewZoomOutIcon()
		case "visibility-off", "VisibilityOffIcon":
			res = theme.VisibilityOffIcon()
		case "visibility", "VisibilityIcon":
			res = theme.VisibilityIcon()
		case "volume-down", "VolumeDownIcon":
			res = theme.VolumeDownIcon()
		case "volume-mute", "VolumeMuteIcon":
			res = theme.VolumeMuteIcon()
		case "volume-up", "VolumeUpIcon":
			res = theme.VolumeUpIcon()
		case "warning", "WarningIcon":
			res = theme.WarningIcon()
		default:
			panic(fmt.Sprintf("gui.theme-icon: unknown theme icon name, given %v", z3.Str(a[0])))
		}
		return &z3.Boxed{Datum: res, Sort: BoxedResource, Valid: true}
	})
}

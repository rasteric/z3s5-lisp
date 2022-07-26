package z3s5

import (
	"errors"
)

var ErrNoSuchPermission = errors.New("security violation - missing permission")
var ErrPermissionManagerFail = errors.New("internal permission manager failure")

type Permissions struct {
	AllowUnprotect bool
	AllowProtect   bool
	LoadPrelude    bool
	LoadUserInit   bool
	Interactive    bool
	AllowFileRead  bool
	AllowFileWrite bool
}

// FullPermissions are the default full permissions of a machine,
// allowing full access to all resources of the machine.
var FullPermissions = Permissions{
	AllowUnprotect: true,
	AllowProtect:   true,
	LoadPrelude:    true,
	LoadUserInit:   true,
	Interactive:    true,
	AllowFileRead:  true,
	AllowFileWrite: true,
}

// SafeSilentPermissions are minimal permissions without sound, graphics, and file access
// but with prefs read access and console.
var RestrictedPermissions = Permissions{
	AllowUnprotect: false,
	AllowProtect:   false,
	LoadPrelude:    true,
	LoadUserInit:   true,
	Interactive:    false,
	AllowFileRead:  false,
	AllowFileWrite: false,
}

var ErrUnknownPermission = errors.New("unknown permission")
var ErrSecurityViolation = errors.New("security violation")

// NewPermissions creates a set of permissions from an array of symbolic strings.
// The permissions must be as restrictive or more restictive as the current permissions,
// or ErrSecurityViolation is returned.
func NewPermissions(current Permissions, perms []string) (Permissions, error) {
	p := Permissions{}
	for _, perm := range perms {
		switch perm {
		case "load-prelude":
			p.LoadPrelude = true
		case "load-user-init":
			p.LoadUserInit = true
		case "allow-unprotect":
			p.AllowUnprotect = true
		case "allow-protect":
			p.AllowProtect = true
		case "interactive":
			p.Interactive = true
		case "file-read":
			p.AllowFileRead = true
		case "file-write":
			p.AllowFileWrite = true
		}
	}
	return p, nil
}

func (p *Permissions) Strings() []string {
	s := make([]string, 0, 14)
	if p.LoadPrelude {
		s = append(s, "load-prelude")
	}
	if p.LoadUserInit {
		s = append(s, "load-user-init")
	}
	if p.AllowUnprotect {
		s = append(s, "allow-unprotect")
	}
	if p.AllowProtect {
		s = append(s, "allow-protect")
	}
	if p.Interactive {
		s = append(s, "interactive")
	}
	if p.AllowFileRead {
		s = append(s, "file-read")
	}
	if p.AllowFileWrite {
		s = append(s, "file-write")
	}
	return s
}

// Set sets a permission to the given value, returns an error if the permission cannot be changed.
// Permissions can only be changed from less secure to more secure, not vice versa, where the
// security hierarchy is hardcoded.
func (p Permissions) Set(perm string, v any) (Permissions, error) {
	value, _ := v.(bool)
	switch perm {
	case "load-prelude":
		if p.LoadPrelude == false && value == true {
			return p, ErrSecurityViolation
		}
		p.LoadPrelude = value
	case "load-user-init":
		if p.LoadUserInit == false && value == true {
			return p, ErrSecurityViolation
		}
		p.LoadUserInit = value
	case "allow-unprotect":
		if p.AllowUnprotect == false && value == true {
			return p, ErrSecurityViolation
		}
		p.AllowUnprotect = value
	case "allow-protect":
		if p.AllowProtect == false && value == true {
			return p, ErrSecurityViolation
		}
		p.AllowProtect = value
	case "interactive":
		if p.Interactive == false && value == true {
			return p, ErrSecurityViolation
		}
		p.Interactive = value
	case "file-read":
		if p.AllowFileRead == false && value == true {
			return p, ErrSecurityViolation
		}
		p.AllowFileRead = value
	case "file-write":
		if p.AllowFileWrite == false && value == true {
			return p, ErrSecurityViolation
		}
		p.AllowFileWrite = value
	default:
		return p, ErrUnknownPermission
	}
	return p, nil
}

func (p *Permissions) Get(s string) (any, error) {
	switch s {
	case "load-prelude":
		return p.LoadPrelude, nil
	case "load-user-init":
		return p.LoadUserInit, nil
	case "allow-unprotect":
		return p.AllowUnprotect, nil
	case "allow-protect":
		return p.AllowProtect, nil
	case "interactive":
		return p.Interactive, nil
	case "file-read":
		return p.AllowFileRead, nil
	case "file-write":
		return p.AllowFileWrite, nil
	default:
		return false, ErrUnknownPermission
	}
}

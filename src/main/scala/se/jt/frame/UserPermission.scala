package se.jt.frame

object UserPermission {
	case object Look extends UserPermission
	case object Copy extends UserPermission
	case object Edit extends UserPermission	
}

sealed abstract class UserPermission

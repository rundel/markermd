# Register S7 methods (e.g. the print methods for markermd_template,
# markermd_project, ...) with their generics when the installed package is
# loaded. Without this, S7 methods for base generics like print() only dispatch
# under devtools::load_all() (which registers them itself) and fall back to the
# default S7 output under a normal library() load.

.onLoad = function(libname, pkgname) {
  S7::methods_register()
}

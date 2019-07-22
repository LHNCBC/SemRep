#include <stdarg.h>             /* [PM] April 2000 before stdio.h fixes va_list issue on dec-osf */
#include <tcl.h>

int Tcl_AppInit(interp)
     Tcl_Interp *interp;
{
  /*
   * Call the init procedures for included packages.  Each call should
   * look like this:
   *
   * if (Mod_Init(interp) == TCL_ERROR) {
   *     return TCL_ERROR;
   * }
   *
   * where "Mod" is the name of the module.
   */

  /*
   * Call Tcl_CreateCommand for application-specific commands, if
   * they weren't already created by the init procedures called above.
   */
  
  /*
   * Specify a user-specific startup file to invoke if the application
   * is run interactively.  Typically the startup file is "~/.apprc"
   * where "app" is the name of the application.  If this line is deleted
   * then no user-specific startup file will be run under any conditions.
   */

  /*
  tcl_RcFileName = "~/.wishrc";
  */

  (void)interp;                 /* avoid -Wunused */

  return TCL_OK;
}

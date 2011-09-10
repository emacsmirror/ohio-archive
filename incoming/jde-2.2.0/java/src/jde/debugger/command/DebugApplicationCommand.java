/*
 * Copyright (c) 2000    Paul Kinnucan
 *
 * $Revision: 1.2 $
 */

package jde.debugger.command;
import java.util.List;
import jde.debugger.JDEException;
import jde.debugger.Jdebug;
import jde.debugger.Application;


/**
 * DebugApplicationCommand.java
 *
 *
 * Created: Fri Jan 28 21:58:06 2000
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.2 $
 */

abstract public class DebugApplicationCommand extends DebugCommand {
  
  public void init(Integer appID, Integer cmdID, 
		   String cmdName, List args) throws JDEException {
    super.init(appID, cmdID, cmdName, args);

    // this app id should be valid.  
    if (!Jdebug.theDebugger.appExists(appID)) 
	throw new JDEException("Application "+ appID + " does not exist");

    app = Jdebug.theDebugger.getApplication(appID);

  }

  Application app;
  
} // DebugApplicationCommand


/*
 * $Log: DebugApplicationCommand.java,v $
 * Revision 1.2  2000/03/03 07:40:32  paulk
 * Converted get_string and get_array commands from functions to objects.
 *
 * Revision 1.1  2000/01/31 12:46:10  paulk
 * Defines general behavior of application debug commands.
 *
 */

// End of DebugApplicationCommand.java

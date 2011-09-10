/*
 * Copyright (c) 2000    Paul Kinnucan
 *
 * $Revision: 1.3 $
 */

package jde.debugger.command;
import java.util.List;
import jde.debugger.JDEException;
import jde.debugger.Etc;
import jde.debugger.Jdebug;
import com.sun.jdi.connect.Connector;
import com.sun.jdi.Bootstrap;
import java.util.Iterator;


/**
 * DebugSessionCommand.java
 *
 *
 * Created: Fri Jan 28 21:59:32 2000
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.3 $
 */

abstract public class DebugSessionCommand extends DebugCommand {
  
  public DebugSessionCommand() { }

  public void init(Integer debuggerID, Integer cmdID, 
		   String cmdName, List args) throws JDEException {

    super.init(debuggerID, cmdID, cmdName, args);

    if (cmdName.equals("quit")) return;

    if (args.size() < 1 )
      throw new JDEException("Missing application ID");

    // the app id with which it will be known.
    // note that we remove the arguments as we consume them from the
    // list.
    appID = new Integer(Etc.safeGetint(args.remove(0), "application ID"));

    // the app id cannot be same as the debugger ID (-1)
    if (appID.equals(Jdebug.debuggerID)) {
      throw new JDEException("Invalid Application ID");
    }

    // an app using this id is already present!
    // XXX make sure you dispose the id once done with the app
 
    if (Jdebug.theDebugger.appExists(appID)) {
	throw new JDEException("Application ID is duplicate");
    }


  }

    /*
     * Gets a connector.
     *
     * @param type connector class name
     *
     */    
    protected final Connector getConnector(String name) {
        
        Iterator iter = connectors.iterator();
        while (iter.hasNext()) {
            Connector connector = (Connector)iter.next();
            if (connector.name().equals(name)) {
                return connector;
            }
        }
        return null;
    }

  static List connectors = Bootstrap.virtualMachineManager().allConnectors();

  
} // DebugSessionCommand


/*
 * $Log: DebugSessionCommand.java,v $
 * Revision 1.3  2000/02/02 06:01:14  paulk
 * Removed the get connector list code from getConnectors method and
 * instead made the connector list a static member that is initialized
 * once per session. Did this because it is suspected that getting the
 * connector list on the command thread was causing the debugger to hang
 * on some Windows/NT systems.
 *
 * Revision 1.2  2000/02/01 06:02:47  paulk
 * Added special handling for quit command.
 *
 * Revision 1.1  2000/01/30 12:37:44  paulk
 * Defines debug session commands (e.g., launch, attach, quit, etc.).
 *
 */

// End of DebugSessionCommand.java

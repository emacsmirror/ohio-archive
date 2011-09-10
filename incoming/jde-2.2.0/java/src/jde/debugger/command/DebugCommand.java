/*
 * Copyright (c) 2000    Paul Kinnucan
 *
 * $Revision: 1.1 $
 */

package jde.debugger.command;
import java.util.List;
import jde.debugger.JDEException;
import jde.debugger.Protocol;
import jde.debugger.Jdebug;
import jde.debugger.Debug;


/**
 * Class of debugger commands.
 *
 * Command-line syntax:
 *
 * app_id cmd_id cmd_name [arg]*
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.1 $
 */

abstract public class DebugCommand extends Thread 
  implements Protocol,Cloneable {
  
  public DebugCommand() {super("JDEbug command");}

  public void init(Integer appID, Integer cmdID, 
		   String cmdName, List args) throws JDEException {
    this.appID = appID;
    this.cmdID = cmdID;
    this.cmdName = cmdName;
    this.args = args;

    setName("JDEbug command(" + appID + " " + cmdID + " " + cmdName + ")");

  }

  abstract protected void doCommand() throws JDEException;


  public void run() {

    // see if there already is a command with this cmd_id. this
    // should never happen.
    if (Jdebug.theDebugger.pendingCmdExists(cmdID)) {
      Jdebug.theDebugger.signalCommandError(Jdebug.debuggerID, 
					    new Integer(-1), 
					    "Duplicate cmd_id '" + cmdID + "'");
      return;
    }

    // if not, add to pending commands.
    Jdebug.theDebugger.addPendingCommand(cmdID);
    

    try {
      doCommand();
    }
    catch (JDEException ex) {
      Debug.printIf(ex);
      // a jde exception was raised. the kind of error is already
      // in there.
      Jdebug.theDebugger.signalCommandError(Jdebug.debuggerID, 
					    cmdID, ex.getMessage());
      return;
    } 
    catch (Exception ex) {
      Debug.printIf(ex);
      Jdebug.theDebugger.signalCommandError(Jdebug.debuggerID, 
					    cmdID, "Unspecified error: "+ex.toString());
      return;
    } 
    finally {
      Jdebug.theDebugger.removePendingCommand(cmdID);
    }

  }

  abstract public Object clone();

  Integer appID;

  Integer cmdID;

  String cmdName;

  List args;

  
} // DebugCommand


/*
 * $Log: DebugCommand.java,v $
 * Revision 1.1  2000/01/30 12:31:51  paulk
 * Initial revision.
 *
 */

// End of DebugCommand.java

/*
 * Copyright (c) 2000    Paul Kinnucan
 *
 * $Revision: 1.1 $
 */

package jde.debugger.command;
import jde.debugger.JDEException;
import jde.debugger.Jdebug;


/**
 * Kills the debugger.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * quit
 * </pre>
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.1 $
 */
public class Quit extends DebugSessionCommand {
  
  protected void doCommand() throws JDEException {
    try {
      Jdebug.theDebugger.shutdown();
    } catch (Exception ex) {
      // do nothing
    }
    Jdebug.theDebugger.signalCommandResult(Jdebug.debuggerID, cmdID);
    System.exit(0);
   }

  public Object clone() {return new Quit();}
   
} // Quit


/*
 * $Log: Quit.java,v $
 * Revision 1.1  2000/01/31 12:47:01  paulk
 * Quit debugger.
 *
 */

// End of Quit.java

/*
 * Copyright (c) 2000    Paul Kinnucan
 *
 * $Revision: 1.1 $
 */

package jde.debugger.command;
import jde.debugger.JDEException;


/**
 * Runs an application.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * run
 * </pre>
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.1 $
 */
public class Run extends DebugApplicationCommand {
  
  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {

    try {
      app.getVM().resume();
    } catch (Exception ex) {
      throw new JDEException("Unspecified Error occured: "+ex.toString());
    }
    app.signalCommandResult(cmdID);
 
  }

  public Object clone() {return new Run();}
  
} // Run

/*
 * $Log: Run.java,v $
 * Revision 1.1  2000/01/31 12:48:02  paulk
 * Start or continue application.
 *
 */

// End of Run.java

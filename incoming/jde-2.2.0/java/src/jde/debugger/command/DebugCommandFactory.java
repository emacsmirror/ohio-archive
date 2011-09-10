/*
 * Copyright (c) 2000    Paul Kinnucan
 *
 * $Revision: 1.5 $
 */

package jde.debugger.command;

import java.util.HashMap;
import java.util.List;
import java.lang.InstantiationException;
import java.lang.ClassNotFoundException;
import java.lang.IllegalAccessException;
import jde.debugger.JDEException;



/**
 * DebugCommandFactory.java
 *
 *
 * Created: Fri Jan 28 22:04:57 2000
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.5 $
 */

public class DebugCommandFactory  {
 
  protected DebugCommandFactory() {

    prototypes.put("launch", new LaunchApplication());
    prototypes.put("attach_shmem", new AttachShmem());
    prototypes.put("attach_socket", new AttachSocket());
    prototypes.put("listen_shmem", new ListenShmem());
    prototypes.put("listen_socket", new ListenSocket());
    prototypes.put("quit", new Quit());
    prototypes.put("run", new Run());
    prototypes.put("get_string", new GetString());
    prototypes.put("get_array", new GetArray());
    prototypes.put("get_locals", new GetLocals());
    prototypes.put("get_this", new GetThis());
    prototypes.put("evaluate", new EvaluateExpression());
  }

  public final DebugCommand createCommand(Integer appID, Integer cmdID, 
					  String cmdName, List args)   
                            throws JDEException {

    DebugCommand prototype = (DebugCommand) prototypes.get(cmdName);

    if (prototype == null) return null;

    DebugCommand cmd = (DebugCommand) prototype.clone();
    cmd.init(appID, cmdID, cmdName, args);
    
    return cmd;

  }

  private HashMap prototypes = new HashMap();
 
  public static DebugCommandFactory theFactory = new DebugCommandFactory();
  
} // DebugCommandFactory


/*
 * $Log: DebugCommandFactory.java,v $
 * Revision 1.5  2000/07/28 06:27:02  paulk
 * Committing all modified files.
 *
 * Revision 1.4  2000/04/10 05:36:50  paulk
 * Added get_locals and get_this commands.
 *
 * Revision 1.3  2000/03/03 07:40:32  paulk
 * Converted get_string and get_array commands from functions to objects.
 *
 * Revision 1.2  2000/01/31 12:41:45  paulk
 * * Continue converting commands from functional to OO implementation.
 *
 * Revision 1.1  2000/01/30 12:35:20  paulk
 * Creates debugger commands.
 *
 */

// End of DebugCommandFactory.java

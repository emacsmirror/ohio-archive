/*
 * Copyright (c) 2000    Paul Kinnucan
 *
 * $Revision: 1.2 $
 */

package jde.debugger.command;
import jde.debugger.JDEException;
import com.sun.jdi.connect.ListeningConnector;
import java.util.Map;
import com.sun.jdi.connect.Connector;
import jde.debugger.Jdebug;
import com.sun.jdi.VirtualMachine;
import jde.debugger.Application;
import java.io.IOException;
import jde.debugger.Debug;
import com.sun.jdi.connect.IllegalConnectorArgumentsException;


/**
 *  Listens in shared memory for a debuggee vm requesting debug
 *  services.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * listen_shmem  app_id name
 * </pre>
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.2 $
 */
public class ListenShmem extends DebugSessionCommand {
  
  protected void doCommand() throws JDEException {

    if (args.size() < 1)
      throw new JDEException("Missing name");

    final String address = args.remove(0).toString();
    
    String connectSpec = "com.sun.jdi.SharedMemoryListen";

    final ListeningConnector connector = (ListeningConnector) getConnector(connectSpec);

    if (connector == null) 
      throw new JDEException("No such connector is available: "+connectSpec);
	
    Thread thread = new Thread("Listen on shared memory channel.") {

	public void run()  {

	  try {
	    Map argumentMap = connector.defaultArguments();

	    Connector.Argument nameArg =
	      (Connector.Argument)argumentMap.get("name");
	    nameArg.setValue(address);
	    
	    Jdebug.theDebugger.signalCommandResult(appID, cmdID);
	    Jdebug.theDebugger.signal(appID, 
				      MESSAGE, 
				      "Listening at shared memory address: " + address);
	    connector.startListening(argumentMap);
	    VirtualMachine vm = connector.accept(argumentMap);
	    connector.stopListening(argumentMap);

	    // note that new App might raise a jdeexception.
	    Application app = new Application(Jdebug.theDebugger, appID, vm);

 
	    if (Jdebug.theDebugger.appExists(appID)) {
		app.shutdown();
		app.signal(MESSAGE, "An application with the same ID already exists.");
	    }
	    
	    Jdebug.theDebugger.addApplication(appID, app);
	    
	   

	    app.signal(MESSAGE, "Attached VM (shmem) " + vm.description());
    
	    
	  } catch (IOException ex) {
	    Debug.printIf(ex);
	    Jdebug.theDebugger.signal(appID, 
				      MESSAGE, 
				      "I/O error occured while listening at shared memory address:" 
				      + address);
	  } catch (IllegalConnectorArgumentsException ex) {
	    Jdebug.theDebugger.signal(appID, 
				      MESSAGE, 
				      "Illegal argument error occurred while listening " + 
				      "at shared memory address: " + address);
	  }	    

	}
      };

    thread.start(); 	

  }

  public Object clone() {return new ListenShmem();}
  
} // ListenShmem


/*
 * $Log: ListenShmem.java,v $
 * Revision 1.2  2000/04/10 05:44:54  paulk
 * Corrected spelling error in message.
 *
 * Revision 1.1  2000/01/30 12:42:19  paulk
 * Defines command to attach debugger to an existing application through
 * shared memory.
 *
 */

// End of ListenShmem.java

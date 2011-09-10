package jde.debugger;

import com.sun.jdi.connect.Connector;
import com.sun.jdi.connect.LaunchingConnector;
import com.sun.jdi.Bootstrap;
import com.sun.jdi.VirtualMachine;
import com.sun.jdi.connect.VMStartException;
import com.sun.jdi.connect.IllegalConnectorArgumentsException;
import com.sun.jdi.connect.AttachingConnector;
import com.sun.jdi.connect.ListeningConnector;
import java.io.BufferedReader;
import java.io.PrintWriter;
import java.util.Map;
import java.util.Collection;
import java.io.IOException;
import java.io.StreamTokenizer;
import java.util.List;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Collections;
import java.util.ArrayList;
import java.util.Iterator;
import jde.debugger.command.DebugCommandFactory;

/**
 * Class of JDEbug debuggers.
 * <p>
 * This class defines methods for communicating with the JDE. It
 * maintains a list of active applications. It passes application
 * commands to the apps specified by the commands. 
 * <p>
 * See {@link Protocol Protocol class} for command/response formats and
 * {@link EventHandler EventHandler} for event set formats.
 * <p>
 * JDEbug responds to every command with either a result or error 
 * message.
 * <p>
 *
 * @author Amit Kumar
 * @since 0.1
 * @author Paul Kinnucan
 * @since 1.3
 */
public class Jdebug extends Thread implements Protocol {


  /********************************************************************
   * CONSTRUCTORS                                                     *
   ********************************************************************/

  protected Jdebug() {}

  public void init() throws IOException {
        
    // The debugger uses standard out to sent command responses, error
    // messages and event notifications to the JDE.
    out = new PrintWriter(System.out);

    // The debugger uses standard int to read commands from the JDE.
    in = new BufferedReader(new InputStreamReader(System.in));
	
    applications = Collections.synchronizedMap(new HashMap());

    pendingCommands = Collections.synchronizedSet(new HashSet());

  }

  /********************************************************************
   * METHODS                                                          *
   ********************************************************************/

  /**
   * Sets the syntax of the input stream. We want the input to be broken
   * into lines, with whitespaces separating tokens
   */
  private void setSyntax(StreamTokenizer st) {
    st.resetSyntax();
    st.eolIsSignificant(true);
    st.whitespaceChars('\u0000', '\u0020');
    st.wordChars('\u0021', '\u00ff');
    st.quoteChar('"');
  }	

  /**
   * Runs the debugger thread. This method reads and executes commands
   * from the JDE.
   */
  public void run() {
    
    // read till we get an error
    try {
      StreamTokenizer st = new StreamTokenizer(in);
      setSyntax(st);
	    
      int ttype;

      // read a line at a time
      ttype = st.nextToken();
      while (ttype != StreamTokenizer.TT_EOF) {

	try {
		    
	  List commandLine = new ArrayList();
	  while(ttype != StreamTokenizer.TT_EOL) {
			
	    if (ttype == StreamTokenizer.TT_EOF) {
	      throw new IOException("I/O EOF occured");
	    } else if (ttype == StreamTokenizer.TT_WORD) {
	      //Debug.printIf(Debug.JDE_PIPE, "word "+st.sval);
	      commandLine.add(st.sval);
	    } else if ((ttype == '"') || (ttype == '\'')) {
	      //Debug.printIf(Debug.JDE_PIPE, "phrase "+st.sval);
	      commandLine.add(st.sval);
	    } else {
	      // should be a char. see if we need to get ttype
	      // and convert it into a string instead of using
	      // tostring
	      //Debug.printIf(Debug.JDE_PIPE, "char "+st);
	      commandLine.add(String.valueOf((char)ttype));
	    }
		    
	    ttype = st.nextToken();

	  } // finished reading a line, or eof raised.
		    
	  //		    if (Debug.set(Debug.JDE_PIPE)) Etc.dump(commandLine);
		    
	  // we have the complete command. since we're really a
	  // private interface, we assume that the line is really
	  // well formed. if not, we don't raise an exception,
	  // just
	  // warn the jde side that we didn't understand the last
	  // command. just to be consistent, we will *not* send
	  // back
	  // the app_id and cmd_id back (they will both be -1)
	  // when
	  // we complain about malformed commands, even if
	  // one/both
	  // *were* available to us (ie were parsed correctly)

	  if (commandLine.size() < 3) {
	    if (commandLine.size() > 0) 
	      signalCommandError(debuggerID, new Integer(-1), "Malformed command");
	    ttype = st.nextToken();
	    // don't get irritated by simple newlines
	    continue;
	  }
		    
	  final Integer app_id =
	    Integer.valueOf(commandLine.get(0).toString());
	  final Integer cmd_id =
	    Integer.valueOf(commandLine.get(1).toString());
	  final String command = 
	    commandLine.get(2).toString().toLowerCase();
	  final List arguments = 
	    commandLine.subList(3, commandLine.size());
		    
	  Thread cmd = 
	    DebugCommandFactory.theFactory.createCommand(app_id, cmd_id, command, arguments);
	

	  if (cmd == null) {	  

	    cmd = new Thread("Executing ("+app_id+","+cmd_id+")") {
		public void run() {
		    handleAppCommand(app_id, cmd_id, command, arguments);
		}
	      };
	  }

	  cmd.start();
		    
	} catch (IOException ex) {
	  // if there is an ioexception, the connection was
	  // broken. fall through the loop, raising another
	  // exception as the outer loop tries to read a token
	  //      Debug.printIf(ex);
	} catch (Exception ex) {
	  // any other exception means the command wasn't good.
	  //		    Debug.printIf(ex);
	  // since we cannot be sure of app_id and cmd_id being
	  // good, we send back a general error.
	  signalCommandError(debuggerID, new Integer(-1), "Malformed command: "+ex.toString());
	}

	// read another token
	ttype = st.nextToken();
		    
      } // finished reading all input
		
    } 
    catch (IOException ex) {
      //	    Debug.printIf(ex);
    } 
    finally {
      // close down gracefully, shutting down all VMs
      // jdeexceptions might be raised: ignore them.
      try {
	shutdown();
      } catch (Exception ex) {
	// do nothing
      }
    }
  }


  /**
   * Commands received from Jdebug that don't have app_id == -1 get
   * funneled to handleAppCommand. We have to check if the app_id is
   * valid and if it corresponds to a valid app. If not, we send a
   * general error message back to jde, with debuggerID=1 and cmd_id=-1
   * since app_id is not valid anyway
   *
   * @param app_id The ID of the app this command targets
   * @param cmd_id ID of the command
   * @param command the command
   * @param arguments command arguments
   */
  private void handleAppCommand(Integer app_id, Integer cmd_id,
				String command, List arguments) {
    Application app;

    synchronized(applications) {
      // this app id should be valid. register using a connector
      // earlier
      if (!applications.containsKey(app_id)) {
	signalCommandError(debuggerID, cmd_id, "Application ID '"+app_id+"' does not exist");
	return;
      }
    }
    synchronized(applications) {
      // also, it should be a valid application
      if ((app = (Application)applications.get(app_id)) == null) {
	signalCommandError(debuggerID, cmd_id, "Application ID '"+app_id+"' does not correspond to a valid application");
      }
    }
    // at this point, we call the handleCommand function of the
    // app to take care of actions.
    app.handleCommand(cmd_id, command, arguments);
  }


  public void addApplication(Integer appID, Application app) {
    synchronized (applications) {
      applications.put(appID, app);
    }
  }

  /**
   * called by {@link Application#shutdown}
   * to remove it's own entry from
   * the applications collection here
   */
  public void removeApplication(Integer app_id) {
    synchronized (applications) {
      applications.remove(app_id);
    }
  }

  public boolean appExists(Integer appID) {
    synchronized (applications) {
      return applications.containsKey(appID);
    }
  }

  public Application getApplication(Integer appID) {
    synchronized (applications) {
      return (Application) applications.get(appID);
    }
  }



  public void addPendingCommand(Integer cmdID) {
    synchronized (pendingCommands) {
      pendingCommands.add(cmdID);
    }
  }

  public void removePendingCommand(Integer cmdID) {
    synchronized (pendingCommands) {
      pendingCommands.remove(cmdID);
    }
  }

  public boolean pendingCmdExists(Integer cmdID) {
    synchronized (pendingCommands) {
      return pendingCommands.contains(cmdID);
    }
  }


  /**
   * Shuts down all the applications prior to exiting
   */
  public void shutdown()
    throws JDEException {
    // run the "shutdown" function for all the apps currently being
    // debugged.
    synchronized (applications) {
      Iterator iterator = applications.values().iterator();
      while (iterator.hasNext()) {
	((Application)iterator.next()).shutdown();
	iterator.remove();
      }
    }
  }


    
  /*
   *
   * FUNCTIONS FOR SENDING INFORMATION OVER TO THE JDE SIDE
   *
   */




  /**
   * Returns a string representation of the object. Here is the logic:
   * <ul>
   * <li> If the object is null, return an empty string
   * <li> If the object is string, quote it within quotation marks.
   * <li> If the object is a list, recursively call stringRep, appending
   *      a space.
   * <li> If it's any other kind of object, return the .toString()
   * </ul>
   */
  public String stringRep(Object obj) {
    if (obj == null) {
      return "";
    } else if (obj instanceof String) {
      return "\""+obj.toString()+"\"";
    } else if (obj instanceof List) {
      StringBuffer returnString = new StringBuffer("");
      Iterator it = ((List)obj).iterator();
      while (it.hasNext()) {
	returnString.append(stringRep(it.next())+" ");
      }
      return  BR +returnString.toString().trim();
    } else {
      return obj.toString();
    }
  }


  /**
   * Send an arbitrary lisp function across.
   *
   * @param app_id The application ID
   * @param type The function name. JDE_BUG gets added to
   * its beginning
   * @param obj An arbitrary object. If a string, it's just printed out,
   * if a list, each of its elements is printed out, with a space after
   * each.
   */
  synchronized public void signal(Integer app_id, String type,
				  Object obj) {
    String strRep = stringRep(obj);
    if (strRep.equals("")) {
      out.println(BR+"("+JDE_BUG+type+" "+app_id+")"+BR);
    } else {
      String temp="("+JDE_BUG+type+ BR +app_id+" "+stringRep(obj)+")";
      if (temp.length() <= 80)
	out.println(BR+"("+JDE_BUG+type+" "+app_id+" "+
		    stringRep(obj)+")"+BR);
      else
	out.println( BR +temp+ BR );
    }
    out.flush();
  }

  /**
   * Signal a reply: a result or an error
   */
  synchronized private void signalReply(Integer app_id, Integer cmd_id,
					Object obj, String type) {
    String strRep = stringRep(obj);
    if (strRep.equals("")) {
      out.println(BR+"("+type+" "+cmd_id+")"+BR);
    } else {
      String temp = "("+type+ BR +cmd_id+" "+strRep+")";
      if (temp.length() <= 80)
	out.println(BR+"("+type+" "+cmd_id+" "+strRep+")"+BR);
      else 
	out.println( BR +temp+ BR );
    }	    
    out.flush();
  }
    

  /**
   * send the result of a command. indicates a positive completion of
   * the command. this could of course be provisional: eg. in case of
   * provisional breakpoints
   */
  public void signalCommandResult(Integer app_id, Integer cmd_id) {
    signalCommandResult(app_id, cmd_id, null);
  }

  public void signalDebug(String msg) {
    signal(debuggerID, MESSAGE, msg);
  } 

  /**
   *
   * the result of a command. if it's a string, just send it across, else
   * it should be a list. each element is sent across, just doing a
   * toString() -> ie, if you want to send (... 23 "some string" 45), you
   * need to put in the quotes (ie "") yourself: else what will be sent
   * will be (... 23 some string 45), obviously wrong.
   */
  synchronized public void signalCommandResult(Integer app_id,
					       Integer cmd_id, Object obj) {
    signalReply(app_id, cmd_id, obj, COMMAND_RESULT);
  }

    
  /**
   * reply to a command with an error. 
   * @param obj Is usually a string explaining what went wrong.
   */
  synchronized public void signalCommandError(Integer app_id,
					      Integer cmd_id, Object obj) {
    signalReply(app_id, cmd_id, obj, COMMAND_ERROR);
  }


  /********************************************************************
   * FIELDS                                                           *
   ********************************************************************/

  /*
   * Reads commands from the JDE.
   */
  BufferedReader in;

  /*
   * Writes command responses, messages, and event notifications to 
   * the JDE.
   */
  PrintWriter out;

  /**
   * Registry of active applications. The registry maps each application
   * to its ID. The app removes itself from the registry once it's finished.
   */
  Map applications;

  /**
   * Each command has a command id associated with it, that is used by
   * jde, to match with the corresponding result/error. jdebug maintains
   * the pending command ids in this collection (as does {@link
   * Application}, see
   * {@link Application#pendingCommands}),
   * and removes
   * them once the command processing is over.<br>
   * Hence, the command id can actually be reused.
   * <p>
   * Note that in case the command is actually meant for one of the apps
   * (ie as ascertained by matching the app_id), the id isn't stored in
   * our Collection, but the applications. This means that, in fact, it
   * is the tuple (app_id, cmd_id) that need be unique (assuming that a
   * "general" command has a app_id of -1
   */
  private Collection pendingCommands;

  /**
   * The ID of jdebug. This is used by jde when issuing commands that
   * are not specific to any particular vm, for instance, 'quit', or
   * the command used to launch new application/vms.<br>
   * It is the Integer -1.
   */
  public static final Integer debuggerID = new Integer(-1); 

  public static Jdebug theDebugger = new Jdebug();


} // Jdebug

/*
 * $Log: Jdebug.java,v $
 * Revision 1.7  2000/07/28 06:26:31  paulk
 * Committing all modified files.
 *
 * Revision 1.6  2000/02/14 06:25:34  paulk
 * Implemented workaround for JPDA bug that prevented setting of
 * breakpoints in inner classes.
 *
 * Revision 1.5  2000/01/31 12:41:39  paulk
 * * Continue converting commands from functional to OO implementation.
 *
 * Revision 1.4  2000/01/30 12:47:40  paulk
 * Changed to a singleton class. Implemented support for object-oriented
 * commands created by DebugCommandFactory. Reimplemented launch and
 * listen commands as object-oriented commands.
 *
 * Revision 1.3  2000/01/28 04:24:55  paulk
 * Threaded listen commands. Moved launch, attach, and listen commands
 * from Application to Jdebug class. Did general cleanup of Jdebug and
 * Application class, including using more specific names for some
 * variables, moving fields to the end of files, rewriting comments.
 *
 */


// End of Jdebug.java

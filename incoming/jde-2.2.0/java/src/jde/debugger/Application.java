package jde.debugger;

import com.sun.jdi.ReferenceType;
import com.sun.jdi.VirtualMachine;
import com.sun.jdi.event.EventQueue;
import com.sun.jdi.request.ClassPrepareRequest;
import com.sun.jdi.request.EventRequest;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import jde.debugger.ObjectStore;
import jde.debugger.spec.BreakpointSpec;
import jde.debugger.spec.EventRequestSpec;
import jde.debugger.spec.EventRequestSpecList;
import jde.debugger.spec.ExceptionSpec;
import jde.debugger.spec.WatchpointSpec;
import java.io.BufferedWriter;
import java.io.OutputStreamWriter;


/**
 * Class of debuggee applications.
 *
 * @author Amit Kumar
 * @since 0.1
 * @author Paul Kinnucan
 * @since 1.5
 */
public class Application implements Protocol {

     
  /********************************************************************
   * CONSTRUCTORS                                                     *
   ********************************************************************/

  /**
   * Creates an instance of an application object.
   *
   * @param jdebug {@link Jdebug} class
   * @param appID Identifier used to specify this app int commands.
   * @param vm Virtual machine in which this application is running.
   */
  public Application(Jdebug jdebug, Integer appID, VirtualMachine vm) {
    
    this.jdebug = jdebug;
    
    this.appID = appID;

    this.vm = vm;

    pendingCommands = Collections.synchronizedSet(new HashSet());

    store = new ObjectStore(this);

    eventHandler = new EventHandler(this);

    eventRequestSpecs = new EventRequestSpecList(this);

    // we need to raise all class prepare events to 
    // make sure we resolve the corresponding specs.
    ClassPrepareRequest cprequest =
      vm.eventRequestManager().createClassPrepareRequest();

    // this (hack?) is used to identify if the user itself specified
    // a class prepare request, or the event was raised because of
    // this request.
    cprequest.putProperty("default", "default");
    cprequest.setSuspendPolicy(EventRequest.SUSPEND_ALL);
    cprequest.enable();

    // set up command handling classes.
    generalCommands =
      new GeneralCommands(this, store);
    specCommands =
      new SpecCommands(this,eventRequestSpecs,store);
    threadCommands =
      new ThreadCommands(this, store);

  }

  /********************************************************************
   * METHODS                                                          *
   ********************************************************************/

  /*
   * Gets the virtual machine that is running this application.
   * @return Application running this vm.
   */
  public final VirtualMachine getVM() { return vm; }

  /*
   * Gets the ID used by commands to specify this application.
   *
   * @return application ID
   */
  public final Integer getId() { return appID; }

  /*
   * Generates an ID used by commands to reference objects.
   * 
   * @return new object ID
   */
  public final Long generateObjectID() {
    // synchronize on any static object
    synchronized (Jdebug.debuggerID) {
      return new Long(objIdCounter++);
    }
  }

  public final ObjectStore getStore() { return store; }

  public final EventQueue getEventQueue() { return vm.eventQueue(); }


  public final void shutdown() {
    // we need to be a little considerate to the output. given
    // there are all kinds of threads running, we make sure we
    // wait for all the output to complete.

    shuttingDown = true;
	
    // isolate the process first
    Process process = null;
    if (vm != null) 
      process = vm.process();
    try {
      if (vm != null) {
	vm.dispose();
	vm = null;
	eventHandler.shutdown();
      }
      sioSocket.close();
    } catch (Exception ex) {
      // do nothing
    } finally {
      if (process != null) {
	process.destroy();
	// XXX sun's jdb implementation works a lot to make sure
	// the stderr and stdout are dumped before killing
	// things. i'm not sure how important it is, or even how
	// well it works (given i can't test it)
	// sooo, if the reader finds bugs with the output handling
	// on finish, lemme know.
      }
      jdebug.removeApplication(appID);
    }
  }

    
    
  /*
   * FUNCTIONS TO MANAGE Standard IO STREAMS
   */   

 
  /**
   * Launches a thread to connect the Emacs standard I/O buffer
   * for the current process to the standard I/O of the process.
   *
   * <p>
   * This method creates a socket for the standard I/O connection.
   * The thread waits for Emacs to connect to the standard I/O socket.
   *
   * @return Address of standard I/O socket.
   * @exception JDEException if an error occurs
   */
  public int initSIOConnect() throws JDEException {

    ServerSocket ss = null;
    try {
      ss = new ServerSocket(0);
    } catch (IOException ex) {
      throw new JDEException("Unable to create a server socket");
    }

    final ServerSocket sstmp = ss;
    final int port = ss.getLocalPort();

    standardIOConnectThread = new Thread("Standard I/O Thread for App #"+appID) {
	public void run() {
	  try {
	    signal(MESSAGE, "Debugger waiting for Emacs to connect to app SIO port " +
		   port + ".");
	    sioSocket = sstmp.accept();
	    sstmp.close();
	  } catch (IOException ex) {
	    signal(ERROR, "IOException occurred while connecting to app SIO port " + port +
		   ". Shutting down...");
	    shutdown();
	  }
	}
      };

    standardIOConnectThread.start();
    return port;

  }

  /**
   * Describe <code>initSIOTransport</code> method here.
   *
   */
  public void initSIOTransport() {

    try {
      standardIOConnectThread.join();
      signal(MESSAGE, "Debugger connected to standard I/O socket.");

      

      final Process process = vm.process();
      standardInputProcessor = new StandardInputProcessor(process.getOutputStream());
      standardInputProcessor.start();

      standardOutputWriter = new StandardOutputWriter(sioSocket);

      standardOutputProcessor = new StandardOutputProcessor(process.getInputStream());
      standardOutputProcessor.start();

      standardErrorProcessor = new StandardErrorProcessor(process.getErrorStream());
      standardErrorProcessor.start();

    }
    catch (InterruptedException ex1) {
      signal(ERROR, "Debugger interrupted while waiting for Emacs to connect to " +
	     "standard I/O socket.");
    }

  }

  /**
   * Reads standard input from Emacs and forwards it to the application.
   *
   * @author Paul Kinnucan <paulk@mathworks.com>
   * @version 1.0
   * @since 1.0
   * @see Thread
   */
  private class StandardInputProcessor extends Thread {

    public StandardInputProcessor(final OutputStream toVmStream) {
      super("Input Processor for App #"+appID);
      
      toVM = new PrintStream(toVmStream, true);

      try {
	fromEmacs =
	  new BufferedReader(new InputStreamReader(sioSocket.getInputStream()));
      }
      catch (IOException ex1) {
	signal(ERROR, "Could not get standard input stream from Emacs.");
      }
	
      setPriority(Thread.MAX_PRIORITY-1);

    }

    public void run() {

      if (fromEmacs == null) return;
      
      try {
	String line;
	while ((line = fromEmacs.readLine()) != null) {
	  toVM.println(line);
	  toVM.flush();
	}

	if (!Application.this.isShuttingDown()) {
	  try {
	    // sioSocket.close();
	    signal(MESSAGE, "Application closed its standard input.");
	  } catch (Exception ex) {
	    signal(MESSAGE, "Couldn't close socket to standard input.");
	  }
	}
			
      } catch (IOException ex) {
	if (!Application.this.isShuttingDown()) {
	  try {
	    // sioSocket.close();
	    signal(ERROR, "Input error; application I/O closed");
	  } catch (Exception e) {
	    signal(ERROR, "Input error; couldn't close application I/O");
	  }
	}
      }
    }

    PrintStream toVM;
    BufferedReader fromEmacs;

  }


  /**
   * Writes a line to the socket connected to the
   * standard I/O buffer maintained by Emacs for this
   * application.
   *
   * <p>This class is used by the StandardOutputProcessor
   * and StandardErrorProcessor to forward the application's
   * standard ouput and error output to Emacs.
   *
   * @author "" <paulk@mathworks.com>
   * @version 1.0
   * @since 1.0
   */
  private class StandardOutputWriter {

    public StandardOutputWriter(Socket sioSocket) {
      if (sioSocket == null) {
	signal(ERROR, "Could not transport app output. " +
	       "Transport socket does not exist.");
	return;   
      }

      OutputStream toEmacsStream;

      try {
	toEmacsStream = sioSocket.getOutputStream();
	if (toEmacsStream == null) {
	  signal(ERROR, "Could not transport app output. Transport socket closed.");
	  return;
	}
      }
      catch (IOException ex1) {
	signal(ERROR, "Could not transport app output. Transport socket closed.");
	return;
      }

      toEmacs = new BufferedWriter(new OutputStreamWriter(toEmacsStream));
    }
    
    public void write(char[] cbuf, int len) {
      if (toEmacs != null) {
	try {
	  toEmacs.write(cbuf, 0, len);
	  toEmacs.flush();
	}
	catch (IOException ex1) {
	  signal(ERROR, "I/O error: could not pipe application output to Emacs.");
	}
      }
    }

    BufferedWriter toEmacs;
  }


  /**
   * Forwards the application's standard output to Emacs.
   *
   * @author Paul Kinnucan <paulk@mathworks.com>
   * @version 1.0
   * @since 1.0
   * @see Thread
   */
  private class StandardOutputProcessor extends Thread {

   public StandardOutputProcessor(InputStream fromVMStream) {
      fromVM = new BufferedReader(new InputStreamReader(fromVMStream));
      setPriority(Thread.MAX_PRIORITY-1);
    }

    public void run() {

      String line;

      try {
	char[] cbuf = new char[256];
	int len;
	while ((len = fromVM.read(cbuf, 0, 256)) != -1) {
	  synchronized (standardOutputWriter) {
	    if (standardOutputWriter != null) {
	      standardOutputWriter.write(cbuf, len);      
	    } // end of if ()    
	  }
	}
      }
      catch (IOException ex) {
      }

      if (!Application.this.isShuttingDown()) {
	try {
	  // sioSocket.close();
	  signal(MESSAGE, "Closed transport for application's standard output.");
	} catch (Exception ex) {
	  signal(ERROR, "Could not close application standard output transport.");
	}
      }
    }

    BufferedReader fromVM;

  }


  /**
   * Forwards the application's error output to Emacs.
   *
   * @author Paulk Kinnucan <paulk@mathworks.com>
   * @version 1.0
   * @since 1.0
   * @see Thread
   */
  private class StandardErrorProcessor extends Thread {

    public StandardErrorProcessor(InputStream fromVMStream) {
      super("Standard Error Processor for App #" + appID);
      fromVM = new BufferedReader(new InputStreamReader(fromVMStream));
      setPriority(Thread.MAX_PRIORITY-1);
    }

    public void run() {

      String line;

      try {
	char[] cbuf = new char[256];
	int len;
	while ((len = fromVM.read(cbuf, 0, 256)) != -1) {
	  synchronized (standardOutputWriter) {
	    if (standardOutputWriter != null) {
	      standardOutputWriter.write(cbuf, len);      
	    } // end of if ()      
	  }
	}
      }
      catch (IOException ex) {
      }

      if (!Application.this.isShuttingDown()) {
	try {
	  // sioSocket.close();
	  signal(MESSAGE, "Closed transport for application's standard error output.");
	} catch (Exception ex) {
	  signal(ERROR, "Could not close application standard error output transport.");
	}
      }
    }

    BufferedReader fromVM;

  }

    
  /*
   *
   * USEFUL FUNCTIONS
   *
   */


    
  /**
   * Return a list of ReferenceType objects for all
   * currently loaded classes and interfaces whose name
   * matches the given pattern.  The pattern syntax is
   * open to some future revision, but currently consists
   * of a fully-qualified class name in which the first
   * component may optionally be a "*" character, designating
   * an arbitrary prefix.
   */
  public List findClassesMatchingPattern(String pattern)
    throws JDEException {
    if (vm == null) return null;
    List result = new ArrayList();  //### Is default size OK?
    if (pattern.startsWith("*.")) {
      // Wildcard matches any leading package name.
      pattern = pattern.substring(1);
      List classes = vm.allClasses();
      Iterator iter = classes.iterator();
      while (iter.hasNext()) {
	ReferenceType type = ((ReferenceType)iter.next());
	if (type.name().endsWith(pattern)) {
	  result.add(type);
	}
      }
      return result;
    } else {
      // It's a class name.
      return vm.classesByName(pattern);
    }
  }



  /*
   *
   * HANDLING JDE COMMANDS
   *
   */
    


  /**
   * Primary function called by {@link Jdebug} when a command specific to
   * a vm/app is received from jde.
   *
   * @param cmd_id Identifies a command uniquely. See {@link
   * Jdebug#pendingCommands}
   * @param command The command
   * @param args And its arguments
   */
  public void handleCommand(Integer cmd_id, String command, List args) {

    synchronized (pendingCommands) {
      // first check if we already got a command with this cmd_id that
      // we haven't handled yet.
      if (pendingCommands.contains(cmd_id)) {
	// return an "invalid" command id to indicate that the 
	// command id itself is bad
	signalCommandError(new Integer(-1), "Duplicate command id '"+cmd_id+"'");
      }
    }

    try {
      command = command.toLowerCase();
      // finish up
      if (command.equals(FINISH)) {
	generalCommands.doFinish(cmd_id, args);
      }
     // get info about an object
      else if (command.equals(GET_OBJECT)) {
	generalCommands.doGetObject(cmd_id, args);
      }
      else if (command.equals(GET_LOADED_CLASSES)) {
	generalCommands.doGetLoadedClasses(cmd_id, args);
      }
      // get path information
      else if (command.equals(GET_PATH_INFORMATION)) {
	generalCommands.doGetPathInformation(cmd_id, args);
      }
      // get class preparation notification
      else if (command.equals(TRACE_CLASSES)) {
	generalCommands.doTraceClasses(cmd_id, args);
      }
      // cancel one of the above
      else if (command.equals(CANCEL_TRACE_CLASSES)) {
	generalCommands.doCancelTraceClasses(cmd_id, args);
      }
      // exceptions...
      else if (command.equals(TRACE_EXCEPTIONS)) {
	specCommands.doTraceExceptions(cmd_id, args);
      }
      // watchpoints...
      else if (command.equals(WATCH)) {
	specCommands.doWatch(cmd_id, args);
      }
      // breakpoint commands
      else if (command.equals(BREAK)) {
	specCommands.doBreak(cmd_id, args);
      }
      // clear a breakpoint
      else if (command.equals(CLEAR)) {
	specCommands.doClear(cmd_id, args);
      }
      // do a step
      else if (command.equals(STEP)) {
	specCommands.doStep(cmd_id, args);
      }
      // request for tracing methods
      else if (command.equals(TRACE_METHODS)) {
	generalCommands.doTraceMethods(cmd_id, args);
      }
      // cancel a tracing request
      else if (command.equals(CANCEL_TRACE_METHODS)) {
	generalCommands.doCancelTraceMethods(cmd_id, args);
      }
      // suspend a thread, threadgroup or class
      else if (command.equals(SUSPEND)) {
	threadCommands.doSuspend(cmd_id, args);
      }
      // resume a thread, threadgroup or class
      else if (command.equals(RESUME)) {
	threadCommands.doResume(cmd_id, args);
      }
      // interrupt a thread 
      else if (command.equals(INTERRUPT)) {
	threadCommands.doInterrupt(cmd_id, args);
      }
      // kill a thread using a throwable object
      else if (command.equals(KILL_THREAD)) {
	threadCommands.doKillThread(cmd_id, args);
      }
      // get all threads
      else if (command.equals(GET_THREADS)) {
	threadCommands.doGetThreads(cmd_id, args);
      }
      // get detailed info on one thread
      else if (command.equals(GET_THREAD)) {
	threadCommands.doGetThread(cmd_id, args);
      }
      // get info about an object
      else if (command.equals(GET_OBJECT_MONITORS)) {
	threadCommands.doGetObjectMonitors(cmd_id, args);
      }
      // trace thread start/deaths
      else if (command.equals(TRACE_THREADS)) {
	threadCommands.doTraceThreads(cmd_id, args);
      }
      // cancel a thread start/death trace request
      else if (command.equals(CANCEL_TRACE_THREADS)) {
	threadCommands.doCancelTraceThreads(cmd_id, args);
      }
      // if it's not supported
      else {
	signalCommandError(cmd_id, "'"+command+"' is not supported");
      }
    } catch (JDEException ex) {
      // a jde exception was raised. the kind of error is already
      // in there.
      Debug.printIf(ex);
      signalCommandError(cmd_id, ex);
      return;
    } catch (Exception ex) {
      Debug.printIf(ex);
      signalCommandError(cmd_id, "An unspecified error occured: '"+ex.toString()+"'");
      return;
    } finally {
      synchronized (pendingCommands) {
	// once the command is done, remove it from the pending
	// commands list. if an exception is raised, it should be
	// removed from the pending list there.
	pendingCommands.remove(cmd_id);
      }
    }
  }
    

  /* spec related functions */

    
  /**
   * This method is executed whenever a new reference type is prepared.
   * If any outstanding specs match, they get resolved in the process
   *
   * @see EventRequestSpecList#resolve(ReferenceType)
   */
  public void resolve(ReferenceType ref) {
    eventRequestSpecs.resolve(ref);
  }

  /**
   * Inform jde on a successful spec resolution
   */
  public void informJDEInstallSuccessful(EventRequestSpec spec) {
    signal(SPEC_RESOLVED, spec.getID());
  }
    
  /**
   * Removes a Spec from the specList, and informs jde.
   * If there is an error while resolving a spec, indicating that it
   * cannot be resolved (ie even at a later time when more classes are
   * prepared), this function is called to remove it from the list, and
   * inform the jde about this error
   */
  public void removeSpecAndInformJDE(EventRequestSpec spec, String problem) {
    if (spec instanceof BreakpointSpec) {
      signal(INVALID+BREAK, new LispForm(spec.getID()
					 +" \""+problem+"\""));
    } else if (spec instanceof WatchpointSpec) {
      signal(INVALID+WATCH, new LispForm(spec.getID()
					 +" \""+problem+"\""));
    } else if (spec instanceof ExceptionSpec) {
      signal(INVALID+TRACE_EXCEPTIONS,
	     new LispForm(spec.getID()+" \""+problem+"\""));
    }
    eventRequestSpecs.delete(spec);
  }

	    
  /* shortcut to jde output commands */

  public void signal(String type, Object obj){
    jdebug.signal(appID, type, obj);
  }

  public void signalCommandResult(Integer cmd_id) {
    jdebug.signalCommandResult(appID, cmd_id);
  }

  public void signalCommandResult(Integer cmdID, Object obj) {
    jdebug.signalCommandResult(appID, cmdID, obj);
  }

  public void signalCommandError(Integer cmdID, String error) {
    jdebug.signalCommandError(appID, cmdID, error);
  }

  /**
   * if a jde exception was caused somewhere (causing an error to be sent
   * back for the command), we can use the string in the exception to
   * create the error string
   */
  public void signalCommandError(Integer cmdID, JDEException ex) {
    jdebug.signalCommandError(appID, cmdID, ex.getMessage());
  }


  /********************************************************************
   * FIELDS                                                           *
   ********************************************************************/

  /** The ID that uniquely identifies this application in jdebug. */
  final Integer appID;	

  /** Represents the jde. {@link Jdebug} manages the jdebug link to jde. */
  final Jdebug jdebug;

  /** Socket connection to do i/o */
  Socket sioSocket = null;


  /**
   * The {@link EventHandler} manages the events received from the
   * debugee vm
   */
  EventHandler eventHandler;

  /** The virtual machine that is running this application. */
  VirtualMachine vm;	        

  /**
   * {@link jde.debugger.spec.EventRequestSpecList} is responsible for
   * keeping track of the events that the user is interested in. To do
   * this, it maintains a list of "eventRequestSpec"s.
   *
   * @see jde.debugger.spec.EventRequestSpecList
   * @see jde.debugger.spec.EventRequestSpec
   */
  EventRequestSpecList eventRequestSpecs;

  /**
   * A collection of the commands for which the reply (result/error) has
   * not been sent.
   * @see Jdebug#pendingCommands
   */
  Collection pendingCommands;

  /**
   * A store of all the objects jde knows about.
   * @see ObjectStore
   */
  ObjectStore store;

  /**
   * Some classes require a unique ID with which to refer to objects
   * they are tracking: for instance eventRequestSpecs, which need a spec
   * ID with which to identify the specs, and identifiableSpecRequests
   * (in ApplicationCommands)
   * <p>
   * This variable keeps a monotonically increasing count, and can be
   * used to generate a new id, using {@link #generateObjectID}
   */
  private long objIdCounter = 0;


  /* Command handlers */
  GeneralCommands generalCommands;
  SpecCommands specCommands;
  ThreadCommands threadCommands;

  /**
   * keeps track of the state of the application: exceptions/error messages
   * will not be raised if we're shutting down.
   */
  private boolean shuttingDown = false;
  private boolean isShuttingDown() { return shuttingDown; }


  Thread standardIOConnectThread;
  StandardInputProcessor standardInputProcessor;
  StandardOutputProcessor standardOutputProcessor;
  StandardErrorProcessor standardErrorProcessor;
  StandardOutputWriter standardOutputWriter;


     
} // Application


/*
 * $Log: Application.java,v $
 * Revision 1.11  2000/07/28 06:26:30  paulk
 * Committing all modified files.
 *
 * Revision 1.10  2000/07/14 00:46:56  paulk
 * Standard output from an application no longer has to end in a newline character to be transported to Emacs.
 *
 * Revision 1.9  2000/04/10 05:52:30  paulk
 * Removed functional version of get_locals command.
 *
 * Revision 1.8  2000/04/05 04:55:11  paulk
 * Added app I/O socket handshaking messages to provide finer grain monitoring of process launch.
 *
 * Revision 1.7  2000/03/03 07:40:01  paulk
 * Converted get_string and get_array commands from functions to objects.
 *
 * Revision 1.6  2000/01/31 12:41:39  paulk
 * * Continue converting commands from functional to OO implementation.
 *
 * Revision 1.5  2000/01/28 04:24:55  paulk
 * Threaded listen commands. Moved launch, attach, and listen commands
 * from Application to Jdebug class. Did general cleanup of Jdebug and
 * Application class, including using more specific names for some
 * variables, moving fields to the end of files, rewriting comments.
 *
 */


// End of Application.java

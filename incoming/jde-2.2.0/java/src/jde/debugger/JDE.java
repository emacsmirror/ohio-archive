/*
 *  $Revision: 1.2 $
 *
 *  Copyright (C) 1999 Free Software Foundation, Inc.
 *
 *  Author: Amit Kumar <amit@usc.edu>
 *  Maintainer: Paul Kinnucan <paulk@mathworks.com>
 *
 *  Sun Microsystems, Inc. sponsored initial development of this code.
 *
 *  This code is distributed with the Java Development Environment for
 *  Emacs (JDE). The latest version of the JDE is available at
 *  http://sunsite.auc.dk/jde/.
 *
 *  This code is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  This code is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Library General Public
 *  License as published by the Free Software Foundation.
 *
 *  You should have received a copy of the GNU Library General Public
 *  License along with this code; if not, write to the Free
 *  Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

package jde.debugger;

import java.io.*;
import java.net.*;
import java.util.*;

/**
 * JDE.java
 * <p>
 * This class is the main link between jdebug and jde. It has methods that
 * allow communication between jde and jdebug, as well as a store
 * of all the applications currently active. Commands that have valid app_ids
 * interpret get passed on to the corresponding applications.
 * If not, corresponding errors are sent on the
 * pipe.
 * <p>
 * Look {@link Protocol here} for the command/response formats, and
 * {@link EventHandler here} for information about handling of events,
 * including event set formats
 * <p>
 * Note that for each command sent by jde, there *has* to be a reply. (either
 * a result or an error) For example, in
 * case we're launching a thread, we should return true, even if the thread
 * might bomb later on.
 * <p>
 *
 * <b> Handshake </b>
 *
 * Since JDI supports different kinds of connections, (for
 * example, it being possible to "launch" a debugger once an
 * uncaught exception is raised), we should probably have some
 * sort of a handshake mechanism preceding a debugging session,
 * for each new vm.
 * <p>
 * For example, if the debugee vm is already running, during the
 * handshake we might specify its address. On the other hand, if
 * we want the debugger to *launch* a vm, we can do that during
 * the handshake.
 * <p>
 * For our purposes, we will only support "launch" in the
 * beginning.
 * <p>
 * The purpose of the handshake is to establish the
 * debugee_vm_id <-> debugee VM binding. It is guaranteed that no
 * events will be raised from jdebug for a debugee VM for which
 * the handshake is not yet complete.
 * <p>
 *
 * Created: Wed Jul  7 20:49:16 1999
 *
 * @author Amit Kumar
 * @since 0.1
 */

public class JDE extends Thread implements Protocol {

    BufferedReader in;
    PrintWriter out;

    /**
     * All the active applications are kept in a map of
     * <code>app_id -> app</code>. 
     * The app_id uniquely idenifies the application: it's the duty
     * of the app to nicely remove itself from the map once it's finished
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
    public static final Integer my_id = new Integer(-1); 


    public JDE() throws IOException {

	//out = new PrintWriter(new BufferedWriter(new OutputStreamWriter(socket.getOutputStream())));
	//in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
	out = new PrintWriter(System.out);
	in = new BufferedReader(new InputStreamReader(System.in));
	
	applications = Collections.synchronizedMap(new HashMap());
	pendingCommands = Collections.synchronizedSet(new HashSet());
    }

    /**
     * Sets the syntax of the input stream. We want the input to be broken
     * into lines, with whitespaces separating tokens
     */
    private void setSyntax(StreamTokenizer st) {
	/*
	st.resetSyntax();
	st.eolIsSignificant(true);
	st.quoteChar('"');
	st.whitespaceChars('\u0000', '\u0020');
	st.wordChars('.', '.');	// for class names and all
	st.wordChars('-', '-');	// for negative numbers...
	st.wordChars('0', '9');
	st.wordChars('A', 'Z');
	st.wordChars('a', 'z');
	st.wordChars('\u00A0', '\u00FF');
	*/
	st.resetSyntax();
	st.eolIsSignificant(true);
	st.whitespaceChars('\u0000', '\u0020');
	st.wordChars('\u0021', '\u00ff');
	st.quoteChar('"');
    }	

    /**
     * JDE is a thread: it reads a line at a time and executes the command
     * therein
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
			ttype = st.nextToken();
			// don't get irritated by simple newlines
			if (commandLine.size() > 1) 
			    signalCommandError(my_id, new Integer(-1), "Malformed command");
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
		    
		    // by now all of the above variables are properly set

		    Thread thread = new
			Thread("Executing ("+app_id+","+cmd_id+")") {
			    public void run() {
				// a general command has app_id = -1
				if (app_id.equals(my_id)) {
				    handleGeneralCommand
					(cmd_id, command, arguments);
				}
				// otherwise we find which app is targetted
				else {
				    handleAppCommand
					(app_id, cmd_id, command, arguments);
				}
			    }
			};
		    thread.start();
		    
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
		    signalCommandError(my_id, new Integer(-1), "Malformed command: "+ex.toString());
		}

		// read another token
		ttype = st.nextToken();
		    
	    } // finished reading all input
		
	} catch (IOException ex) {
	    //	    Debug.printIf(ex);
	} finally {
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
     * Commands that are not meant for any particular app/vm are funneled
     * to this method.
     *
     * @param cmd_id The ID of the command sent. The app_id was -1
     * @param command The command itself
     * @param arguments And the arguments of the command
     */
    private void handleGeneralCommand(Integer cmd_id, String command,
				      List arguments) {
	synchronized (pendingCommands) {
	    // see if there already is a command with this cmd_id. this
	    // should never happen.
	    if (pendingCommands.contains(cmd_id)) {
		signalCommandError(my_id, new Integer(-1), "Duplicate cmd_id '"+cmd_id+"'");
		return;
	    }

	    // if not, add to pending commands.
	    pendingCommands.add(cmd_id);
	}
	
	// any parse errors will raise an exception
	try {
	    // a new app is to be launched
	    if (command.equals(LAUNCH)) {
		doLaunch(cmd_id, arguments);
	    }
	    // attach to existing app
	    else if (command.equals(ATTACH_SOCKET)) {
		doAttach(command, cmd_id, arguments);
	    }
	    // attach to existing app
	    else if (command.equals(ATTACH_SHMEM)) {
		doAttach(command, cmd_id, arguments);
	    }
	    // listen to existing app
	    else if (command.equals(LISTEN_SOCKET)) {
		doListen(command, cmd_id, arguments);
	    }
	    // listen to existing app
	    else if (command.equals(LISTEN_SHMEM)) {
		doListen(command, cmd_id, arguments);
	    }
	    // quitting jdebug
	    else if (command.equals(QUIT) || command.equals(EXIT)) {
		doQuit(cmd_id, arguments);
	    }
	    // if it's not supported
	    else {
		signalCommandError(my_id, cmd_id, "'"+command+"' is not supported");
	    }
	} catch (JDEException ex) {
	    Debug.printIf(ex);
	    // a jde exception was raised. the kind of error is already
	    // in there.
	    signalCommandError(my_id, cmd_id, ex.getMessage());
	    return;
	} catch (Exception ex) {
	    Debug.printIf(ex);
	    signalCommandError(my_id, cmd_id, "Unspecified error: "+ex.toString());
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

    /**
     * Commands received from JDE that don't have app_id == -1 get
     * funneled to handleAppCommand. We have to check if the app_id is
     * valid and if it corresponds to a valid app. If not, we send a
     * general error message back to JDE, with my_id=1 and cmd_id=-1
     * since app_id is not valid anyway
     *
     * @param app_id The ID of the application for which the command is
     * intended
     * @param cmd_id ID of the command
     * @param command The command
     * @param arguments And a list of the arguments of the command
     */
    private void handleAppCommand(Integer app_id, Integer cmd_id,
				  String command, List arguments) {
	Application app;

	synchronized(applications) {
	    // this app id should be valid. register using a connector
	    // earlier
	    if (!applications.containsKey(app_id)) {
		signalCommandError(my_id, cmd_id, "Application ID '"+app_id+"' does not exist");
		return;
	    }
	}
	synchronized(applications) {
	    // also, it should be a valid application
	    if ((app = (Application)applications.get(app_id)) == null) {
		signalCommandError(my_id, cmd_id, "Application ID '"+app_id+"' does not correspond to a valid application");
	    }
	}
	// at this point, we call the handleCommand function of the
	// app to take care of actions.
	app.handleCommand(cmd_id, command, arguments);
    }

    /*
     * a note on convention: any replies sent back will either by in
     * the handleXCommand functions, or in the doX functions. arbitrary
     * replies (ie from any function) to jde should be avoided
     */



    /**
     * This method is actually called by doLaunch, doAttach, and doListen
     * and does all the ugly work. The 3 above are separated for
     * documentation purposes
     * <p>
     * <font size=-4>The real reason is that I realised too late that they
     * share
     * the same code :-(</font>
     */
    private void initApplication(String category, Integer cmd_id, List args)
	throws JDEException {

	if (args.size() < 1)
	    throw new JDEException("Missing application ID");

	// the app id with which it will be known.
	// note that we remove the arguments as we consume them from the
	// list.
	Integer app_id =
	    new Integer(Etc.safeGetint(args.remove(0), "application ID"));

	// the app id cannot be same as my id
	if (app_id.equals(my_id)) {
	    throw new JDEException("Invalid Application ID");
	}
	// an app using this id is already present!
	// XXX make sure you dispose the id once done with the app
	synchronized (applications) {
	    if (applications.containsKey(app_id)) {
		throw new JDEException("Application ID is duplicate");
	    }
	}
	// new app. initialize stuff
	// note that new App might raise a jdeexception.
	Application app = new Application(this, app_id);
	int port = app.initialize(category, args);
	synchronized (applications) {
	    if (applications.containsKey(app_id)) {
		app.shutdown();
		throw new JDEException("A new application with the same ID exists: things will go miserably wrong!");
	    }
	    applications.put(app_id, app);
	}
	signalCommandResult(my_id, cmd_id, new Integer(port));
    }


    /**
     * 'launch' command. Launches a new application.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * launch app_id [-use_executable javax] classname [args]
     * </pre>
     * 
     * <b>Comments:</b>
     * <ul>
     * <li> this command establishes the existence of a unique
     *  "stream" of data between jde and jdebug, indexed by
     *  app_id, and referring to a particular vm.
     * <li> other commands might be ... attach app_id [args]
     *   ... where args specify the "location" of the running
     *   VM. In that case, jdebug might try to connect to the
     *   "location", and return an error if unable to do so.
     * </ul>
     */
    private void doLaunch(Integer cmd_id, List args)
	throws JDEException {
	initApplication(LAUNCH, cmd_id, args);
    }

    /**
     * 'attach_socket' and 'attach_shmem' commands. Attaches to an already
     * running application through a socket/shared memory.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * attach_socket app_id -port p_value [-host h_value]
     * attach_shmem  app_id name
     * </pre>
     *
     * <b>Comments:</b>
     * <ul>
     * <li> Like launch, except here we're attaching to a debugee vm that
     * has, of course, been launched with the "right" parameters. See the
     * Connection and Invocation details document of JPDA about this.
     * </ul>
     */
    private void doAttach(String type, Integer cmd_id, List args)
	throws JDEException {
	initApplication(type, cmd_id, args);
    }


    /**
     * 'listen_socket' and 'listen_shmem' commands. Listenes for an
     * incoming debuggee connection
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * listen_socket app_id port 
     * listen_shmem  app_id name
     * </pre>
     */
    private void doListen(String type, Integer cmd_id, List args)
	throws JDEException {
	initApplication(type, cmd_id, args);
    }


    /**
     * 'quit' command.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * quit
     * </pre>
     */
    private void doQuit(Integer cmd_id, List arguments)
	throws JDEException {
	try {
	    shutdown();
	} catch (Exception ex) {
	    // do nothing
	}
	signalCommandResult(my_id, cmd_id);
	System.exit(0);
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

    /**
     * Shuts down all the applications prior to exiting
     */
    private void shutdown()
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
	    return "\n"+returnString.toString().trim();
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
	    out.println("\n("+JDE_BUG+type+" "+app_id+")\n");
	} else {
	    String temp="("+JDE_BUG+type+"\n"+app_id+" "+stringRep(obj)+")";
	    if (temp.length() <= 80)
		out.println("\n("+JDE_BUG+type+" "+app_id+" "+
			    stringRep(obj)+")\n");
	    else
		out.println("\n"+temp+"\n");
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
	    out.println("\n("+type+" "+cmd_id+")\n");
	} else {
	    String temp = "("+type+"\n"+cmd_id+" "+strRep+")";
	    if (temp.length() <= 80)
		out.println("\n("+type+" "+cmd_id+" "+strRep+")\n");
	    else 
		out.println("\n"+temp+"\n");
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

} // JDE


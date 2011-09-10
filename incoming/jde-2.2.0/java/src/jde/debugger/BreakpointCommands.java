
package jde.debugger;

import jde.debugger.spec.*;

import com.sun.jdi.*;
import com.sun.jdi.event.*;
import com.sun.jdi.request.*;

import java.util.*;

/**
 * BreakpointCommands.java
 * <p>
 * Handles commands related to breakpointing, watchpointing and catching
 * exceptions. Jump to
 * '{@link #doCatchException doCatchException}',
 * '{@link #doWatch watch}', '{@link #doBreak break}',
 * '{@link #doClear clear}', '{@link #doStep step}'
 * <p>
 * Created: Fri Jul 30 15:44:59 1999
 * 
 * @author Amit Kumar
 * @since 0.1
 */

public class BreakpointCommands extends ApplicationCommands {

    /* Handling exceptions */


    /**
     * 'catch_exception' command.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * catch_exception classPattern <u>type</u>
     *      [{@link Etc#getThreadFromArgs(List) thread-restriction}]
     *      [{@link Etc#getSuspendPolicyFromArgs(List) suspend-policy}]
     *      [{@link Etc#getClassFiltersFromArgs(List) class-filters}]
     *      [{@link Etc#getClassExFiltersFromArgs(List) class-exclusion-filters}]
     * </pre>
     *
     * <b>Returns:</b>
     * <pre>
     * (jde-dbo-command-result cmd_id specID)
     * </pre>
     *
     * <b>Comments:</b>
     * <ul>
     * <li> <u>type</u> can be "caught", "uncaught", or "both"
     * <li> specID is a 'long', and can be used in the 'clear'
     * command
     * </ul>
     *
     * <p>
     * @see EventHandler#exceptionEvent(ExceptionEvent)
     */
    public void doCatchException(Integer cmd_id, List args)
	throws JDEException {

	if (args.size() < 2) 
	    throw new JDEException("Insufficient arguments");

	String classPattern = args.remove(0).toString();
	String type = args.remove(0).toString().toLowerCase();

	boolean caught = false;
	boolean uncaught = false;
	if (type.equals("both")) {
	    caught = true;
	    uncaught = true;
	} else if (type.equals("caught")) {
	    caught = true;
	} else if (type.equals("uncaught")) {
	    uncaught = true;
	} else {
	    throw new JDEException("'"+type+"' not understood");
	}
	    
	EventRequestSpec er = eventRequests.createExceptionIntercept(classPattern, caught, uncaught);
	er.setThread(Etc.getThreadFromArgs(args));
	er.setSuspendPolicy(Etc.getSuspendPolicyFromArgs(args));
	er.setClassFilters(Etc.getClassFiltersFromArgs(args));
	er.setClassExFilters(Etc.getClassExFiltersFromArgs(args));
	
	app.signalCommandResult(cmd_id, er.getID());
    }
    
	 
    /**
     * 'watch' command.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * watch classPattern fieldName <u>type</u>
     *      [{@link Etc#getThreadFromArgs(List) thread-restriction}]
     *      [{@link Etc#getExprFromArgs(List) expression-restriction}]
     *      [{@link Etc#getObjectIDFromArgs(List) object-id-restriction}]
     *      [{@link Etc#getSuspendPolicyFromArgs(List) suspend-policy}]
     *      [{@link Etc#getClassFiltersFromArgs(List) class-filters}]
     *      [{@link Etc#getClassExFiltersFromArgs(List) class-exclusion-filters}]
     * </pre>
     *
     * <b>Returns:</b>
     * <pre>
     * (jde-dbo-command-result cmd_id specID)
     * </pre>
     *
     * <b>Comments:</b>
     * <ul>
     * <li> <u>type</u> can be "for_access" or "for_modification"
     * <li> 'classPattern' can be a string pattern of the type *.Test
     * <li> objectID is used when, for example, when you already know the
     *  object id of the object, the access/modification of which's field
     *  you're interested in. 
     * <li> specID is a 'long', that can be used in 'clear' commands.
     * </ul>
     *
     * <p>
     * @see EventHandler#watchpointEvent(WatchpointEvent)
     */
    public void doWatch(Integer cmd_id, List args)
	throws JDEException {
	
	if (args.size() < 3)
	    throw new JDEException("Insufficient arguments");

	String classPattern = args.remove(0).toString();
	String methodName = args.remove(0).toString();
	String typeString = args.remove(0).toString().toLowerCase();

	WatchpointSpec er = null;
	if (typeString.equals("for_access")) {
	    if (!app.getVM().canWatchFieldAccess()) 
		throw new JDEException("This VM implementation cannot watch field accesses");
	    er = eventRequests.createAccessWatchpoint(classPattern, methodName);
	} else if (typeString.equals("for_modification")) {
	    if (!app.getVM().canWatchFieldModification()) 
		throw new JDEException("This VM implementation cannot watch field modifications");
	    er = eventRequests.createModificationWatchpoint(classPattern, methodName);
	} else {
	    throw new JDEException("'"+typeString+"' not understood: use either 'for_access' or 'for_modification'");
	}
	er.setThread(Etc.getThreadFromArgs(args));
	er.setExpression(Etc.getExprFromArgs(args));
	er.setObjectID(Etc.getObjectIDFromArgs(args));
	er.setSuspendPolicy(Etc.getSuspendPolicyFromArgs(args));
	er.setClassFilters(Etc.getClassFiltersFromArgs(args));
	er.setClassExFilters(Etc.getClassExFiltersFromArgs(args));

	app.signalCommandResult(cmd_id, er.getID());
    }	


    /**
     * 'break' command.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * break {@link #doBreakInMethod in_method} class method [(args)] 
     *      [{@link Etc#getThreadFromArgs(List) thread-restriction}]
     *      [{@link Etc#getExprFromArgs(List) expression-restriction}]
     *      [{@link Etc#getSuspendPolicyFromArgs(List) suspend-policy}]
     *     
     * break {@link #doBreakOnLine on_line}   class line
     *      [{@link Etc#getThreadFromArgs(List) thread-restriction}]
     *      [{@link Etc#getExprFromArgs(List) expression-restriction}]
     *      [{@link Etc#getSuspendPolicyFromArgs(List) suspend-policy}]
     *
     * break {@link #doBreakAbsolute absolute}  file line
     *      [{@link Etc#getThreadFromArgs(List) thread-restriction}]
     *      [{@link Etc#getExprFromArgs(List) expression-restriction}]
     *      [{@link Etc#getSuspendPolicyFromArgs(List) suspend-policy}]
     * </pre>
     *
     * <b>Returns:</b>
     * <pre>
     * (jde-dbo-command-result cmd_id specID)
     * </pre>
     *
     * <b>Comments:</b>
     * <ul>
     * <li> There are exactly three kinds of 'break' commands. One 
     *	of in_method, on_line, or absolute need to be used.
     * <li> 'class' can be a string pattern of the type *.Test
     * <li> specID is a 'long', that can be used in 'clear' commands.
     * </ul>
     *
     * <p>
     * @see EventHandler#breakpointEvent(BreakpointEvent)
     */
    public void doBreak(Integer cmd_id, List args) throws JDEException {
	try {
	    // whatever function is called, should do a signalCommandResult
	    // during the execution.
	    String type = args.remove(0).toString().toLowerCase();
	    if (type.equals("in_method")) {
		doBreakInMethod(cmd_id, args);
	    } else if (type.equals("on_line")) {
		doBreakOnLine(cmd_id, args);
	    } else if (type.equals("absolute")) {
		doBreakAbsolute(cmd_id, args);
	    } else
		throw new JDEException("Syntax error: expecting one of 'in_method', 'on_line', or 'absolute'; '"+type+"' is not supported");
	} catch (UnsupportedOperationException ex) {
	    throw new JDEException("Unspecified Error occured");
	} catch (IndexOutOfBoundsException ex) {
	    throw new JDEException("Syntax error: argument missing");
	}
    }

    
    /**
     * 'clear' command. Clears a breakpoint, watchpoint or an exception
     * intercept
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * clear specID
     * </pre>
     *
     * <b>Comments:</b>
     * <ul>
     * <li> specIDs are returned in the 'break'/'watch'/catch_exception
     * commands.
     * </ul>
     */
    public void doClear(Integer cmd_id, List args)
	throws JDEException {
	if (args.size() < 1)
	    throw new JDEException("Insufficient arguments");
	Long specID = Etc.safeGetLong(args.remove(0), "spec ID");
	eventRequests.removeSpec(specID);
	app.signalCommandResult(cmd_id);
    }


    /**
     * A break in a particular method.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * break in_method class method [(arg1,arg2,...)] 
     *      [{@link Etc#getThreadFromArgs(List) thread-restriction}]
     *      [{@link Etc#getExprFromArgs(List) expression-restriction}]
     *      [{@link Etc#getSuspendPolicyFromArgs(List) suspend-policy}]
     * </pre>
     *
     * <b>Comments:</b>
     * <ul>
     * <li> There should be <b>no spaces</b> before or after the ','; when 
     *      the arguments are supplied.
     * <li> A void method should be indicated by <code>()</code>
     * <li> A unique method doesn't need to supply the arguments. The
     * <b>entire</b> argument list should be absent in this case.
     * </ul>
     */
    public void doBreakInMethod(Integer cmd_id, List args)
	throws JDEException {

	if (args.size() < 2)
	    throw new JDEException("Insufficient arguments");
	
	String classPattern = args.remove(0).toString();
	String method = args.remove(0).toString();

	// the argument list
	List argumentList = null;

	// see if more arguments are present
	if (args.size() > 0) {

	    String arg = args.remove(0).toString();

	    // see if any arglist was provided at all
	    if (arg.startsWith("(")) {
		// apparently it was. double check.
		if (!arg.endsWith(")")) {
		    throw new JDEException("The argument list seems to be corrupt");
		}
		// trim the parens
		arg = arg.substring(1, arg.length() - 1);
		argumentList = new ArrayList();
		StringTokenizer t = new StringTokenizer(arg, ",");
		while (t.hasMoreTokens()) {
		    argumentList.add(t.nextToken());
		}
	    }
	}
	EventRequestSpec er = eventRequests.createMethodBreakpoint(classPattern, method, argumentList);
	er.setThread(Etc.getThreadFromArgs(args));
	er.setExpression(Etc.getExprFromArgs(args));
	er.setSuspendPolicy(Etc.getSuspendPolicyFromArgs(args));

	app.signalCommandResult(cmd_id, er.getID());
    }

    /** A break on a particular line of a class */
    public void doBreakOnLine(Integer cmd_id, List args)
	throws JDEException {

	if (args.size() < 2)
	    throw new JDEException("Insufficient arguments");
	
	String classPattern = args.remove(0).toString();
	int line = Etc.safeGetint(args.remove(0), "line number");

	EventRequestSpec er =
	    eventRequests.createClassLineBreakpoint(classPattern, line);
	er.setThread(Etc.getThreadFromArgs(args));
	er.setExpression(Etc.getExprFromArgs(args));
	er.setSuspendPolicy(Etc.getSuspendPolicyFromArgs(args));
	    
	app.signalCommandResult(cmd_id, er.getID());
    }

    /** A break on a line of a given source file */
    public void doBreakAbsolute(Integer cmd_id, List args)
	throws JDEException {

	if (args.size() < 2)
	    throw new JDEException("Insufficient arguments");
	
	String file = args.remove(0).toString();
	int line = Etc.safeGetint(args.remove(0), "line number");

	EventRequestSpec er =
	    eventRequests.createSourceLineBreakpoint(file, line);
	er.setThread(Etc.getThreadFromArgs(args));
	er.setExpression(Etc.getExprFromArgs(args));
	er.setSuspendPolicy(Etc.getSuspendPolicyFromArgs(args));
	    
	app.signalCommandResult(cmd_id, er.getID());
    }

    
    /**
     * 'step' command. This is only possible if the current thread is
     * suspended.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * step <u>type</u> threadID
     *      [{@link Etc#getSuspendPolicyFromArgs(List) suspend-policy}]
     * </pre>
     *
     * <b>Comments:</b>
     * <ul>
     * <li> <u>type</u> is one of "over", "into", and "out"
     * There are three kinds of steps that are being supported currently:
     * <ul>
     * <li> step over: steps over method calls
     * <li> step into: goes into called methods
     * <li> step out: executes till it returns to the calling method
     * </ul>
     *
     * <li>
     *	threadID is required. Also, this is only possible on a
     *	suspended thread. jde should check this before calling
     *	step.
     * </ul>
     *
     * <p>
     * @see EventHandler#stepEvent(StepEvent)
     */
    public void doStep(Integer cmd_id, List args) 
	throws JDEException {

	if (args.size() < 2)
	    throw new JDEException("Insufficient arguments");
	
	// ascertain the kind of step
	String arg = args.remove(0).toString().toLowerCase();
	int depth;
	if (arg.equals("over")) {
	    depth = StepRequest.STEP_OVER;
	} else if (arg.equals("out")) {
	    depth = StepRequest.STEP_OUT;
	} else if (arg.equals("into")) {
	    depth = StepRequest.STEP_INTO;
	} else {
	    throw new JDEException("Syntax error: use step over/out/into");
	}

	// find the thread on which to step
	Long uniqueID = Etc.safeGetLong(args.remove(0), "thread ID");
	
	ThreadReference tRef = (ThreadReference)store.get(uniqueID);
	    
	// it should exist
	if (tRef == null) {
	    throw new JDEException("Invalid thread ID or the thread is dead");
	}

	// we need to be suspended.
	// also see ThreadCommands.getThreadStringRep for some info
	if (tRef.suspendCount() == 0) {
	    throw new  JDEException("The specified thread is not suspended");
	}

	// clear any previous steps on this thread
	clearPreviousStep(tRef);

	// set a new request!
	EventRequestManager erm = app.getVM().eventRequestManager();
	StepRequest request =
	    erm.createStepRequest(tRef, StepRequest.STEP_LINE, depth);
	// a single step event... will set it again if need be.
	request.addCountFilter(1);
	request.setSuspendPolicy(Etc.getSuspendPolicyFromArgs(args));
	request.enable();

	// and now resume the vm. the thread suspended is resumed now.
	app.getVM().resume();

	app.signalCommandResult(cmd_id);
	
    }


    /**
     * Clear a previous step request on this thread: only one is allowed
     * per thread
     */
    private void clearPreviousStep(ThreadReference thread) {
	EventRequestManager mgr = app.getVM().eventRequestManager();
	List requests = mgr.stepRequests();
	Iterator iter = requests.iterator();
	while (iter.hasNext()) {
	    StepRequest request = (StepRequest)iter.next();
	    if (request.thread().equals(thread)) {
		mgr.deleteEventRequest(request);
		break;
	    }
	}
    }
    
    public BreakpointCommands(Application a, EventRequestSpecList e,
			      ObjectStore s) {
	super(a, e, s);
    }

} // BreakpointCommands

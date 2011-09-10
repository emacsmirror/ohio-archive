
package jde.debugger;

import jde.debugger.spec.*;
import jde.debugger.expr.*;

import com.sun.jdi.*;
import com.sun.jdi.request.*;

import java.util.*;

/**
 * ThreadCommands.java
 * <p>
 * Handles thread related commands. Jump to
 * '{@link #doSuspend suspend}', {@link #doResume resume}',
 * '{@link #doInterrupt interrupt}', {@link #doKillThread kill_thread},
 * '{@link #doGetThread get_thread}', '{@link #doGetThread get_thread}',
 * '{@link #doGetObjectMonitors get_object_monitors}',
 * '{@link #doTraceThreads trace_threads}',
 * '{@link #doCancelTraceThreads cancel_trace_threads}'
 * 
 * <p>
 * Created: Fri Jul 30 16:04:07 1999
 * 
 * @author Amit Kumar
 * @since 0.1
 */

public class ThreadCommands extends ApplicationCommands {
    
    /**
     * 'suspend' command.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * suspend [threadID]*
     * </pre>
     *
     * <b>Comments:</b>
     * <ul>
     * <li> threadIDs can be retreived using the get_threads command
     * <li> if the list is omitted, the entire VM is suspended
     * <li> threadIDs can refer to either threads or threadgroups.
     * </ul>
     */
    public void doSuspend(Integer cmd_id, List args)
	throws JDEException {
	// see if there are arguments (should be the thread id). if so, we
	// suspend the thread ids passed. else, suspend the whole vm.
	if (args.size() > 0) {
	    Iterator it = args.iterator();
	    while (it.hasNext()) {
		
		Long uniqueID = Etc.safeGetLong(it.next(), "thread(group)");
		    
		ObjectReference oRef = (ObjectReference)store.get(uniqueID);
		if (oRef == null) {
		    throw new JDEException("Invalid ThreadID, or the thread/threadgroup is dead");
		} else if (oRef instanceof ThreadReference) {
		    ((ThreadReference)oRef).suspend();
		} else if (oRef instanceof ThreadGroupReference) {
		    ((ThreadGroupReference)oRef).suspend();
		} else {
		    throw new JDEException("The object is not a thread or a threadgroup");
		}
	    }
	    app.signalCommandResult(cmd_id);
	} else {
	    try {
		app.getVM().suspend();
		app.signalCommandResult(cmd_id);
	    } catch (Exception ex) {
		throw new JDEException("Unable to suspend the application");
	    }
	}
    }


    /**
     * 'resume' command.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * resume [threadID]*
     * </pre>
     *
     * <b>Comments:</b>
     * <ul>
     * <li> threadIDs can be retreived using the get_threads command
     * <li> if the list is omitted, the entire VM is resumed
     * <li> threadIDs can refer to either threads or thereadgroups.
     * </ul>
     */
    public void doResume(Integer cmd_id, List args)
	throws JDEException {
	// see if there are arguments (should be the thread id). if so, we
	// resume the thread ids passed. else, resume the whole vm.
	if (args.size() > 0) {
	    Iterator it = args.iterator();
	    while (it.hasNext()) {
		Long uniqueID = Etc.safeGetLong(it.next(), "thread(group)");

		ObjectReference oRef = (ObjectReference)store.get(uniqueID);
		if (oRef == null) {
		    throw new JDEException("Invalid ThreadID, or the thread/threadgroup is dead");
		} else if (oRef instanceof ThreadReference) {
		    ((ThreadReference)oRef).resume();
		} else if (oRef instanceof ThreadGroupReference) {
		    ((ThreadGroupReference)oRef).resume();
		} else {
		    throw new JDEException("The object is not a thread or a threadgroup");
		}
	    }
	    app.signalCommandResult(cmd_id);
	} else {
	    try {
		app.getVM().resume();
		app.signalCommandResult(cmd_id);
	    } catch (Exception ex) {
		throw new JDEException("Unable to resume the application");
	    }
	}
    }


    /**
     * 'interrupt' command.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * interrupt [threadID]+
     * </pre>
     *
     * <b>Comments:</b>
     * <ul>
     * <li> threadID can be retreived using the get_threads command
     * <li> at least one threadId should be specified
     * </ul>
     */
    public void doInterrupt(Integer cmd_id, List args)
	throws JDEException {

	if (args.size() < 1) 
	    throw new JDEException("Insufficient arguments");

	Iterator it = args.iterator();
	while (it.hasNext()) {
	    Long uniqueID = Etc.safeGetLong(it.next(), "thread ID");

	    ObjectReference oRef = (ObjectReference)store.get(uniqueID);
	    if (oRef == null) {
		throw new JDEException("Invalid ThreadID, or the thread is dead");
	    } else if (oRef instanceof ThreadReference) {
		((ThreadReference)oRef).interrupt();
	    } else {
		throw new JDEException("The object is not a thread");
	    }
	}
	app.signalCommandResult(cmd_id);
    }



    /**
     * 'kill' command. Kill a thread with a given exception object.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * kill_thread threadID exceptionObjectID
     * </pre>
     *
     * <b>Comments:</b>
     * <ul>
     * <li> threadID can be retreived using the get_threads command
     * <li> exceptionObjectID is the object id of a Throwable object. It
     * can be created using the 'evaluate' command, or an existing throwable
     * object can be used.
     * </ul>
     */
    public void doKillThread(Integer cmd_id, List args)
	throws JDEException {
	
	if (args.size() < 2) 
	    throw new JDEException("Insufficient arguments");
	
	Long uniqueID = Etc.safeGetLong(args.remove(0), "thread ID");

	ObjectReference oRef = store.get(uniqueID);
	if (oRef == null) {
	    throw new JDEException("No such thread exists");
	} else if (!(oRef instanceof ThreadReference)) {
	    throw new JDEException("The ID doesn't correspond to a thread");
	}
	ThreadReference tRef = (ThreadReference)oRef;

	uniqueID = Etc.safeGetLong(args.remove(0), "thread ID");

	oRef = store.get(uniqueID);
	if (oRef == null) {
	    throw new JDEException("No such thread exists");
	}

	try {
	    tRef.stop(oRef);
	} catch (InvalidTypeException ex) {
	    throw new JDEException("Object ID doesn't correspond to a Throwable object");
	}
	app.signalCommandResult(cmd_id);
    }
    


    /**
     * List all threads. 'get_threads' command.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * get_threads
     * </pre>
     *
     * <b>Returns:</b>
     * <pre>
     * (jde-dbo-command-result cmd_id {@link #getAllThreadsInformation thread-info})
     * </pre>
     *
     * <b>Comments:</b>
     * <ul>
     * <li> There are a couple of quirks regarding the reporting
     * of thread state:
     * <ul>
     * <li> Quirk 1: Due to a bug in ThreadReference.isAtBreakpoint(),
     *        a thread will report a breakpoint at a location
     *	      even if a threadFilter has been applied for the
     *	      thread. ie, if test.Test.java:41 is your
     *	      breakpoint, and you've applied a threadfilter
     *	      saying you DON'T want an event if the thread ==
     *	      Thread-A, and you somehow suspend Thread-A at
     *	      exactly that line, and do a 'get_threads';
     *	      Thread-A will report to be suspended on a
     *	      breakpoint, although ordinarily it would have
     *	      skipped it.
     *
     * <li> Quirk 2: There seems to be no way of reporting the
     *        status if the user does a
     *	      Thread.suspend(). Well, it's deprecated
     *	      anyways... *shrug*.
     * </ul>
     * </ul>
     */
    public void doGetThreads(Integer cmd_id, List args)
	throws JDEException {
	app.signalCommandResult(cmd_id, getAllThreadsInformation(app.getVM(), store));
    }

    /**
     * Returns a representation of all the threads and threadgroups
     * in the VM. For example:
     * <pre>
     *              ThreadGroup-1
     *                  +- ThreadGroup-2
     *                  |        +- ThreadGroup-3
     *                  |        |        \- Thread-1
     *                  |        +- ThreadGroup-4
     *                  |        |        +- Thread-2
     *                  |        |        \- Thread-3
     *                  |        \- Thread-4
     *                  \- Thread-5
     *              ThreadGroup-5
     *                  +- Thread-6
     *
     *
     *          (list
     *            (list "ThreadGroup" <tgID> "ThreadGroup-1"
     *              (list 
     *                (list "Thread" <tID> "Thread-5" ...))
     *              (list 
     *                (list "ThreadGroup" <tgID> "ThreadGroup-2"
     *                  (list 
     *                    (list "Thread" <tID> "Thread-4"))
     *                  (list 
     *                    (list "ThreadGroup" <tgID> "ThreadGroup-3"
     *                      (list)
     *                      (list 
     *                        (list "Thread" <tID> "Thread-1" ...)))
     *                    (list "ThreadGroup" <tgID> "ThreadGroup-4"
     *                      (list)
     *                        (list
     *                          (list "Thread" <tID> "Thread-2" ...)
     *                          (list "Thread" <tID> "Thread-3" ...)))))))
     *          (list "ThreadGroup" <tgID> "ThreadGroup-5"
     *            (list)
     *              (list
     *                (list "Thread" <tID> "Thread-6" ...))))
     * </pre>
     * <b>Syntax:</b>
     * <pre>
     * (list [{@link Rep#getThreadGroupRep top-level thread group}]*)
     * </pre>
     *
     * @param vm The virtual machine itself
     * @param store The object store where you should keep references to
     * the thread ids. For details, see {@link ObjectStore}
     */
    static LispForm getAllThreadsInformation(VirtualMachine vm,
					     ObjectStore store) {
	List l = vm.topLevelThreadGroups();
	
	String info = "(list ";
	Iterator it = l.iterator();

	while (it.hasNext()) {
	    info +=  BR +Rep.getThreadGroupRep((ThreadGroupReference)it.next(), store);
	}

	info += ")";
	return new LispForm(info);
    }

    /**
     * Returns the thread corresponding to this name
     */
    static ThreadReference getThread(VirtualMachine vm, String name) {
	
	List list = vm.allThreads();
	Iterator it = list.iterator();

	ThreadReference thread;
	while (it.hasNext()) {
	    thread = (ThreadReference)it.next();
	    if (thread.name().equals(name)) return thread;
	}

	return null;
    }


    /**
     * 'get_thread' command. List a thread in more detail.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * get_thread threadID
     * </pre>
     *
     * <b>Returns:</b>
     * <pre>
     * (jde-dbo-command-result cmd_id {@link Rep#getThreadRep(ThreadReference, ObjectStore, boolean) detailed-thread-info})
     * </pre>
     *
     * <b>Comments:</b>
     * <ul>
     * <li> The thread can be waiting for a monitor through entry
     * into a synchronized method, the synchronized
     * statement, or Object.wait(). The status() method can be used to
     * differentiate between the first two cases and the third. 
     * </ul>
     */
    public void doGetThread(Integer cmd_id, List args)
	throws JDEException {

	if (args.size() < 1)
	    throw new JDEException("Insufficient arguments");

	Long uniqueID = Etc.safeGetLong(args.remove(0), "thread ID");
	ObjectReference tRef = store.get(uniqueID);
	    
	if (tRef == null) {
	    throw new JDEException("No such thread exists");
	} else if (!(tRef instanceof ThreadReference)) {
	    throw new JDEException("No such thread exists (anymore?)");
	}
	    
	app.signalCommandResult(cmd_id, Rep.getThreadRep((ThreadReference)tRef, store, true));
	    
    }


    /**
     * 'get_object_monitors' command. Information about the monitors
     * corresponding to a particular object.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * get_object_monitors objectID
     * </pre>
     *
     * <b>Returns:</b>
     * <pre>
     * (jde-dbo-command-result cmd_id {@link Rep#getObjectMonitorsRep(ObjectReference, ObjectStore) object-monitors-info})
     * </pre>
     */
    public void doGetObjectMonitors(Integer cmd_id, List args)
	throws JDEException {

	if (args.size() != 1)
	    throw new JDEException("Insufficient arguments");
	
	Long uniqueID = Etc.safeGetLong(args.remove(0), "object ID");
	ObjectReference oRef = store.get(uniqueID);
	if (oRef == null) 
	    throw new JDEException("No such object exists");

	app.signalCommandResult(cmd_id,Rep.getObjectMonitorsRep(oRef,store));
    }


    /**
     * 'trace_threads' command.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * trace_threads <u>type</u> [threadID]
     *      [{@link Etc#getSuspendPolicyFromArgs(List) suspend-policy}]
     * </pre>
     *
     * <b>Comments:</b>
     * <ul>
     * <li> <u>type</u> can be either "start" or "death"
     * <li> If no threadID is specified, all the corresponding thread
     * events are raised.
     * </ul>
     *
     * <p>
     * @see EventHandler#threadStartEvent(ThreadStartEvent)
     * @see EventHandler#threadDeathEvent(ThreadDeathEvent)
     */
    public void doTraceThreads(Integer cmd_id, List args)
	throws JDEException {

	if (args.size() < 2)
	    throw new JDEException("Insufficient arguments");

	String type = args.remove(0).toString().toLowerCase();

	if (!(type.equals("start") || type.equals("death")))
	    throw new JDEException("Invalid type");

	List classFilters = Etc.getClassFiltersFromArgs(args);
	List classExFilters = Etc.getClassExFiltersFromArgs(args);

	EventRequestManager em = app.getVM().eventRequestManager();
	
	Long requestID = null;

	if (type.equals("start")) {

	    ThreadStartRequest ter = em.createThreadStartRequest();

	    ter.setSuspendPolicy(Etc.getSuspendPolicyFromArgs(args));

	    if (args.size() > 0) {
		Long threadID = Etc.safeGetLong(args.remove(0), "thread ID");
		ObjectReference tRef = store.get(threadID);
		if (tRef == null) {
		    throw new JDEException("No such thread exists");
		} else if (!(tRef instanceof ThreadReference)) {
		    throw new JDEException("No such thread exists (anymore?)");
		}
		ter.addThreadFilter((ThreadReference)tRef);
	    }

	    requestID = addIdentifiableRequest(ter);
	    
	} else if (type.equals("death")) {

	    ThreadDeathRequest ter = em.createThreadDeathRequest();

	    ter.setSuspendPolicy(Etc.getSuspendPolicyFromArgs(args));
	    
	    if (args.size() > 0) {
		Long threadID = Etc.safeGetLong(args.remove(0), "thread ID");
		ObjectReference tRef = store.get(threadID);
		if (tRef == null) {
		    throw new JDEException("No such thread exists");
		} else if (!(tRef instanceof ThreadReference)) {
		    throw new JDEException("No such thread exists (anymore?)");
		}
		ter.addThreadFilter((ThreadReference)tRef);
	    }
	    
	    requestID = addIdentifiableRequest(ter);
	}
	app.signalCommandResult(cmd_id, requestID);
    }

    
    /**
     * 'cancel_trace_threads' command.
     * <p>
     *
     * <b>Syntax: </b>
     * <pre>
     * cancel_trace_threads <u>requestID</u>
     * </pre>
     *
     * <b>Comments:</b>
     * <ul>
     * <li> <u>requestID</u> is returned in the trace threads reply
     * </ul>
     */
    public void doCancelTraceThreads(Integer cmd_id, List args)
	throws JDEException {

	if (args.size() < 1)
	    throw new JDEException("Insufficient arguments");

	deleteIdentifiableRequest(Etc.safeGetLong
				  (args.remove(0), "request ID"));

	app.signalCommandResult(cmd_id);
    }

    
    public ThreadCommands(Application a, ObjectStore s) {
	super(a, s);
    }

} // ThreadCommands


package jde.debugger;

import jde.debugger.spec.*;

import java.util.*;

import com.sun.jdi.*;
import com.sun.jdi.event.*;
import com.sun.jdi.request.*;
import com.sun.jdi.connect.*;

/**
 * EventHandler.java
 * <p>
 * Each debugee VM has an event handler thread on the jdebug side
 * associated with it that receive all the events from the debugee
 * vm. In turn, the event handler thread passes the events on to the
 * jde, indicating if the vm/current thread was suspended.
 * <p>
 * Created: Tue Jul  6 14:08:44 1999
 *
 * @author Amit Kumar
 * @since 0.1
 */

public class EventHandler implements Runnable, Protocol {

    /** Are we connected to the VM? */
    boolean connected = true;
    
    /** The application for which we're the event handler */
    final Application app;

    /** The ID of the application */
    final Integer my_id;

    /**
     * My own thread. Used when we want to suspend ourselves or some such
     * weird stuff :-)
     */
    final Thread thread;

    /** Keeping track of if the thread is over yet or not */
    boolean completed = false;

    /**
     * Used by the event handlers to indicate if the app should be
     * resumed after the event occured.
     */
    boolean resumeApp;

    public EventHandler(Application app) {
        /* the q gets all the events from the vm */
	this.app = app;
	this.my_id = app.getId();
	this.thread = new Thread(this, "Event Handler for App #"+my_id);
	this.thread.start();
    }

    public void shutdown() {
	connected = false;
	thread.interrupt();
	while (!completed) {
	    try {wait();} catch (InterruptedException ex) {}
	}
    }

    /**
     * The thread reads an eventset at a time from the application queue,
     * and processes it. Essentially, one by one, each event of the eventset
     * is sent to the corresponding "handler", and if at least one of them
     * returns a non-null value, we pass on the eventset (and the values
     * returned by the handlers) to jde. Otherwise, we just resume the vm
     * (regardless of the suspend policy) and wait for the next eventset.
     * <p>
     * This is the syntax of the event set:
     * <pre>
     * (JDE_BUG_EVENTSET <u>suspend-state</u> <u>thread</u> [<u>event-string</u>]+)
     * </pre>
     * 
     * <ul>
     * <li> <u>suspend-state</u> is one of "all", "none" and "thread"
     * <li> <u>thread</u> is either nil, or the thread corresponding to the
     *      event set.
     * <li> <u>event-string</u> is the reply from a handler method. See the
     *      individual handler methods.
     * <li> The suspend policy is as follows. Some of the commands (see
     *      the {@link Protocol protocol}) allow for specifying the suspend
     *      policy for the corresponding event. For example, we might specify
     *      that the vm resume automatically after a  method entry event is
     *      reported.
     *      <p>
     *      Firstly, when multiple events are sent (in an event-set), the
     *      suspend policy of the entire event set is the one that would
     *      suspend the most threads. So, if a breakpoint-hit is sent with
     *      the method-entry event, and the breakpoint-hit wants to suspend
     *      the vm, while method-entry wants to resume, overall, the event
     *      set suspend policy will be to suspend
     *      <p>
     *      Further, some of the events might occur that haven't been
     *      explicitely requested, and hence we haven't set their suspend
     *      policy anyway. In these cases, and even otherwise, another
     *      decision is made at the time of handling of events regarding the
     *      suspension/resumption of the vm.
     *      <p>
     *      So, when an event set is received, each event is handled (via
     *      it's handler method). Each event ANDs to a variable resumeApp
     *      what it thinks should be done to the app: for example, the
     *      vmDeathEvent handler wants to resume, so it does
     *      <code>resumeApp &= true;</code>. In the same way, when a
     *      breakpoint is hit (and matches the constraints), it does a
     *      <code>resumeApp &= false:</code>.
     *      <p>
     *      After all events have been handled, we check the value of
     *      resumeApp. If it's true, we resume the vm. If not, we check
     *      the suspend-policy of the event set, and take appropriate action.
     *      <p>
     *      This two-tier suspend-policy handling might be simplified to
     *      some extent once all commands support setting of a suspend
     *      policy, with that and that alone being used to decide on what
     *      to do with the vm.
     *      <p>
     *      A problem with that approach is that since we do a second pass
     *      on the events when we get the eventset to determine if the
     *      events should be sent to the user (eg. a breakpoint-hit is still
     *      not interesting if it doesn't occur on the "right" thread), the
     *      latter approach might not work well at all.
     * </ul>
     */ 
    public void run() {
	EventQueue queue = app.getEventQueue();
        /* only need to run while we're connected */
        while (connected) {
            try {
                EventSet eventSet = queue.remove();

		resumeApp = true;

		List events = new ArrayList();
                EventIterator it = eventSet.eventIterator();

		//		System.out.println("Suspend policy of eventset: "+((eventSet.suspendPolicy() == EventRequest.SUSPEND_NONE)?"none":"not none"));

		while (it.hasNext()) {
		    //		    System.out.println("An event");
		    LispForm sendLispForm = handleEvent(it.nextEvent());
		    // if the handler thinks the user doesn't <i>deserve</i>
		    // the event, it'll return null. another microsoftism
		    // in the code... :-(
		    if (sendLispForm != null) {
			events.add(sendLispForm);
		    }
                }

		// if no event wants to inform itself to the user, what
		// are we doing anyway? 
		if (events.size() == 0) {
		    eventSet.resume();
		    continue;
		}
		
		String suspendStateString;
                if (resumeApp) {
		    // this resume does a vm resume or a thread resume
		    // depending on how it was suspended
		    suspendStateString = "none";
                    eventSet.resume();
                } else {
		    switch(eventSet.suspendPolicy()) {
		    case EventRequest.SUSPEND_ALL: {
			suspendStateString = "all";
			break;
		    }
		    case EventRequest.SUSPEND_EVENT_THREAD: {
			suspendStateString = "thread";
			break;
		    }
		    case EventRequest.SUSPEND_NONE: {
			suspendStateString = "none";
			eventSet.resume();
			break;
		    }
		    default: {
			suspendStateString = "invalid";
			break;
		    }
		    }
		}

		String eventSetString = "\""+suspendStateString+"\"";

		ThreadReference eventThread = getCurrentThread(eventSet);
		if (eventThread == null) {
		    eventSetString += " nil";
		} else {
		    eventSetString +=  BR +
			Rep.getThreadRep(eventThread, app.getStore());
		}
		
		Iterator iter = events.iterator();
		while (iter.hasNext()) {
		    eventSetString +=  BR +iter.next();
		}
		
		// finally, we send the events to The Man. (or woman...)
		app.signal(EVENTSET, new LispForm(eventSetString));
		
	    } catch (InterruptedException ex) {
		// Debug.printIf(ex);
		// used by shutdown
	    } catch (VMDisconnectedException ex) {
		Debug.printIf(ex);
		handleDisconnectedException();
	    }
	}
	synchronized (this) {
	    completed = true;
	    notifyAll();
	}

    }


    /**
     * Handles the events that happened
     *
     * @param event One of the events in the event set.
     * @return A LispForm that indicates what should be sent to jde. This
     * should be of the form "(list jde-dbo-EVENT-event [args])". Each
     * of the functions called herein should return a LispForm of the above
     * form.
     */
    private LispForm handleEvent(Event event) {
	if (event instanceof BreakpointEvent) {
	    return breakpointEvent((BreakpointEvent)event);
	} else if (event instanceof StepEvent) {
	    return stepEvent((StepEvent)event);
	} else if (event instanceof WatchpointEvent) {
	    return watchpointEvent((WatchpointEvent)event);
	} else if (event instanceof ExceptionEvent) {
	    return exceptionEvent((ExceptionEvent)event);
	}
	else if (event instanceof ThreadStartEvent) {
	    return threadStartEvent((ThreadStartEvent)event);
	} else if (event instanceof ThreadDeathEvent) {
	    return threadDeathEvent((ThreadDeathEvent)event);
	}
	else if (event instanceof MethodEntryEvent) {
	    return methodEntryEvent((MethodEntryEvent)event);
	} else if (event instanceof MethodExitEvent) {
	    return methodExitEvent((MethodExitEvent)event);
	}
	else if (event instanceof ClassPrepareEvent) {
	    return classPrepareEvent((ClassPrepareEvent)event);
	}
	else if (event instanceof VMStartEvent) {
	    return vmStartEvent((VMStartEvent)event);
	} else if (event instanceof VMDeathEvent) {
	    return vmDeathEvent((VMDeathEvent)event);
	} else if (event instanceof VMDisconnectEvent) {
	    return vmDisconnectEvent((VMDisconnectEvent)event);
	} else {
	    return otherEvent(event);
	}
    }

    /**
     * Duh... we don't recognize this event... or, we choose not to do
     * anything about it
     */
    private LispForm otherEvent(Event event) {
	resumeApp &= true;
	return new LispForm("(list '"+EVENT_OTHER+")");
    }

    /**
     * What we should do if we get disconnected while we're doing something
     * else.
     */
    private void handleDisconnectedException() {
	// disconnected while handling some other event. flush queue
	// and deal with disconnectEvent and deathEvents
	EventQueue queue = app.getEventQueue();
	while (connected) {
	    try {
		EventSet eventSet = queue.remove();
		EventIterator iter = eventSet.eventIterator();
		while (iter.hasNext()) {
		    Event evt = (Event)iter.next();
		    if (evt instanceof VMDeathEvent) {
			vmDeathEvent(evt);
		    } else if (evt instanceof VMDisconnectEvent) {
			vmDisconnectEvent(evt);
		    }
		}
	    } catch (InterruptedException ex) {
		Debug.printIf(ex);
		// ignore
	    }
	}
    }

    /**
     * Get the current thread of this event set. It's not necessary for
     * an event set to have a thread associated with it: in those cases
     * just return null.
     *
     * @param eventSet The event set that occured
     * @return <code>null</code> if there is no thread associated, otherwise
     * the ThreadReference of the Thread
     */
    private ThreadReference getCurrentThread(EventSet eventSet) {
	ThreadReference thread;
	if (eventSet.size() > 0) {
            /*
             * If any event in the set has a thread associated with it, 
             * they all will, so just grab the first one. 
             */
            Event event = (Event)eventSet.iterator().next(); // Is there a better way?
            thread = getEventThread(event);
        } else {
            thread = null;
        }
	return thread;
    }

    /**
     * Black magic to divine the ThreadReference of the
     * event. Question: can we use "Black magic" and "divine" in the same
     * sentence?
     *
     * @param event An event from the event set: any event will do
     * @return The ThreadReference of the thread
     */
    private ThreadReference getEventThread(Event event) {
        if (event instanceof ClassPrepareEvent) {
            return ((ClassPrepareEvent)event).thread();
        } else if (event instanceof LocatableEvent) {
            return ((LocatableEvent)event).thread();
        } else if (event instanceof ThreadStartEvent) {
            return ((ThreadStartEvent)event).thread();
        } else if (event instanceof ThreadDeathEvent) {
            return ((ThreadDeathEvent)event).thread();
        } else if (event instanceof VMStartEvent) {
            return ((VMStartEvent)event).thread();
        } else {
            return null;
        }
    }


    /**
     * For events with corresponding specs (eg. watchpoints), checks to see
     * if the event thread matches the thread constraint in the spec.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * nil                              # If no such constraint
     *                                  #   was found in the spec
     * "Error mesg"                     # Constraint found, but error
     *                                  #   during evaluation
     * (list "on_thread_id" threadID)   # Constraint found, satisfied
     * (list "on_thread_name" "name")
     * </pre>
     *
     * <b>Comments:</b>
     * <ul>
     * <li> The above are strings: so the '"'s above are actually quoted
     *      in the string returned.
     * <li> Note that if the Constraint is <b>found but does not satisfy</b>
     *      'null' is returned: this is tested in the handler routines.
     * </ul>
     */
    private String threadMatch(Event event) {
	Object thread =
	    event.request().getProperty(EventRequestSpec.threadKey);
	if (thread == null) {
	    return "nil";
	} else if (thread instanceof Long) {
	    ThreadReference t = getEventThread(event);
	    if (t.uniqueID() == ((Long)thread).longValue()) {
		return "(list \"on_thread_id\" "+t.uniqueID()+")";
	    } else {
		return null;
	    }
	} else if (thread instanceof String) {
	    ThreadReference tRef =
		ThreadCommands.getThread(app.getVM(), thread.toString());
	    ThreadReference t = getEventThread(event);
	    
	    if (t.equals(tRef)) {
		return "(list \"on_thread_name\""
		    +" \""+thread.toString()+"\")";
	    } else {
		return null;
	    }
	} else {
	    return "\"Error matching thread\"";
	}
    }

    /**
     * For events with corresponding specs (eg. watchpoints), evaluates the
     * expression stored in the spec, if any, to check if the event is
     * interesting to the user or not. The constraint is thus the expression.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * nil                              # If no such constraint
     *                                  #   was found in the spec
     * "Error mesg"                     # Constraint found, but error
     *                                  #   during evaluation
     * (list "expression")              # Constraint found, satisfied
     * </pre>
     *
     * <b>Comments:</b>
     * <ul>
     * <li> The above are strings: so the '"'s above are actually quoted
     *      in the string returned.
     * <li> Note that if the Constraint is <b>found but does not satisfy</b>
     *      'null' is returned: this is tested in the handler routines.
     * </ul>
     */
    private String expressionSuccess(Event event) {
	Object exprObject =
	    event.request().getProperty(EventRequestSpec.expressionKey);
	if (exprObject != null) {
	    String expr = exprObject.toString();
	    try {
		StackFrame frame = getEventThread(event).frame(0);
		Value val = Etc.evaluate(expr, frame);
		if (!val.type().name().equals("boolean")) {
		    return "\"Expression evaluates to non-boolean\"";
		} else {
		    BooleanValue boolValue = (BooleanValue)val;
		    if (boolValue.value() == true) {
			return "(list \""+expr+"\")";
		    } else {
			return null;
		    }
		}
	    } catch (Exception ex) {
		Debug.printIf(ex);
		return "\"Expression didn't evaluate correctly\"";
	    }
	}
	return "nil";
    }


    /**
     * For watchpoints, when the object-id specified during the command
     * should match the object id of the event.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * nil                              # if no such constraint was in spec
     * "Error mesg"                     # Constraint found, error in eval.
     * (list <u>object-id</u>)                 # Constraint found, satisfied.
     * </pre>
     *
     * <b>Comments:</b>
     * <ul>
     * <li> The above are strings: so the '"'s above are actually quoted
     *      in the string returned.
     * <li> Note that if the Constraint is <b>found but does not satisfy</b>
     *      'null' is returned: this is tested in the handler routines.
     * <li> <u>object-id</u> is the object id that was specified in the
     *      command
     * </ul>
     */
    private String objectIDMatches(WatchpointEvent event) {
	String objectIDString;
	Object idObject =
	    event.request().getProperty(WatchpointSpec.objectIDKey);
	if (idObject == null) {
	    return "nil";
	} else if (idObject instanceof Long) {
	    Long id = (Long)idObject;
	    if (event.object() == null) {
		return "(list "+id+")";
	    } else if (event.object().uniqueID() == id.longValue()) {
		return "(list "+id+")";
	    } else {
		return null;
	    }
	} else {
	    return "\"Object ID was not a Long\"";
	}
    } 

    
    /**
     * A Breakpoint was hit. We check (based on information in the event
     * request; put in when the spec was resolved; that's sent along with
     * the event) if the user wants to know about this event. If so, we
     * return a lispform, else return null.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * (list {@link Protocol#EVENT_BREAKPOINT_HIT breakpoint-hit-function} specID {@link Rep#getLocationRep(Location) location} {@link #threadMatch thread-string} {@link #expressionSuccess expression-string})
     * </pre>
     *
     * <b>Comments:</b>
     * <ul>
     * <li> If one of the constraints returns null instead of a string, it
     *      means the constraint isn't satisfied. We return null, indicating
     *      that the user is not interested in the event of this event :-)
     * </ul>
     */
    private LispForm breakpointEvent(BreakpointEvent event) {

	Long specID = ((EventRequestSpec)event.request().getProperty(EventRequestSpec.specPropertyKey)).getID();

	// check if the current thread is ok.
	String threadString = threadMatch(event);
	if (threadString == null) {
	    resumeApp &= true;
	    return null;
	}
		
	// check if the expression matches
	String exprString = expressionSuccess(event);
	if (exprString == null) {
	    resumeApp &= true;
	    return null;
	}

	resumeApp &= false;
	return new LispForm("(list '"+EVENT_BREAKPOINT_HIT
			    +" "+specID
			    + BR +Rep.getLocationRep(event.location())
			    +" "+threadString
			    +" "+exprString+")");

    }


    /**
     * A step event occured.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * (list {@link Protocol#EVENT_STEP_COMPLETED step-function} {@link Rep#getLocationRep(Location) location-string})
     * </pre>
     */
    private LispForm stepEvent(StepEvent event) {
	resumeApp &= false;
	return new LispForm("(list '"
			    +EVENT_STEP_COMPLETED
			    +" "+Rep.getLocationRep(event.location())
			    +")");
    }

    /**
     * A Watchpoint occured. We check (based on information in the event
     * request; put in when the spec was resolved; that's sent along with
     * the event) if the user wants to know about this event. If so, we
     * return a lispform, else return null.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * (list {@link Protocol#EVENT_WATCHPOINT_HIT watchpoint-hit-function} specID {@link Rep#getObjectRep(ObjectReference,ObjectStore) object-on-which-hit} {@link Rep#getFieldValueRep(Field, Value, ObjectStore) field-and-value} {@link Rep#getLocationRep(Location) location}
     *       {@link #objectIDMatches object-id-string} {@link #threadMatch thread-string} {@link #expressionSuccess expression-string})
     * </pre>
     *
     * <b>Comments:</b>
     * <ul>
     * <li> If one of the constraints returns null instead of a string, it
     *      means the constraint isn't satisfied. We return null, indicating
     *      that the user is not interested in the event of this event :-)
     * </ul>
     */
    private LispForm watchpointEvent(WatchpointEvent event) {

	Long specID = ((EventRequestSpec)event.request().getProperty(EventRequestSpec.specPropertyKey)).getID();

	//	System.out.println("Suspend policy of WatchpointEvent: "+((event.request().suspendPolicy() == EventRequest.SUSPEND_NONE)?"none":"not none"));

	// check if the current thread is ok.
	String threadString = threadMatch(event);
	if (threadString == null) {
	    resumeApp &= true;
	    return null;
	}
		
	// check if the expression matches
	String exprString = expressionSuccess(event);
	if (exprString == null) {
	    resumeApp &= true;
	    return null;
	}

	// check if the object id (if specified) matches the event object
	String objectIDString = objectIDMatches(event);
	if (objectIDString == null) {
	    resumeApp &= true;
	    return null;
	}

	String fieldValueString =
	    Rep.getFieldValueRep(event.field(), event.valueCurrent(),
				 app.getStore()).toString();

	String objectString = Rep.getObjectRep(event.object(),
					       app.getStore()).toString();

	resumeApp &= false;
	return new LispForm("(list '"+EVENT_WATCHPOINT_HIT
			    +" "+specID
			    + BR +objectString
			    + BR +fieldValueString
			    + BR +Rep.getLocationRep(event.location())
			    + BR +objectIDString
			    +" "+threadString
			    +" "+exprString+")");

    }


    /**
     * An exception event occured. Depending on the command, it could occur
     * for both caught and uncaught exceptions.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * (list {@link Protocol#EVENT_EXCEPTION exception-function} specID {@link Rep#getObjectRep(ObjectReference,ObjectStore) exception} {@link #threadMatch thread-string})
     * </pre>
     *
     * <b>Comments:</b>
     * <ul>
     * <li> If one of the constraints returns null instead of a string, it
     *      means the constraint isn't satisfied. We return null, indicating
     *      that the user is not interested in the event of this event :-)
     * </ul>
     */
    private LispForm exceptionEvent(ExceptionEvent event) {

	if (event.request() == null) {
	    resumeApp &= true;
	    return null;
	}
	
	Long specID = ((EventRequestSpec)event.request().getProperty(EventRequestSpec.specPropertyKey)).getID();
	    
	ExceptionRequest request =
	    (ExceptionRequest)event.request();

	// check if the current thread is ok.
	String threadString = threadMatch(event);
	if (threadString == null) {
	    resumeApp &= true;
	    return null;
	}
		
	resumeApp &= false;
	return new LispForm("(list '"
			    +EVENT_EXCEPTION
			    +" "+specID
			    + BR +Rep.getObjectRep(event.exception(),
						   app.getStore())
			    + BR +threadString+")");
    }


    /**
     * A method was entered.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * (list {@link Protocol#EVENT_METHOD_ENTRY method-entry-function} {@link Rep#getMethodRep method})
     * </pre>
     */
    private LispForm methodEntryEvent(MethodEntryEvent event) {
	resumeApp &= false;
	return new LispForm("(list '"
			    + EVENT_METHOD_ENTRY
			    + BR +Rep.getMethodRep(event.method())
			    +")");
    }
			    
    /**
     * A method was exit.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * (list {@link Protocol#EVENT_METHOD_EXIT method-exit-function} {@link Rep#getMethodRep method})
     * </pre>
     */
    private LispForm methodExitEvent(MethodExitEvent event) {
	resumeApp &= false;
	return new LispForm("(list '"
			    + EVENT_METHOD_EXIT
			    + BR +Rep.getMethodRep(event.method())
			    +")");
    }
			    

    /**
     * A thread started
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * (list {@link Protocol#EVENT_THREAD_START thread-start-function} {@link Rep#getThreadRep thread})
     * </pre>
     */
    private LispForm threadStartEvent(ThreadStartEvent event) {
	resumeApp &= false;
	return new LispForm("(list '"
			    + EVENT_THREAD_START
			    + BR +Rep.getThreadRep(event.thread(),
						   app.getStore())
			    +")");
    }
			    
    /**
     * A thread died.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * (list {@link Protocol#EVENT_THREAD_DEATH thread-death-function} {@link Rep#getThreadRep thread})
     * </pre>
     */
    private LispForm threadDeathEvent(ThreadDeathEvent event) {
	resumeApp &= false;
	return new LispForm("(list '"
			    + EVENT_THREAD_DEATH
			    + BR +Rep.getThreadRep(event.thread(),
						   app.getStore())
			    +")");
    }
			    

    /**
     * A class was prepared. The user might not have even requested for
     * this event: we set it up by default in {@link Application#initialize
     * Application} because we need it for resolution of specs.
     * <p>
     * If a user also requests for this event, a particular property is
     * likely not set, and that is how we know that we're supposed to
     * handle the event for the user.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * (list {@link Protocol#EVENT_CLASS_PREPARE class-prepare-function} reference-type)
     * </pre>
     */
    private LispForm classPrepareEvent(ClassPrepareEvent event) {
	app.resolve(event.referenceType());
	// now find out if this event was also requested by the user.
	// it will be, if the "default" property does NOT exists in the
	// corresponding request. 
	EventRequest request = event.request();
	if (request.getProperty("default") == null) {
	    resumeApp &= false;
	    return new LispForm("(list '"+EVENT_CLASS_PREPARE
				+" \""+event.referenceType().name()+"\")");
	} else {
	    resumeApp &= true;
	    return null;
	}
    }

    /**
     * A class was unloaded.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * (list {@link Protocol#EVENT_CLASS_UNLOAD class-unload-function} reference-type)
     * </pre>
     */
    private LispForm classUnloadEvent(ClassUnloadEvent event) {
	resumeApp &= false;
	return new LispForm("(list '"+EVENT_CLASS_UNLOAD
			    +" \""+event.className()+"\")");
    }
    
    private LispForm vmStartEvent(Event event) {
	resumeApp &= false;
	return new LispForm("(list '"+EVENT_VM_START+")");
    }
    
    private LispForm vmDeathEvent(Event event) {
	resumeApp &= true;
	return new LispForm("(list '"+EVENT_VM_DEATH+")");
    }

    private LispForm vmDisconnectEvent(Event event) {
	connected = false;
	resumeApp &= true;
	app.shutdown();
	return new LispForm("(list '"+EVENT_VM_DISCONNECT+")");
    }
			
} // EventHandler

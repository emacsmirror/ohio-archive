
package jde.debugger.spec;

import jde.debugger.*;

import com.sun.jdi.*;
import com.sun.jdi.event.*;
import com.sun.jdi.request.*;

import java.util.*;

/**
 * EventRequestSpecList.java
 * <p>
 * Maintains a list of all the "specs", ie requests by the user for
 * notification of a particular type of event. Not all commands create
 * specs: watchpoints, breakpoints, and exception catches do.
 * <p>
 * See {@link EventRequestSpec} for more details.
 * <p>
 * Created: Thu Jul 15 11:26:23 1999
 *
 * @author Amit Kumar
 * @since 0.1
 */

public class EventRequestSpecList {

    /**
     * a Hashmap of all the {@link EventRequestSpec}s for the application,
     * specID -> spec
     */
    private Map eventRequestSpecs =
	Collections.synchronizedMap(new HashMap());

    /** The application for which we're maintaining the specs */
    private final Application app;

    public EventRequestSpecList(Application app) {
	this.app = app;
    }

    //    public void setupInitialRequests() {

	//	ExceptionRequest erequest = em.createExceptionRequest(null, false, true);
	//	erequest.setSuspendPolicy(EventRequest.SUSPEND_NONE);
	//	erequest.enable();
    //    }

    /** 
     * Resolve all deferred eventRequests waiting for 'refType'. This is
     * called when a new reference type is prepared. We iterate through
     * all the requestspecs, calling their
     * {@link EventRequestSpec#attemptResolve(ReferenceType) attemptResolve}
     * methods.
     *
     * @param refType The reference type that was recently prepared
     */
    public void resolve(ReferenceType refType) {
        synchronized(eventRequestSpecs) {
            Iterator iter = eventRequestSpecs.values().iterator();
            while (iter.hasNext()) {
                ((EventRequestSpec)iter.next()).attemptResolve(refType);
             }
        }
    }

    /** Install a new event request spec */
    public void install(EventRequestSpec ers) {
        synchronized (eventRequestSpecs) {
            eventRequestSpecs.put(ers.getID(), ers);
        }
        if (app.getVM() != null) {
            ers.attemptImmediateResolve(app.getVM());
        }
    }

    /** Delete an existing event request spec */
    public void delete(EventRequestSpec ers) {
        EventRequest request = ers.getEventRequest();
        synchronized (eventRequestSpecs) {
            eventRequestSpecs.remove(ers.getID());
        }
        if (request != null) {
            request.virtualMachine().eventRequestManager()
                .deleteEventRequest(request);
        }
    }

    /** remove a spec based on its specID */
    public void removeSpec(Long specID)
	throws JDEException {
	synchronized (eventRequestSpecs) {
	    if (!eventRequestSpecs.containsKey(specID))
		throw new JDEException("'"+specID+"' doesn't exist");
	    delete((EventRequestSpec)eventRequestSpecs.get(specID));
	}
    }

    public EventRequestSpec createExceptionIntercept(String classPattern, 
						     boolean notifyCaught, 
						     boolean notifyUncaught){
	ReferenceTypeSpec refSpec =
	    new PatternReferenceTypeSpec(classPattern);
	EventRequestSpec ers =
	    new ExceptionSpec(app, refSpec, notifyCaught, notifyUncaught);
	return ers;
    }

    public WatchpointSpec createAccessWatchpoint
	(String classPattern, String m) {
        ReferenceTypeSpec refSpec = 
            new PatternReferenceTypeSpec(classPattern);
	WatchpointSpec ers =
	    new AccessWatchpointSpec(app, refSpec, m);
	return ers;
    }

    public WatchpointSpec createModificationWatchpoint
	(String classPattern, String m) {
        ReferenceTypeSpec refSpec = 
            new PatternReferenceTypeSpec(classPattern);
	WatchpointSpec ers =
	    new ModificationWatchpointSpec(app, refSpec, m);
	return ers;
    }

    public EventRequestSpec createClassLineBreakpoint
	(String classPattern, int line) {
        ReferenceTypeSpec refSpec = 
            new PatternReferenceTypeSpec(classPattern);
	EventRequestSpec ers =
	    new LineBreakpointSpec(app, refSpec, line);
	return ers;
    }

    public EventRequestSpec createSourceLineBreakpoint
	(String sourceName, int line) {
        ReferenceTypeSpec refSpec = 
            new SourceNameReferenceTypeSpec(sourceName, line);
	EventRequestSpec ers = 
	    new LineBreakpointSpec(app, refSpec, line);
	return ers;
    }
        
    public EventRequestSpec createMethodBreakpoint
	(String classPattern, String methodId, List methodArgs) {
        ReferenceTypeSpec refSpec = 
            new PatternReferenceTypeSpec(classPattern);
	EventRequestSpec e = 
	    new MethodBreakpointSpec(app, refSpec, methodId, methodArgs);
	return e;
    }
        
} // EventRequestSpecList

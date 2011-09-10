
package jde.debugger;

import jde.debugger.spec.*;
import jde.debugger.expr.*;

import com.sun.jdi.*;
import com.sun.jdi.request.*;

import java.util.*;

/**
 * GeneralCommands.java
 * <p>
 * Handles general commands. Jump to
 * '{@link #doRun run}', '{@link #doFinish finish}',
 * '{@link #doEvaluate evaluate}'
 * '{@link #doGetObject get_object}', '{@link #doGetArray get_array}',
 * '{@link #doGetLoadedClasses get_loaded_classes}',
 * '{@link #doGetPathInformation get_path_information}',
 * '{@link #doTraceClasses trace_classes}',
 * '{@link #doCancelTraceClasses cancel_trace_classes}',
 * '{@link #doTraceMethods trace_methods}',
 * '{@link #doCancelTraceMethods cancel_trace_methods}'
 *
 * <p>
 * Created: Fri Jul 30 16:29:00 1999
 * 
 * @author Amit Kumar
 * @since 0.1
 */

public class GeneralCommands extends ApplicationCommands {


    
    /**
     * 'finish' command.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * finish
     * </pre>
     *
     * <b>Comments:</b>
     * <ul>
     * <li> if multiple VMs are being debugged, this command will
     * kill the one corresponding to app_id, retaining others.
     * </ul>
     */
    public void doFinish(Integer cmd_id, List args) {
	app.shutdown();
	app.signalCommandResult(cmd_id);
    }


    /**
     * 'trace_classes' command.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * trace_classes <u>type</u>
     *      [{@link Etc#getSuspendPolicyFromArgs(List) suspend-policy}]
     *      [{@link Etc#getClassFiltersFromArgs(List) class-filters}]
     *      [{@link Etc#getClassExFiltersFromArgs(List) class-exclusion-filters}]
     * </pre>
     *
     * <b>Returns:</b>
     * <pre>
     * (jde-dbo-command-result cmd_id <u>requestID</u>)
     * </pre>
     *
     * <b>Comments:</b>
     * <ul>
     * <li> <u>type</u> is either "preparation" or "unloading"
     * <li> use <u>requestID</u> to cancel the trace request.
     * </ul>
     *
     * <p>
     * @see EventHandler#classPrepareEvent(ClassPrepareEvent)
     * @see EventHandler#classUnloadEvent(ClassUnloadEvent)
     */
    public void doTraceClasses(Integer cmd_id, List args)
	throws JDEException {

	if (args.size() < 1)
	    throw new JDEException("Insufficient arguments");

	String type = args.remove(0).toString().toLowerCase();

	if (!(type.equals("preparation") || type.equals("unloading")))
	    throw new JDEException("Invalid type");

	Long requestID = null;
	
	List classFilters = Etc.getClassFiltersFromArgs(args);
	List classExFilters = Etc.getClassExFiltersFromArgs(args);

	EventRequestManager em = app.getVM().eventRequestManager();

	if (type.equals("preparation")) {

	    ClassPrepareRequest cpr = em.createClassPrepareRequest();

	    cpr.setSuspendPolicy(Etc.getSuspendPolicyFromArgs(args));

	    if (classFilters != null) {
		Iterator it = classFilters.iterator();
		while (it.hasNext())
		    cpr.addClassFilter(it.next().toString());
	    }
	    if (classExFilters != null) {
		Iterator it = classExFilters.iterator();
		while (it.hasNext())
		    cpr.addClassExclusionFilter(it.next().toString());
	    }
	    requestID = addIdentifiableRequest(cpr);

	} else if (type.equals("unloading")) {

	    ClassUnloadRequest cur = em.createClassUnloadRequest();

	    cur.setSuspendPolicy(Etc.getSuspendPolicyFromArgs(args));

	    if (classFilters != null) {
		Iterator it = classFilters.iterator();
		while (it.hasNext())
		    cur.addClassFilter(it.next().toString());
	    }
	    if (classExFilters != null) {
		Iterator it = classExFilters.iterator();
		while (it.hasNext())
		    cur.addClassExclusionFilter(it.next().toString());
	    }
	    requestID = addIdentifiableRequest(cur);
	}
	app.signalCommandResult(cmd_id, requestID);
    }

    
    /**
     * 'cancel_trace_classes' command.
     * <p>
     *
     * <b>Syntax: </b>
     * <pre>
     * cancel_trace_classes <u>requestID</u>
     * </pre>
     *
     * <b>Comments:</b>
     * <ul>
     * <li> <u>requestID</u> is returned in the trace classes reply
     * </ul>
     */
    public void doCancelTraceClasses(Integer cmd_id, List args)
	throws JDEException {

	if (args.size() < 1)
	    throw new JDEException("Insufficient arguments");

	deleteIdentifiableRequest(Etc.safeGetLong
				  (args.remove(0), "request ID"));
	
	app.signalCommandResult(cmd_id);
    }


    /**
     * 'trace_methods' command.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * trace_methods <u>type</u>
     *      [{@link Etc#getThreadFromArgs(List) thread-restriction}]
     *      [{@link Etc#getSuspendPolicyFromArgs(List) suspend-policy}]
     *      [{@link Etc#getClassFiltersFromArgs(List) class-filters}]
     *      [{@link Etc#getClassExFiltersFromArgs(List) class-exclusion-filters}]
     * </pre>
     *
     * <b>Returns:</b>
     * <pre>
     * (jde-dbo-command-result cmd_id <u>requestID</u>)
     * </pre>
     *
     * <b>Comments:</b>
     * <ul>
     * <li> <u>type</u> is either "entry" or "exit"
     * <li> Use <u>requestID</u> to cancel the trace request.
     * </ul>
     *
     * <p>
     * @see EventHandler#methodEntryEvent(MethodEntryEvent)
     * @see EventHandler#methodExitEvent(MethodExitEvent)
     */
    public void doTraceMethods(Integer cmd_id, List args)
	throws JDEException {

	if (args.size() < 2)
	    throw new JDEException("Insufficient arguments");

	String type = args.remove(0).toString().toLowerCase();
	if (!(type.equals("entry") || type.equals("exit")))
	    throw new JDEException("Invalid type");

	Object thread = Etc.getThreadFromArgs(args);
	ObjectReference tRef = null;
	if (thread == null) {
	    tRef = null;
	} else if (thread instanceof Long) {
	    tRef = (ObjectReference)store.get(thread);
	    if (tRef == null) {
		throw new JDEException("No such thread exists");
	    } else if (!(tRef instanceof ThreadReference)) {
		throw new JDEException("No such thread exists (anymore?)");
	    }
	} else if (thread instanceof String) {
	    tRef = ThreadCommands.getThread(app.getVM(),
					    thread.toString());
	}

	List classFilters = Etc.getClassFiltersFromArgs(args);
	List classExFilters = Etc.getClassExFiltersFromArgs(args);

	Long requestID = null;
	
	EventRequestManager em = app.getVM().eventRequestManager();

	if (type.equals("entry")) {

	    MethodEntryRequest mer = em.createMethodEntryRequest();

	    if (tRef != null) 
		mer.addThreadFilter((ThreadReference)tRef);

	    mer.setSuspendPolicy(Etc.getSuspendPolicyFromArgs(args));

	    if (classFilters != null) {
		Iterator it = classFilters.iterator();
		while (it.hasNext())
		    mer.addClassFilter(it.next().toString());
	    }
	    if (classExFilters != null) {
		Iterator it = classExFilters.iterator();
		while (it.hasNext())
		    mer.addClassExclusionFilter(it.next().toString());
	    }
	    requestID = addIdentifiableRequest(mer);
	    
	} else if (type.equals("exit")) {

	    MethodExitRequest mer = em.createMethodExitRequest();

	    if (tRef != null) 
		mer.addThreadFilter((ThreadReference)tRef);
	    
	    mer.setSuspendPolicy(Etc.getSuspendPolicyFromArgs(args));
	    
	    if (classFilters != null) {
		Iterator it = classFilters.iterator();
		while (it.hasNext())
		    mer.addClassFilter(it.next().toString());
	    }
	    if (classExFilters != null) {
		Iterator it = classExFilters.iterator();
		while (it.hasNext())
		    mer.addClassExclusionFilter(it.next().toString());
	    }
	    requestID = addIdentifiableRequest(mer);
	}
	app.signalCommandResult(cmd_id, requestID);
    }

    /**
     * 'cancel_trace_methods' command.
     * <p>
     *
     * <b>Syntax: </b>
     * <pre>
     * cancel_trace_methods <u>requestID</u>
     * </pre>
     *
     * <b>Comments:</b>
     * <ul>
     * <li> <u>requestID</u> is returned in the trace methods reply
     * </ul>
     */
    public void doCancelTraceMethods(Integer cmd_id, List args)
	throws JDEException {

	if (args.size() < 1)
	    throw new JDEException("Insufficient arguments");

	deleteIdentifiableRequest(Etc.safeGetLong
				  (args.remove(0), "request ID"));
	
	app.signalCommandResult(cmd_id);
    }	    
    

    /**
     * 'get_object' command. Information about a particular object.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * get_object objectID
     * </pre>
     *
     * <b>Returns:</b>
     * <pre>
     * (jde-dbo-command-result cmd_id {@link Rep#getObjectRep(ObjectReference, ObjectStore) detailed-object-info})
     * </pre>
     */
    public void doGetObject(Integer cmd_id, List args)
	throws JDEException {

	if (args.size() < 1)
	    throw new JDEException("Insufficient arguments");

	Long uniqueID = Etc.safeGetLong(args.remove(0), "object ID");
	ObjectReference oRef = store.get(uniqueID);

	if (oRef == null) 
	    throw new JDEException("No such object exists");

	app.signalCommandResult(cmd_id, Rep.getObjectRep(oRef, store, true));
    }

    

    /**
     * 'get_loaded_classes' command. Returns a list of all loaded classes
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * get_loaded_classes
     * </pre>
     *
     * <b>Returns:</b>
     * <pre>
     * (jde-dbo-command-result cmd_id (list ["type-name"]*))
     * </pre>
     */
    public void doGetLoadedClasses(Integer cmd_id, List args)
	throws JDEException {

	String typeNames = "(list";
	Iterator it = app.getVM().allClasses().iterator();
	while (it.hasNext()) {
	    typeNames += " \""+((ReferenceType)it.next()).name()+"\"";
	}
	typeNames += ")";

	app.signalCommandResult(cmd_id, new LispForm(typeNames));
    }

    /**
     * 'get_path_information' command. Returns all the vm knows about
     * paths.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * get_path_information
     * </pre>
     *
     * <b>Returns:</b>
     * <pre>
     * (jde-dbo-command-result cmd_id "base-directory" (list [boot-class-path component]*) (list [class-path component]*))
     * </pre>
     */
    public void doGetPathInformation(Integer cmd_id, List args)
	throws JDEException {

	if (!(app.getVM() instanceof PathSearchingVirtualMachine))
	    throw new JDEException("VM doesn't search paths");

	PathSearchingVirtualMachine vm =
	    (PathSearchingVirtualMachine)app.getVM();

	String bootClassPathString = "(list";
	Iterator it = vm.bootClassPath().iterator();
	while (it.hasNext()) {
	    bootClassPathString += " \""+it.next()+"\"";
	}
	bootClassPathString += ")";

	bootClassPathString = bootClassPathString.replace('\\', '/');

	String classPathString = "(list";
	it = vm.classPath().iterator();
	while (it.hasNext()) {
	    classPathString += " \""+it.next()+"\"";
	}
	classPathString += ")";
	
	classPathString = classPathString.replace('\\', '/');

	app.signalCommandResult(cmd_id,
				new LispForm("\""+vm.baseDirectory().replace('\\', '/')+"\""
					     + BR +bootClassPathString
					     + BR +classPathString));
    }

    public GeneralCommands(Application a, ObjectStore s) {
	super(a, s);
    }

} // GeneralCommands

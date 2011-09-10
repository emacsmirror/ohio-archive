
package jde.debugger;

import java.util.*;

import com.sun.jdi.*;

/**
 * ObjectStore.java
 * <p>
 *
 * The main function of this class is to keep a store of all the currently
 * referenced objects. Any time jdebug sends an object ID across, it stores
 * the ObjectReference itself in {@link #objectMap}, mapped to the id that
 * will identify this object. This id is the ObjectReference.uniqueID().
 * <p>
 * We need to do this because if we don't keep a link to the ObjectReference
 * <i>some</i>where, it might get garbage collected, and the id used to
 * identify it (ie the uniqueID) reused. If the user then requests info
 * about that ID, he'll be sent information about the new object, which is
 * obviously wrong.
 * <p>
 * When jde wants to know more about the object, it sends across the id,
 * which is used to reference the ObjectReference in the Map
 * <p>
 * Since this is done with each object that's ever reported to jde, the list
 * can get pretty huge, and needs to be refreshed from time to time. For
 * this purpose, we maintain the variable {@link #maximumLimit}.
 * <p>
 * Objects keep getting added to the list, until we
 * reach {@link #maximumLimit}. At this point, a notification is sent to the
 * jde side requesting for a list of all the object references (ie, the ids)
 * that it is currently interested in. 
 * <p>
 * When this list is obtained, the {@link #objectMap} is scanned and entries
 * <i>not</i> in this list removed. {@link #maximumLimit} is then set to
 * 2 times the current size of the list, or the old maximumLimit, whichever
 * is larger. This is to ensure we don't keep sending the request over and
 * over again very frequently.
 * <p>
 * Note that we keep adding objects to the Map even after sending the
 * notification to jde: any reducing the size of the list is only done
 * when jde responds.
 * <p>
 * <b>Note:</b> Question: Should we disable garbage collection of objects
 * in the debugee VM once we put their corresponding ObjectReference in the
 * debugger VM in the objectstore? And maybe enable the gc once they're
 * removed from the store? This way we'll never get the ObjectCollected
 * exception, and we can use the object as long as its displayed on the
 * emacs side. The only thing is, we'd need the emacs side to be pretty
 * frequent about the list of things it is displaying so we don't encumber
 * the debuggee VM too much with objects it's unable to collect.
 * <p>
 * Created: Thu Jul 29 10:38:06 1999
 * 
 * @author Amit Kumar
 * @since 0.1
 */

public class ObjectStore implements Protocol {

    /** my very own application! */
    final Application app;

    /** maps object_id -> ObjectReference */
    private Map objectMap;

    /** maximum number of objects before we send a notification to jde */
    private long maximumLimit = 8;

    /** keep track of if our request has been met yet */
    private boolean requestPending = false;

    /** Create a new object map for a new application */
    public ObjectStore(Application app) {
	this.app = app;
	objectMap = new HashMap();
    }

    /** Register that an object is being sent to the jde side */
    public void put(ObjectReference ref) {
	long size;
	synchronized (this) {
	    objectMap.put(new Long(ref.uniqueID()), ref);
	    size = objectMap.size();
	}
	if (size > maximumLimit) {
	    if (!requestPending) {
		app.signal(REPORT_IDS_IN_USE, null);
		requestPending = true;
	    }
	}
    }

    /**
     * jde sent us a list of objects it is currently interested in. Trim
     * objectMap based on this list
     */
    public void trim(List objectIDs) {
	Map newMap = new HashMap();
	Iterator it = objectIDs.iterator();
	while (it.hasNext()) {
	    Long id = (Long)it.next();
	    synchronized (this) {
		if (objectMap.containsKey(id)) {
		    newMap.put(id, objectMap.get(id));
		}
	    }
	}
	maximumLimit = 2*newMap.size();
	synchronized (this) {
	    objectMap = newMap;
	}
	requestPending = false;
    }

    /** Returns the object corresponding to the id, or null */
    public ObjectReference get(Object id) {
	synchronized (this) {
	    return (ObjectReference)objectMap.get(id);
	}
    }

} // ObjectStore

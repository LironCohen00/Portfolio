

/**
 * A queue class, implemented as a linked list.
 * The nodes of the linked list are implemented as the TaskElement class.
 * 
 * IMPORTANT: you may not use any loops/recursions in this class.
 */
public class TaskQueue {

	TaskElement first;
	TaskElement last;
	
	/**
	 * Constructs an empty queue
	 */
	public TaskQueue(){
		first = null;
		last = null;
	}
	
	/**
	 * Removes and returns the first element in the queue
	 * 
	 * @return the first element in the queue
	 */
	public TaskElement dequeue(){
		TaskElement Removing = first;
		if (!isEmpty()) {
			if (first == last) { // only one node
				first = null;
				last = null;
			} else {
				first = first.prev;
				first.next = null;
			}
		} else {return null;} //if isEmpty()
		return Removing;
	}
	
	/**
	 * Returns and does not remove the first element in the queue
	 * 
	 * @return the first element in the queue
	 */
	public TaskElement peek(){
		return first;
	}
	
	/**
	 * Adds a new element to the back of the queue
	 * 
	 * @param node
	 */
	public void enqueue(TaskElement node){
		if (isEmpty()) {
			first = node;
			last = node;
		}else {
			TaskElement q = last;
			last.prev = node;
			last = node;
			last.next = q;
		}
		
	}
	
	/**
	 * 
	 * @return true iff the queue is Empty
	 */
	public boolean isEmpty() {
		return (first==null);
	}
	
	public TaskElement getLast(){
		return last;
	}
	public void setLast(TaskElement c){
		last = c;
	}
	
	public String toString() {
		TaskElement current = first;
		String s = "";
		while (current != null) {
			s = s + current + " ";
			current = current.prev;
		}
		return s;
	}
}
	


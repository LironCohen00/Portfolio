
/**
 * A wrapper class for Task.
 * This class is intended to both be used as a node in a doubly-linked list,
 * and as an element in a heap.
 * 
 *
 */
public class TaskElement {

	TaskElement next; //the task element which comes after this in the linked list, null if this element is first.
	TaskElement prev; //the task element which comes before this in the linked list, null if this element is last.
	int heapIndex;        //the index of this element in the heap (implemented as an array).
	Task t;
	
	/**
	 * A standard constructor for the class.
	 * Is intended for use before the element is inserted into the list/heap.
	 * 
	 * @param t the Task that the TaskElement contains
	 */
	public TaskElement(Task t){
		this.t = t;
		this.heapIndex = -1;
		this.next = null;
		this.prev = null;
	}
	
	public String toString(){
		return t.toString();
	}
	
	public Task GetTask() {
		return t;
	}
	
	public TaskElement getNext() {
		return next;
	}
	public TaskElement getPrev() {
		return prev;
	}
	public void setNext(TaskElement p) {
		next = p;
	}
	public void setPrev(TaskElement p) {
		prev = p;
	}
}

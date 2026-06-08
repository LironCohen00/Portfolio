/**
 * A task allocation queue which holds the tasks in two parallel queues.
 * One according to priority and one according to order of creation.
 * At any given time, a task at the front of either line may be removed from the data structure.
 * 
 * 
 * 
 *
 */
public class TaskAllocation{

	/*
	 * The heap and queue which compose the data structure.
	 * The most important thing to observe is:
	 * 	A TaskElement exists in the queue if and only if it also exists in the heap.
	 */
	TaskHeap heap;
	TaskQueue q;
	
	/**
	 * Creates an empty task allocation queue
	 */
	public TaskAllocation(){
		heap = new TaskHeap();
		q = new TaskQueue();
	}
	
	
	/**
	 * Creates a task allocation queue from an a array of tasks, ordered according to the creation time 
	 * @param arr a given array of TaskElements. The heapIndex field of the elements in arr might be incorrect
	 */
	public TaskAllocation(TaskElement [] arr){
		this.q=new TaskQueue();
		for(int i=0; i<arr.length;i++) {
			q.enqueue(arr[i]);
		}
		this.heap= new TaskHeap(arr);

	}

	/**
	 * Adds a new Task to the data structure.
	 * The Task is entered (wrapped by a  TaskElement) to the back of the queue
	 *  and into the heap, according to its priority.
	 * 
	 * @param c
	 */
	public void addTask(Task c){
		TaskElement b = new TaskElement(c);
		q.enqueue(b);
		heap.insert(b);
	}
	
	/**
	 * Removes the task with the highest priority from the data structure.
	 * The task must be removed both from the heap and the queue.
	 * 
	 * @return the task with the highest priority.
	 */
	public Task allocatePriorityTask(){
		TaskElement current = q.peek();
		while ((current != heap.findMax()) && (current.getPrev() != null)) { //finding the task in the queue
			current = current.getPrev();
		}
		if (current == q.peek()) { // if we're removing the first in the queue
			q.dequeue();
		}else { 
			TaskElement previous = current.getPrev();
			TaskElement next = current.getNext();
			if (current == q.getLast()) { //if we are removing the last node in the queue, must update the Last field
				q.setLast(next);
			}
			if (previous != null) { // if we are removing node in the middle of the queue
				previous.setNext(next);
			}	 
			next.setPrev(previous);
		}
		return heap.extractMax().GetTask();
	}
	
	/**
	 * Removes the task which was created first to the data structure.
	 * The task must be removed both from the heap and the queue.
	 * 
	 * @return the task which arrived first to the data structure
	 */
	public Task allocateRegularTask(){
		TaskElement task = q.dequeue();
		TaskElement[] heapArr = heap.getHeapArr();
		for (int i = 1; i <= heap.size(); i++) {
			if (heapArr[i] == task) {
				heap.remove(i);
			}
		}
		return task.GetTask();
	}
	
    public static void main (String[] args){
    	/*
    	 * A basic test to check your class.
    	 * Expected outcome:
    	 * task: Solve a problem in production, priority: 100
    	 * task: Add a new feature, priority: 10
    	 * task: Code Review, priority: 3
    	 * task: Analyze performance, priority: 20
    	 * task: Move to the new Kafka server, priority: 2
 
	     */
    
    	Task a = new Task(10, "Add a new feature");
    	Task b = new Task(3, "Code Review");
    	Task c = new Task(2, "Move to the new Kafka server");
     	Task d = new Task(100, "Solve a problem in production");
    	Task e = new Task(20, "Analyze performance");
    	
    	TaskElement [] arr = {new TaskElement(a), new TaskElement(b), new TaskElement(c)};
    	TaskAllocation q = new TaskAllocation(arr);
   
    	q.addTask(d);
    	System.out.println(q.allocatePriorityTask());
    	System.out.println(q.allocatePriorityTask());
		q.addTask(e);
    	System.out.println(q.allocateRegularTask());
    	System.out.println(q.allocatePriorityTask());
    	System.out.println(q.allocateRegularTask());
    }
	
	public String toString() {
		String s = "Heap : \n" + heap + "\n" + "Queue : \n" + q;
		return s;
	}
}


/**
 * A heap, implemented as an array.
 * The elements in the heap are instances of the class TaskElement,
 * and the heap is ordered according to the Task instances wrapped by those objects.
 * 
 * IMPORTANT: Except the percolation (private) functions and the constructors, no single function may loop/recurse through all elements in the heap.
 * 		
 * 
 *
 */
public class TaskHeap{

	public static int capacity=200; // the maximum number of elements in the heap
	/*
	 * The array in which the elements are kept according to the heap order.
	 * The following must always hold true:
	 * 			if i < size then heap[i].heapIndex == i
	 */
	TaskElement[] heap;
	int size; // the number of elements in the heap, it is required that size <= heap.length
	
	/**
	 * Creates an empty heap which can contain 'capacity' elements.
	 */
	public TaskHeap(){
		heap = new TaskElement[capacity + 1];
		size = 0;

	}
	
	/**
	 * Constructs a heap that may contain 'capacity' many elements, from a given array of TaskElements, of size at most 'capacity'. 
	 * This should be done according to the "build-heap" function studied in class.
	 * NOTE: the heapIndex field of each TaskElement might be -1 (or incorrect).
	 * You may NOT use the insert function of heap.
	 * In this function you may use loops.
	 * 
	 */
	public TaskHeap(TaskElement[] arr) {
		this();
		for (int i = 1; i < arr.length + 1; i++) {
			heap[i] = arr[i - 1];
			}
		size = arr.length;	
		//System.out.println(heap);
		for (int i = size/2; i >= 1; i--) {
			PercDown(i,size);
		}
	}
	
    /**
     * Returns the size of the heap.
     *
     * @return the size of the heap
     */
    public int size(){
		return size;
    }

	private void PercDown(int i, int n) {
		if (2*i < n) {
		if ((heap[i].GetTask().compareTo(heap[2*i].GetTask()) == -1 ) || (heap[i].GetTask().compareTo(heap[2*i + 1].GetTask()) == -1 )) {
				if (heap[2*i].GetTask().compareTo(heap[2*i + 1].GetTask()) == 1) {
					TaskElement Temp = heap[i];
					heap[i] = heap[2*i];
					heap[2*i] = Temp;
					PercDown(2*i, n);
				}else {
					TaskElement Temp = heap[i];
					heap[i] = heap[2*i + 1];
					heap[2*i + 1] = Temp;
					PercDown(2*i + 1, n);
				}
			}
		}
		if (2*i == n) {
			if (heap[i].GetTask().compareTo(heap[2*i].GetTask()) == -1) {
				TaskElement Temp = heap[i];
				heap[i] = heap[2*i];
				heap[2*i] = Temp;
			}
		}
	}

	public void PercUp(int i){
		if ( (i != 1) && (heap[i].GetTask().compareTo(heap[i/2].GetTask()) == 1) )  {
				TaskElement Temp = heap[i/2];
				heap[i/2] = heap[i];
				heap[i] = Temp;
				PercUp(i/2);
		}
	}
    
    /**
     * Inserts a given element into the heap.
     *
     * @param e - the element to be inserted.
     */
    public void insert(TaskElement e){
        heap[size + 1] = e;
		size++;
		PercUp(size);
    }
    
    
	
	/**
	 * Returns and does not remove the element which wraps the task with maximal priority.
	 * 
	 * @return the element which wraps the task with maximal priority.
	 */
    public TaskElement findMax(){
		return heap[1];
    }
    
	/**
	 * Returns and removes the element which wraps the task with maximal priority.
	 * 
	 * @return the element which wraps the task with maximal priority.
	 */
    public TaskElement extractMax() {
		TaskElement Max = heap[1];
		remove(1);
		return Max;
    }
    
    /**
     * Removes the element located at the given index.
     * 
	 * Note: this function is not part of the standard heap API.
	 *		 Make sure you understand how to implement it, and why it is required.
	 *       There are several ways this function could be implemented. 
	 * 		 No matter how you choose to implement it, you need to consider the different possible edge cases.
     * @param index
     */
    public void remove(int index){
		if (index == 1) { //root of heap
			heap[1] = heap[size];
			heap[size] = null;
			size--;
			PercDown(1, size);
		} else if (index > size/2) { // a leaf of heap
			heap[index] = heap[size];
			heap[size] = null;
			if (index != size) { // when its not the right-most leaf
				PercUp(index);
			}
			size--;
		} else if ((index <= size/2) && (index > 1)) { //when its a node thats not a leaf or the root
			heap[index] = heap[size];
			heap[size] = null;
			size--;
			if (heap[index].GetTask().compareTo(heap[index/2].GetTask()) == 1) {  //if its greater than its parent
				PercUp(index);
			}
			if ((heap[index].GetTask().compareTo(heap[2*index].GetTask()) == -1 ) || (heap[index].GetTask().compareTo(heap[2*index + 1].GetTask()) == -1 )) {
				PercDown(index,size);
			}
		}
    }
    
	
	public String toString() {
		String s = "";
		for (int i = 1; i < size + 1; i++) {
			s = s + (heap[i]) + " ";
		}
		return s;
	}
    
    public static void main (String[] args){

        	/*
        	 * A basic test for the heap.
        	 * You should be able to run this before implementing the queue.
        	 * 
        	 * Expected outcome: 
        	 * 	task: Add a new feature, priority: 10
    		 *	task: Solve a problem in production, priority: 100
    		 *	task: Solve a problem in production, priority: 100
    		 *	task: Develop a new feature, priority: 10
    		 *	task: Code Review, priority: 3
    		 *	task: Move to the new Kafka server, priority: 2
        	 * 
        	 */
        	
        	Task a = new Task(10, "Add a new feature");
			//System.out.println(a);
        	Task b = new Task(3, "Code Review");
			//System.out.println(b);
        	Task c = new Task(2, "Move to the new Kafka server");
			//System.out.println(c);
        	TaskElement [] arr = {new TaskElement(b), new TaskElement(c), new TaskElement(a)};
        	TaskHeap heap = new TaskHeap(arr);
		//	System.out.println(heap);
        	System.out.println(heap.findMax());
        	
        	Task d = new Task(100, "Solve a problem in production");
        	heap.insert(new TaskElement(d));
			//System.out.println(heap);
			
        	System.out.println(heap.findMax());
			//System.out.println(heap);
			System.out.println(heap.extractMax());
			//System.out.println(heap);
            System.out.println(heap.extractMax());
			//System.out.println(heap);
			System.out.println(heap.extractMax());
			System.out.println(heap.extractMax());
        
        }
	public TaskElement[] getHeapArr() {
		return heap;
	}	
}

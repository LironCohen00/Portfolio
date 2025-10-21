

public class Task {
	int priority;
	String name;
	
	/**
	 * A standard constructor for the task class
	 * 
	 * @param priority
	 * @param name
	 */
	
	public Task(int priority, String name){
		this.name = name;
		this.priority = priority;
	}

	/**
	 * Compares this task to another task.
	 * This task is consider smaller than the other task if and only if
	 * The priority of this task is smaller than the other task or the priorities are equal 
	 * and the name of this task is smaller in the lexicographic ordering than the name of the other task.
	 * 
	 * If this task is smaller returns a negative number. If this task is bigger return a positive number.
	 * If the tasks are equal return 0.
	 * 
	 * 
	 * @param other
	 * @return a negative/positive or zero number of this task is smaller/greater or equal to other
	 */
	public int compareTo(Task other) {
		if (priority < other.priority) {return -1;}
		if (priority > other.priority) {return 1;}
		if (priority == other.priority) {
			if (name.compareTo(other.name) < 0) {return -1;}
			else {return 1;}
		}
		return 0;
	}
	
	public int GetPriority() {
		return priority;
	}
	
	
	public String toString(){
		return "task: " + this.name + ", priority: " + this.priority;
	}
}


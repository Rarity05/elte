import java.util.ArrayList;

public class Signal<T> {
	private T value;
	private ArrayList<ISignalAction<T>> actions;
	
	public Signal() {
		this.actions = new ArrayList<ISignalAction<T>>();
	}
	
	public T getValue() {
		return this.value;
	}
	
	/**
	 * Sets a new value to the Signal
	 * Iterates through the subscribed listeners,
	 * notifying everybody who is interested in this signal
	 * @param value the new value
	 */
	public void setValue(T value) {
		T oldValue = this.value;
		this.value = value;
		
		for (ISignalAction<T> action : actions) {
			try {
				action.onSignalChanged(oldValue, value);
			} catch (Exception e) {
				System.out.println("[ERROR]: There was an error in one of the ISignalActions");
			}
		}
	}
	
	public void subscribe(ISignalAction<T> action) {
		if (action != null) {
			this.actions.add(action);
		}
	}
}

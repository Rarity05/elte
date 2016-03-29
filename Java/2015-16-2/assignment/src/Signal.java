import java.util.ArrayList;
import java.util.function.Function;

public class Signal<T> {
	
	/**
	 * Creates a constant signal
	 * Every time somebody tries to change the value,
	 * the subscribed listener changes it back
	 * @return
	 */
	public static Signal<Integer> createConstantSignal() {
		Signal<Integer> signal = new Signal<Integer>();
		signal.setValue(0);
		signal.subscribe(new ISignalAction<Integer>() {

			@Override
			public void onSignalChanged(Integer oldValue, Integer newValue) {
				if (!oldValue.equals(newValue)) {
					// warning: recursion
					signal.setValue(oldValue);
				}
			}
			
		});
		return signal;
	}
	
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
	
	/**
	 * Creates a new Signal and applies the given Function to its value
	 * @param mapper
	 * @return
	 */
	<R> Signal<R> map(Function<? super T, ? extends R> mapper) {
		Signal<R> signal = new Signal<R>();
		signal.setValue(mapper.apply(this.value));
		this.subscribe(new ISignalAction<T>() {

			@Override
			public void onSignalChanged(T oldValue, T newValue) {
				signal.setValue(mapper.apply(newValue));
			}
			
		});
		
		return signal;		
	}
}

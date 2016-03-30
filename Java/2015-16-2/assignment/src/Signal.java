import java.util.ArrayList;
import java.util.function.BiFunction;
import java.util.function.Function;

public class Signal<T> {
	
	/**
	 * Creates a constant signal
	 * Every time somebody tries to change the value,
	 * the subscribed listener changes it back
	 * @return
	 */
	public static <R> Signal<R> createConstantSignal(final R value) {
		return new Signal<R>(value, true);
	}
	
	private T value;
	private boolean isConstant = false;
	volatile private ArrayList<ISignalAction<T>> actions;
	
	public Signal() {
		this.actions = new ArrayList<ISignalAction<T>>();
	}
	
	public <R> Signal(R value, boolean isConstant) {
		this();
		this.isConstant = true;
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
	synchronized public void setValue(T value) {
		if (this.isConstant) {
			return;
		}
		
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
		
		try {
			action.onSignalChanged(this.value, this.value);
		} catch (Exception e) {
			System.out.println("[ERROR]: There was an error in one of the ISignalActions");
		}
	}
	
	/**
	 * Creates a new Signal and applies the given Function to its value
	 * @param mapper
	 * @return
	 */
	<R> Signal<R> map(Function<? super T, ? extends R> mapper) {
		Signal<R> signal = new Signal<R>();
		this.subscribe(new ISignalAction<T>() {

			@Override
			public void onSignalChanged(T oldValue, T newValue) {
				signal.setValue(mapper.apply(newValue));
			}
			
		});
		
		return signal;		
	}
	
	/**
	 * Creates a new Signal by joining this Signal with the given Signal
	 * The newly created Signal setter is called each time this or the given Signal changes
	 * @param joinSignal the signal we want to join to
	 * @param joiner the function which produces the newly created Signal's type
	 * @return
	 */
	<R, K> Signal<R> join(Signal<K> joinSignal, BiFunction<T,K,R> joiner) {
		Signal<R> signal = new Signal<R>();
		Signal<T> _this = this;
		
		// If this signal changes, we want to update the joined signal
		this.subscribe(new ISignalAction<T>() {

			@Override
			public void onSignalChanged(T oldValue, T newValue) {
				signal.setValue(joiner.apply(newValue, joinSignal.getValue()));
			}
			
		});
		
		// If the given signal changes, we want to update the joined signal
		joinSignal.subscribe(new ISignalAction<K>() {

			@Override
			public void onSignalChanged(K oldValue, K newValue) {
				signal.setValue(joiner.apply(_this.getValue(), newValue));
			}
			
		});
		
		return signal;
	}
	
	/**
	 * Does stuff.
	 * @param accumulater
	 * @param initValue
	 * @return
	 */
	<R> Signal<R> accumulate(BiFunction<R,T,R> accumulater, final R initValue) {
		Signal<R> signal = new Signal<R>();
		this.subscribe(new ISignalAction<T>() {

			@Override
			public void onSignalChanged(T oldValue, T newValue) {
				signal.setValue(accumulater.apply(signal.getValue() == null ? initValue : signal.getValue(), newValue));	
			}
			
		});
		
		return signal;
	}
}

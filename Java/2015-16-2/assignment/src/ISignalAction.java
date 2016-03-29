public interface ISignalAction<T> {
	public void onSignalChanged(T oldValue, T newValue);
}

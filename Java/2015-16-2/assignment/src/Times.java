public enum Times {
	HALF_SECONDS(500), SECONDS(1000), HOURS(1000*60*60);
	
	private int value;
	private Times(int value) {
		this.value = value;
	}
	
	public int getValue() {
		return this.value;
	}

}

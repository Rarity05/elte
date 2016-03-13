import static org.junit.Assert.*;

import java.util.ArrayList;

import org.junit.Test;

public class BiMapTest {
	
	public class BigInt implements Comparable<BigInt> {

		private int bigInt;
		
		@Override
		public int compareTo(BigInt other) {
			return Integer.compare(this.bigInt, other.getNumber());
		}
		
		public BigInt(int number) {
			this.bigInt = number;
		}
		
		public int getNumber() {
			return this.bigInt;
		}
		
	}
	
	public class TinyInt extends BigInt {

		public TinyInt(int number) {
			super(number);
			if (number > 100) {
				throw new IllegalArgumentException("too big");
			}
		}
		
	}

	@Test
	public void test() {
		BiMap<BigInt, BigInt> ten_pow = BiMap.create();
		
		ArrayList<TinyInt> tinyList = new ArrayList<TinyInt>();
		ArrayList<BigInt> bigList = new ArrayList<BigInt>();
		
		for (int i=0; i<50; i++) {
			tinyList.add(new TinyInt(i));
			bigList.add(new BigInt(i+1000));
		}
		
		ten_pow.merge(tinyList, bigList);
		
		for (int i=0; i<50; i++) {
			int bigInt = ten_pow.getByKey(tinyList.get(i)).getNumber();
			int tinyInt = ten_pow.getByValue(bigList.get(i)).getNumber();
			assertEquals(true, bigInt == tinyInt + 1000);
		}
	}

}

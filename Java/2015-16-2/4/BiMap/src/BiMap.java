import java.util.Comparator;
import java.util.List;
import java.util.TreeMap;

public class BiMap<K, V> {
	
	public static <K extends Comparable<? super K>, V extends Comparable<? super V>> BiMap<K, V> create() {
		return new BiMap<K, V>();
	}
	
	public static <K, V> BiMap<K, V> create(Comparator<K> k, Comparator<V> v) {
		return new BiMap<K, V>(k, v);
	}
	
	private TreeMap<K, V> keys;
	private TreeMap<V, K> values;
	
	public BiMap() {
		this.keys = new TreeMap<K, V>();
		this.values = new TreeMap<V, K>();
	}
	
	public BiMap(Comparator<? super K> k, Comparator<? super V> v) {
		this.keys = new TreeMap<K, V>((k1,k2) -> k.compare(k1, k2));
		this.values = new TreeMap<V, K>(v);
	}

	public boolean add(K key, V value) {
		if (keys.containsKey(key)) {
			return false;
		}
		
		keys.put(key, value);
		values.put(value, key);
		
		return true;
	}
	
	public K getByValue(V value) {
		return values.get(value);
	}
	
	public V getByKey(K key) {
		return keys.get(key);
	}
	
	public void merge(List<? extends K> listA, List<? extends V> listB) {
		if (listA.size() == listB.size()) {
			for (int i = 0; i < listA.size(); i++) {
				this.add(listA.get(i), listB.get(i));
			}
		}
	}
}

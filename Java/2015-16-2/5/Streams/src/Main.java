import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Iterator;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Main {

	public static void main(String... args) throws IOException {
	    Path path = Paths.get("C:\\Users\\toduabi\\Desktop");
	    listStream(createStream(Files.list(path)));
	}

	public static Stream<Path> createStream(Stream<Path> stream) throws IOException {
	    
	    Set<Path> s = stream.collect(Collectors.toSet());
	    if (s.isEmpty()) return Stream.empty();
	    
	    Stream<Path> st = s.stream();	    
	    Stream<Path> dirsOnly = st.filter(Files::isDirectory);
	    
	    st = s.stream();
	    Stream<Path> filesOnly = st.filter(Files::isRegularFile);
	    
	    return Stream.concat(filesOnly, createStream(dirsOnly));	    
	    
	    
	    /*Iterator<Path> it = stream.iterator();
	    while (it.hasNext()) {
	    	Path next = it.next();
	    	if (Files.isDirectory(next)) {
	    		retVal = Stream.concat(retVal, createStream(Files.list(next)));
	    	} else {
	    		retVal = Stream.concat(retVal, Stream.of(next));
	    	}
	    }
	    return retVal;*/
	}
	
	public static void listStream(Stream<Path> stream) {
		stream.forEach(System.out::println);
	}

}

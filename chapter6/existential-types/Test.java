import java.util.*;

// This class demonstrates how Java treats existential types.
public class Test {
   public static void main(String[] args) {
     List foo = new ArrayList();
     List<Object> bar = foo;   // When compiled with -Xlint this gives a warning.
   }
   public static List makeList() {
      return new ArrayList();
   }
}

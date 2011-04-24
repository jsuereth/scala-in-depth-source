
class JavaTest {
  public static void main(String[] args) {
    ScalaUtils.log("Hello!");
    ScalaUtils$.MODULE$.log("Hello!");
    System.out.println(ScalaUtils.MAX_LOG_SIZE());
    System.out.println(ScalaUtils$.MODULE$.MAX_LOG_SIZE());
  }
}

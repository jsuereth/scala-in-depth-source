class JavaFunction {
  public static void main(String[] args) {
	System.out.println(FunctionUtil.testFunction((scala.Function1<Integer,Integer>)new AbstractFunctionIntIntForJava() {
      public Integer apply(Integer argument) {
        return argument + 5;
      }
    }));
  }
}

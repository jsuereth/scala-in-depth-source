trait FooHolder 
{
  def foo() 

  {
    println("foo was called")
  }
}


trait FooHolder2
{
  def foo() : Unit =
  
  {
    println("foo2 was called")
  }
}

trait IfIssues {
  if(true)
  {
    println("true!")
  }
  else
  {
    println("false!")
  }
}


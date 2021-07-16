object recursionFP extends App{
  def gcd(x : Int,y :Int):Int = y match {
    case 0 => x
    case y if (y>x) => gcd(y,x)
    case _ => gcd(y,x%y)
  }
  def prime(p:Int,n:Int=2):Boolean = n match{
    case x if(p==x) =>true
    case x if (gcd(p,x)>1) => false
    case x => prime(p,x+1)
  }
  def primeSeq(n:Int):Any={
    if(prime(n)==true ) print(n+"\n")
    if (n>0) primeSeq(n-1)
  }
  def sum(n:Int):Int={
    if(n==1) 1
    else n+sum(n-1)

  }
  def isEven(n:Int):Boolean = n match{
    case 0 => true
    case _ => isOdd(n-1)
  }

  def isOdd(n:Int):Boolean = !isEven(n)

  def numSum(n:Int,m:Int):Int={
    if(n>m) return 0 else return n+numSum(n+2,m)

  }
  def f(n:Int) : Int = n match {
    case n if n==0 =>0
    case n if n==1 => 1
    case n => f(n-1)+f(n-2)
  }
  def fsq(n:Int):Any = {
    if(n>0) fsq(n-1)
    println(f(n))
  }

  print("Question number 01 : \n")
  print(prime(5)+"\n")
  print(prime(8)+"\n")
  print("\n")
  print("Question number 06 : \n")
  fsq(10)
  print("\n")
  print("Question number 05 : \n")
  print(numSum(0,10))
  print("\n")
  print("Question number 04 : \n")
  print(isOdd(5)+"\n")
  print(isEven(4)+"\n")
  print("\n")
  print("Question number 03 : \n")
  print(sum(3)+"\n")
  print("\n")
  print("Question number 02 : \n")
  primeSeq(10)

}

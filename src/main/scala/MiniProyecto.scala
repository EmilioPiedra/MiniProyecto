// Simpson 1/3 Simple
def integracionSimple( f:Double=>Double, a:Double, b:Double):Double = {
  (b-a)*((f(a)+4*f((a+b)/2)+f(b))/6)
}
val ejem1 = integracionSimple(x => (-math.pow(x,2) + 8*x  - 12), 3, 5)
val ejem2 = integracionSimple(x => 3*(math.pow(x,2)), 0, 2)
val ejem3 = integracionSimple(x => x+2*(math.pow(x,2))-(math.pow(x,3))+5*(math.pow(x,4)),1,-1)
val ejem4 = integracionSimple(x => (2*x+1)/((math.pow(x,2))+x) ,1,2)
val ejem5 = integracionSimple(x => math.pow((math.E),x) ,0, 1)
val ejem6 = integracionSimple(x => 1 / math.sqrt(x-1),2,3)
val ejem7 = integracionSimple(x => 1 / 1 + math.pow(x,2),0,1)
//---------------------------------------------------------------------------------------------------
//simpson 1/3 compuesta
def integracionCompuesta (a : Int, b: Int, num : Int ,f :Double=> Double):Double = {
  val h = ( b - a )/num.toDouble
  val xj = ( j : Double) => (a + j*h)
  val funciones = (j : Double) => f(xj(2 * j - 2)) + (4 * f(xj(2 * j - 1))) + (f(xj(2 * j)))
  ((1 to num/2).map(funciones(_)).sum)*h/3
}
val e1 = integracionCompuesta(3,5,8,((x: Double) => (-math.pow(x,2) + 8*x  - 12)))
val e2 = integracionCompuesta(0,2,10,((x: Double) => 3*(math.pow(x,2))))
val e3 = integracionCompuesta(1,-1,10,((x: Double) => x+2*(math.pow(x,2))-(math.pow(x,3))+5*(math.pow(x,4))))
val e4 = integracionCompuesta(1,2,10,((x: Double) => (2*x+1)/((math.pow(x,2))+x)))
val e5 = integracionCompuesta(0,1,10,((x: Double) => math.pow((math.E),x)))
val e6 = integracionCompuesta(2,3,10,((x: Double) => 1 / math.sqrt(x-1)))
val e7 = integracionCompuesta(0,1,10,((x: Double) => 1 / 1 + math.pow(x,2)))
//----------------------------------------------------------------------------
//simpson 1/3 extendida
def integracionExtendida (a:Int,b: Int,f: Double=>Double):Double = {
  val n =2*(b-a)
  val h =(b-a)/n.toDouble
  val x = ( n : Double) => f(a + n*h)
  val funciones = f(a) + 4 * (1 to n-1 by 2).map(x(_)).sum + 2 *(2 to n-2 by 2).map(x(_)).sum +f(b)
  h/3*funciones
}
val ej1 =integracionExtendida(3,5,((x: Double) => (-math.pow(x,2) + 8*x  - 12)))
val ej2 =integracionExtendida(0, 2,((x: Double) => 3*(math.pow(x,2))))
val ej3 =integracionExtendida(1,-1,((x: Double) => x+2*(math.pow(x,2))-(math.pow(x,3))+5*(math.pow(x,4))))
val ej4 =integracionExtendida(1,2,((x: Double) => (2*x+1)/((math.pow(x,2))+x)))
val ej5 =integracionExtendida(0,1,((x: Double) => math.pow((math.E),x)))
val ej6 =integracionExtendida(2,3,((x: Double) => 1 / math.sqrt(x-1)))
val ej7 =integracionExtendida(0,1,((x: Double) => 1 / 1 + math.pow(x,2)))
@main def app(): Unit =
  println("========================\nSimpson 1/3 Simple\n========================\n" +
    ejem1 + "\n" + ejem2 + "\n" + ejem3 + "\n" + ejem4 + "\n" + ejem5 + "\n" + ejem6 + "\n" + ejem7)
  println("========================\nSimpson 1/3 Compuesta\n========================\n" +
    e1 + "\n" + e2 + "\n" + e3 + "\n" + e4 + "\n" + e5 + "\n" + e6 + "\n" + e7)
  println("========================\nSimpson 1/3 Extendida\n========================\n" +
    ej1 + "\n" + ej2 + "\n" + ej3 + "\n" + ej4 + "\n" + ej5 + "\n" + ej6 + "\n" + ej7)
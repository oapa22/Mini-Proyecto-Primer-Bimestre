//Simpson 1/3
def integracion(a : Int, b : Int, f : Double => Double) = {
  val z = (a + b) / 2.0
  (b-a) * ( ( f(a) + (f(z)*4) + f(b) ) / 6 )
}

val f = (x : Double) => -Math.pow(x,2) + 8 * x - 12
integracion(3, 5, f)

val f2 = (x : Double) =>  3 * (x * x)
integracion(0, 2, f2)

val f3 = (x : Double) =>  x + 2*Math.pow(x,2) - Math.pow(-x,3) + 5*Math.pow(x,4)
integracion(-1, 1, f3)

val f4 = (x : Double) =>  (2 * x + 1) / ( Math.pow(x, 2) + x)
integracion(1, 2, f4)

val f5 = (x : Double) =>  Math.pow(Math.E,x)
integracion(0, 1, f5)

val f6 = (x : Double) =>  (1) / (Math.sqrt( x - 1))
integracion(2, 3, f6)

val f7 = (x : Double) => ( 1 / (1 + ( x * x ) ) )
integracion(0, 1, f7)

//Calculo error de la funcion 7
def calculoError(est : Double, func : Double) : Double = (est - func).abs
calculoError(0.785398, integracion(3, 5, f7))


//Simpson Compuesta

//Simpson Extendida


















//Funciones
val f1 = (x : Double) => -Math.pow(x,2) + 8 * x - 12
val f2 = (x : Double) =>  3 * (x * x)
val f3 = (x : Double) =>  x + 2*Math.pow(x,2) - Math.pow(-x,3) + 5*Math.pow(x,4)
val f4 = (x : Double) =>  (2 * x + 1) / ( Math.pow(x, 2) + x)
val f5 = (x : Double) =>  Math.pow(Math.E,x)
val f6 = (x : Double) =>  (1) / (Math.sqrt( x - 1))
val f7 = (x : Double) => ( 1 / (1 + ( x * x ) ) )

//Función para calcular error
def calculoError(est : Double, func : Double) : Double = (est - func).abs


//                                   Simpson 1/3
def simpson1tercio(a : Int, b : Int, f : Double => Double) = {
  val z = (a + b) / 2.0
  (b-a) * ( ( f(a) + (f(z)*4) + f(b) ) / 6 )
}

//Resultados
simpson1tercio(3, 5, f1)
simpson1tercio(0, 2, f2)
simpson1tercio(-1, 1, f3)
simpson1tercio(1, 2, f4)
simpson1tercio(0, 1, f5)
simpson1tercio(2, 3, f6)
simpson1tercio(0, 1, f7)

//Calculo error
calculoError(7.33, simpson1tercio(3, 5, f1))
calculoError(8, simpson1tercio(0, 2, f2))
calculoError(3.333, simpson1tercio(-1, 1, f3))
calculoError(1.09861, simpson1tercio(1, 2, f4))
calculoError(1.71828, simpson1tercio(0, 1, f5))
calculoError(0.828427, simpson1tercio(2, 3, f6))
calculoError(0.785398, simpson1tercio(0, 1, f7))

//                                  Simpson Compuesta
def simpsonCompuesta(a : Int, b : Int, n : Int, f : Double => Double) = {
  val h = 1.0 * (b-a) / n
  val xsubj = (j : Double) => (a + (j*h))
  val funcionDentro = (j : Double) => ( f( xsubj (2*j-2) ) + 4*f( xsubj (2*j-1)) + f( xsubj (2*j)) )
  (h/3) * (1 to n/2).map(funcionDentro(_)).sum
}

//Resultados
simpsonCompuesta(3, 5, 10, f1)
simpsonCompuesta(0, 2, 10, f2)
simpsonCompuesta(-1, 1, 10, f3)
simpsonCompuesta(1, 2, 10, f4)
simpsonCompuesta(0, 1, 10, f5)
simpsonCompuesta(2, 3, 10, f6)
simpsonCompuesta(0, 1, 10, f7)

//Calculo error
calculoError(7.33, simpsonCompuesta(3, 5, 10, f1))
calculoError(8, simpsonCompuesta(0, 2, 10, f2))
calculoError(3.333, simpsonCompuesta(-1, 1, 10, f3))
calculoError(1.09861, simpsonCompuesta(1, 2, 10, f4))
calculoError(1.71828, simpsonCompuesta(0, 1, 10, f5))
calculoError(0.828427, simpsonCompuesta(2, 3, 10, f6))
calculoError(0.785398, simpsonCompuesta(0, 1, 10, f7))


//                                  Simpson Extendida


















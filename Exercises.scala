
object Exercises {

	type Row = List[Int]
  	type Matrix = List[Row]
  	

	// 1 - Sum
    def sum(x: Int): Int = {
    	require(x >= 0, "Please give a number greater or equal to 0.")
    	if (x == 0) 0
        else x + sum(x - 1)
	}
	//Exercises.sum(10)	//55


	// 2 - Factorial
	def factorial(x: Int): Int = {
		require(x >= 0, "Please give a number greater or equal to 0.")	
        if (x == 0) 1
        else x * factorial(x - 1)
	}
	//Exercises.factorial(7)	//5040


	// 3 - Digits
	def digits(x: Int): Int = {
    	if (x / 10 == 0) 1
        else 1 + digits(x / 10)
	}
	//Exercises.digits(3465)	//4


	// 4 - Sum of digits
	def sumDigits(x: Int): Int = {
		if (x >= 0) 
			if (x / 10 == 0) x
      		else x % 10 + sumDigits(x / 10)
      	else sumDigits(-x)	
	}
	//Exercises.sumDigits(3465)	//18


	// 5 - Greatest Common Divisor
	def gcd(x: Int, y: Int): Int = {
		require(x >= 1, "Please give numbers greater than 0.")
		require(y >= 1, "Please give numbers greater than 0.")
    	if (x == y) x
        else
      		if (x > y) gcd(x - y, y)
      		else gcd(x, y - x)
    }
	//Exercises.gcd(10,55)	//5


	// 6 - Fibonacci
	def fibonacci(x: Int): Int = {
		require(x >= 0, "Please give a number greater or equal to 0.")
		if ((x == 0) || (x == 1)) x
		else fibonacci(x - 1) + fibonacci(x - 2)
	}
	//Exercises.fibonacci(12)	//144


	// 7 - Pascal triangle
	def pascal(x: Int, y: Int): Int = {
		if ((x == 0) && (y == 0)) 1
		else if ((x < 0) || (y < 0) || (x < y)) 0
		else pascal(x - 1, y - 1) + pascal(x - 1, y)
	}
	//Exercises.pascal(8, 4)	//35


	// 8 - Currified function
	val currified: (Double => (Double => (Double => Double))) =
	(a) => { (b) => { (c) => { a*b*b+2*a*b*c+a*c*c } } }
	//val f1 = Exercises.currified(5)
	//val f2 = f1(3)
	//f2(4) // = 245
	//Exercises.currified(5)(3)(4) // = 245


	// 9 - Summ elements
	def summ1(l: List[Int]): Int = {
		if (l == Nil) 0
		else l.head + summ1(l.tail)
	}
	//Exercises.summ1(List(1, 2, 3, 4, 5)) //15

	val summ2: (List[Int] => Int) = (l) => {
		if (l == Nil) 0
		else l.head + summ2(l.tail)
	}
	//Exercises.summ2(List(45, -3, 8, -23)) //27


	// 10 - Square elements
	def square1(l: List[Int]): List[Int] = {
		if (l == Nil) Nil
		else (l.head * l.head) :: square1(l.tail)
	}

	def square2(l: List[Int]): List[Int] = l match {
		case Nil => Nil
		case h :: t => (h * h) :: square2(t)	
	}

	val square3: (List[Int] => List[Int]) = /*(l: List[Int]) => l match*/ {
		case Nil => Nil
		case h :: t => (h * h) :: square3(t)	
	}

	val square4: (List[Int] => List[Int]) = 
					  /*(l) => l match*/ {case Nil => Nil; case h :: t => (h * h) :: square4(t)}
    //Exercises.squareN(List(12, 56, 32))    //List(144, 3136, 1024)


	// 11 - Largest element
	def largest(l: List[Int]): Int = l match {
  		case Nil => -1
  		case List(x: Int) => x	//case x :: Nil => x
		case x :: y :: rest => largest((if (x > y) x else y) :: rest)
	}	
	//Exercises.largest(List(-1, 56, 34, -43, 4, 110)) // = 110


	// 12 - Reverse
	def reverse(l: List[Int]): List[Int] = l match {
		case Nil => Nil
		case h :: t => reverse(t) :+ h
	}	
	//Exercises.reverse(List(0, 34, -43, 4, 110)) // = List(110, 4, -43, 34, 0)


	// 13 - Map
	def map(l: List[Int], f: Int => Int): List[Int] = l match {
		case Nil => Nil
		case h :: t => f(h) :: map(t, f)	
	}
	//Exercises.map(List(1, 2, 3, 4, 5), x => x * x) // = List(1, 4, 9, 16, 25)


	// 14 - Sorted
	def sorted(l: List[Int], f: (Int, Int) => Boolean): Boolean = l match {
		case Nil => false
		case List(x: Int) => true
		case a :: b :: rest => if (f(a, b) == true) sorted(b :: rest, f)
							   else false
	}
	//Exercises.sorted(List(1, 2, 3, 4, 5, 3), (a, b) => a < b) // = false


	// 15 - Concatenation
	def concatenate(l1: List[Int], l2: List[Int]): List[Int] = l1 ::: l2
	//Exercises.concatenate(List(1, 2, 3), List(4, 5)) // = List(1, 2, 3, 4, 5)


	// 16 - Zip
	def zip(l1: List[Int], l2: List[Int]): List[(Int, Int)] = {
		if ((l1 == Nil) || (l2 == Nil)) Nil
		else (l1.head, l2.head) :: zip(l1.tail, l2.tail)
	}
	//Exercises.zip(List(1, 2, 3), List(6, 3, 4)) // = List((1, 6), (2, 3), (3, 4))


	// 17 - Dot product
	def dot(l1: List[Int], l2: List[Int]): Int = {
		if (l1 == Nil) 0
		else l1.head * l2.head + dot(l1.tail, l2.tail)	
	}	
	//Exercises.dot(List(1, 2, 3, 4), List(9, 8, 7, 6)) // = 70

	def dotTR(l1: List[Int], l2: List[Int]): Int = {
		def f(l1: List[Int], l2: List[Int], x: Int): Int = {
			if (l1 == Nil) x
			else f(l1.tail, l2.tail, x + l1.head * l2.head)	
		}
		f(l1, l2, 0)
	}
	//Exercises.dotTR(List(1, 2, 3, 4), List(9, 8, 7, 6)) // = 70


	// 18 - Scale
	def scale1(min: Double, max: Double)(a: Double, b: Double)(x: Double): Double = {
		( ( (b - a) * (x - min) ) / (max - min) ) + a
	}
	//val a: List[Double] = List(23, 73, 5, 43, 18, 84, 3)
	//Exercises.scale1(a.min, a.max)(0, 1)(3) // = 0.0
	//Exercises.scale1(a.min, a.max)(0, 1)(84) // = 1.0
	//val s = Exercises.scale1(a.min, a.max) _
	//s(0, 1)(a.head) // = 0.246913...
	//s(10, 20)(a.head) // = 12.46913...
	//a.map(s(0, 10)) // = List(2.469, 8.641, 0.246, 4.938, 1.851, 10.0, 0.0)

	val scale2: ( (Double, Double) => ( (Double, Double) => (Double => Double))) = 
		(min, max) => { (a, b) => { (x) => { ( ( (b - a) * (x - min) ) / (max - min) ) + a } } }
	//val a: List[Double] = List(23, 73, 5, 43, 18, 84, 3)
	//Exercises.scale2(a.min, a.max)(0, 1)(3) // = 0.0
	//Exercises.scale2(a.min, a.max)(0, 1)(84) // = 1.0
	//val s = Exercises.scale2(a.min, a.max)
	//s(0, 1)(a.head) // = 0.246913...
	//s(10, 20)(a.head) // = 12.46913...
	//a.map(s(0, 10)) // = List(2.469, 8.641, 0.246, 4.938, 1.851, 10.0, 0.0)


	// 19 - Filter
	def filter(l: List[Int], f: Int => Boolean): List[Int] = l match {
		case Nil => Nil
		case h :: t => if(f(h) == true)	h :: filter(t, f)
					   else filter(t, f)	
	}
	//Exercises.filter(List(60, 23, 11, 76, 42, 9), x => x > 50) // = List(60, 76)


	// 20 - Reduce
	def reduce(l: List[Int], f: (Int, Int) => Int): Int = l match {
		case Nil => -1
		case a :: Nil => a
		case a :: b :: rest => reduce((f(a, b) :: rest), f)
	}
	//Exercises.reduce(List(23, 76, 34, 84, 24, 58), (a, b) => a.max(b)) // = 84


	// 21 - Nth smallest number
	def smallest(l: List[Int], nth: Int): Int = {

	  	// insert(1,List(3)) = 3 :: insert(1,Nil) = 3 :: 1 :: Nil	
	  	def insert(e: Int, sn: List[Int]): List[Int] = sn match {
		    case Nil => e :: Nil
		    case h :: t => if (e > h) e :: sn else h :: insert(e, t)
	  	}

	  	def loop(l2: List[Int], smallest: List[Int], size: Int): Int = l2 match {
		    case Nil => smallest.head
		    case h :: t => if (size < nth) loop(t, insert(h, smallest), size + 1)
		    			else if (h < smallest.head) loop(t, insert(h, smallest.tail), size)
		    			else loop(t, smallest, size)
	  	}

	  	loop(l, Nil, 0)
	}
	//val ns = List(3, 7, 1, 9)
	//Exercises.smallest(ns, 2) // = 3 (second smallest)

	/*
	smallest(List(3, 7, 1, 9), 2)
	= loop(List(3, 7, 1, 9), Nil, 0)
	= loop(List(7, 1, 9), insert(3, Nil), 1)               insert -> List(3)
	= loop(List(1, 9), insert(7, List(3)), 2)              insert -> List(7,3)
	= loop(List(9), insert(1, List(3)), 2)                 insert -> List(3,1)
	= loop(Nil, List(3, 1), 2)
	= 3
	*/


	// 22 - Balanced
	def balanced(a: List[Char]): Boolean = {
  		
  		def counter(a: List[Char], countL: Int, countR: Int): Boolean = {
    		if (a == Nil) if(countL == countR) true else false
    		else if (a.head == '(') counter(a.tail, countL + 1, countR) 
    		else if(a.head == ')') counter(a.tail, countL, countR + 1) 
    		else counter(a.tail, countL, countR)
    	}

    	counter(a, 0, 0)
	}
	//Exercises.balanced("hello (world)".toList) // = true (balanced).


	// 23 - Combinations of 0s and 1s
	def combinations(n: Int): List[List[Int]] = {

		def numToBin(l: List[Int], n: Int): List[Int] = {
			if ((n == 0) || (n == 1)) List(n) ::: l.tail
			else n % 2 :: numToBin(l.tail, n / 2)
		}

		def loop(l: List[Int], n: Int, acum: Int): List[List[Int]] = {
			if (acum >= Math.pow(2, n)) Nil
			else List(numToBin(l, acum).reverse) ::: loop(List.fill(n)(0), n, acum + 1)
		}

		loop(List.fill(n)(0), n, 0)
	}
	//Exercises.combinations(2) // = List(List(0, 0), List(0, 1), List(1, 0), List(1, 1))


	// 24 - Count change
	def countChange(x: Int, l: List[Int]): Int = {
		
		def loop(a: Int, b: List[Int], c: Int): Int = {
			if (a < 0) c
			else
				if (b.isEmpty)
					if (a == 0)	c + 1
					else c
				else loop(a - b.head, b, c) + loop(a, b.tail, c)
		}

		loop(x, l, 0)
	}
	//Exercises.countChange(12, List(2, 3, 4)) // = 7


	// 25 - Fibonacci list
	def fibonacciList(a: Int): List[Int] = {

		def fib(n: Int): Int = {
  			if ((n == 1)||(n == 2)) 1  
  			else fib(n - 1) + fib(n - 2)
		}

  		if (a == 0) Nil 
  		else fib(a) :: fibonacciList(a - 1)
	}
	//Exercises.fibonacciList(10) // = List(55, 34, 21, 13, 8, 5, 3, 2, 1, 1)


	// 26 - Quicksort
	def quicksort(L: List[Int]): List[Int] = {
  		if (L.length <= 1) L 
  		else quicksort(L.filter((i: Int) => i < L.last)) ::: L.last :: quicksort(L.filter((i: Int) => i > L.last ))
	}
	//Exercises.quicksort(List(1, 3, 2, 5, 4)) // = List(1, 2, 3, 4, 5)

	
	// 27 - Stream of natural numbers
	def streamFrom(n: Int): Stream[Int] = n #:: streamFrom(n + 1)
	//Exercises.streamFrom(11).take(8).toList // = List(11, 12, 13, 14, 15, 16, 17, 18)


	// 28 - Moving average
	def averages(n: Int, s: Stream[Double]): Stream[Double] = (s.take(n).sum / n) #:: averages(n, s.tail)
	//val s = Stream(3.2, 5.3, 5.6, 4.4, 3.4, 7.5, 6.2, 9.4, 2.3)
	//Exercises.averages(3, s).take(3).toList // = List(4.7, 5.1, 4.466666666666667)


	// 29 - Leibniz sequence for Pi
	def leibnizStream(): Stream[Double] = {
		def leibnizStream2(n: Int = 0): Stream[Double] = (math.pow(-1, n) / ((2 * n) + 1)) #:: leibnizStream2(n + 1)
		leibnizStream2()
	}
	//Exercises.leibnizStream().take(1000).sum * 4 // = 3.140592653839794


	// 30 - Fibonacci stream
	def fibonacciStream(): Stream[Int] = {
  		def fibonacciStream2(x: Int, y: Int): Stream[Int] = (x + y) #:: fibonacciStream2(y, x + y)
  		1 #:: 1 #:: fibonacciStream2(1, 1)
	}
	//Exercises.fibonacciStream.take(8).toList // = List(1, 1, 2, 3, 5, 8, 13, 21)


	// 31 - Sieve of Eratosthenes
	def primesStream(): Stream[Int] = {
		def sieve(s: Stream[Int]): Stream[Int] = s.head #:: sieve(s.tail.filter(_ % s.head != 0))
		sieve(Stream.from(2))
	}
	//Exercises.primesStream.take(10).toList	//List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)


	// 32 - Minkowski distance
	val minkowski: (Double => ((List[Double], List[Double]) => Double)) = (a) => { (M, N) => {

		def **(i: Double): Double = 1/i

    	def mink(a: Double, M: List[Double], N: List[Double], Total: Double = 0): Double = {
        	if (M == Nil) Total 
        	else mink(a: Double, M.tail, N.tail, Total + math.pow(math.abs(M.head - N.head), a))
    	}

    	math.pow(mink(a: Double, M: List[Double], N: List[Double]), **(a))
	}}
	//val a = List(1.0, 2.0, 3.0, 4.0, 5.0)
	//val b = List(1.0, 2.0, 1.0, 2.0, 5.0)
	//val euclidean = Exercises.minkowski(2)
	//val manhattan = Exercises.minkowski(1)
	//Exercises.minkowski(1.5)(a, b) // = 3.1748021039363987
	//euclidean(a, b) // = 2.8284271247461903
	//manhattan(a, b) // = 4.0


	// 33 - Merge and reduce
  	def mergeReduce(a: List[Int], b: List[Int], f1: (Int, Int) => Int, f2: (Int, Int) => Int): Int = {

  		/* merge function */
    	def merge(x: List[Int], y: List[Int], f: (Int, Int) => Int): List[Int] = x match {
    		case Nil => Nil
    		case h :: t => if (!y.isEmpty) f(h, y.head) :: merge(t, y.tail, f) else Nil
    	}

    	/* reduce function */
    	def reduce(l: List[Int], f: (Int, Int) => Int): Int = l match {
    		case Nil => -1
			case a :: Nil => a
			case a :: b :: Nil => f(a, b)
			case a :: b :: rest => reduce((f(a, b) :: rest), f)
    	}

    	require(a != Nil, "Please give non empty lists.")
    	require(b != Nil, "Please give non empty lists.")

    	/* first we apply merge, and then we apply reduce */
    	reduce(merge(a, b, f1), f2)
  	}
  	//val a = List(3, 7, 2, 9)
	//val b = List(1, 8, 4, 6)
	//val prod = (x: Int, y: Int) => x * y // merge function
	//val sum = (x: Int, y: Int) => x + y // reduce function
	//Exercises.mergeReduce(a, b, prod, sum) // = 121


	// 34 - Z-Score
	def countAndMean(l: List[Double]): (Int, Double) = {
		def loop(l: List[Double], n: Int, u: Double): (Int, Double) = l match {
			case Nil => (n, u / n)
			case h :: t => loop(t, n, u + h)
		}
		loop(l, l.length, 0.0)
	}
	def stdev(l: List[Double], n: Int, u: Double): Double = {
		def loop2(l: List[Double], n: Int, u: Double, sd: Double): Double = l match {
			case Nil => math.sqrt(sd / (n - 1))
			case h :: t => loop2(t, n, u, sd + math.pow(h - u, 2))
		}
		loop2(l, l.length, u, 0.0)	
	}
	def zscores(l: List[Double], u: Double, sd: Double): List[(Double, Double)] = {
		def loop3(l: List[Double], u: Double, sd: Double): List[(Double, Double)] = l match {
			case Nil => Nil
			case h :: t => List((h, (h - u) / sd)) ::: loop3(t, u, sd)
		}
		loop3(l, u, sd)
	}
	//val l = List(23.2, 65.5, 29.7, 65.5, 83.2, 13.9, 50.8, 1.0)
	//val (n, u) = Exercises.countAndMean(l) // = (8, 41.6)
	//val sd = Exercises.stdev(l, n, u) // = 28.917221759458748
	//Exercises.zscores(l, u, sd) // = List((23.2,-0.636..), (65.5,0.826..), ...)


	// 35 - Pearson correlation
	def pearson(L1: List[Int], L2: List[Int]): Double = {

		def calmean(L: List[Int]): Double = {
    		def counting(L: List[Int], sum: Int = 0, counter: Int = 0): Double = {
      			if (L == Nil) sum / counter 
      			else counting(L.tail, sum + L.head, counter + 1)
    		}
    		counting(L)
  		}

  		def numerator(L1: List[Int], L2:List[Int], ans: Double = 0, L1m: Double, L2m: Double): Double = {
    		if (L1 == Nil) ans 
    		else numerator(L1.tail, L2.tail, ans + ((L1.head - L1m) * (L2.head - L2m)), L1m, L2m)
  		}

  		def denominator(L1: List[Int], L2: List[Int], sum1: Double = 0, sum2: Double = 0, L1m: Double, L2m: Double): Double = {
    		if (L1 == Nil) math.sqrt(sum1) * math.sqrt(sum2) 
    		else denominator(L1.tail, L2.tail, sum1 + math.pow(L1.head - L1m, 2), sum2 + math.pow(L2.head - L2m, 2), L1m, L2m)
  		}
  
  		numerator(L1, L2, 0, calmean(L1), calmean(L2)) / denominator(L1, L2, 0, 0, calmean(L1), calmean(L2))
	}
	//val l1 = 1 to 10 toList
	//val l2 = (1 to 5 toList) ++: (1 to 5 toList).reverse
	//Exercises.pearson(l1, l1) // = 1.0
	//Exercises.pearson(l1, l1.reverse) // = -1.0
	//Exercises.pearson(l1, l2) // = 0.0


	// 36 - Transpose
	val transpose: (Matrix => Matrix) = (m: Matrix) => {

		/* getHeads: get the first elements of sublists, 
		return it in as a Row of the transposed matrix */
		def getHeads(m: Matrix): Row = {
			if (m.isEmpty || m.head.isEmpty) Nil
	  		else m.head.head :: getHeads(m.tail)			
		}

		/* delHeads: return the matrix without the 
		first elements of sublists */
		def delHeads(m: Matrix): Matrix = {
			if (m.isEmpty || m.head.isEmpty) Nil
	  		else m.head.tail :: delHeads(m.tail)			
		}

		if (m.isEmpty || m.head.isEmpty) Nil
		else List(getHeads(m)) ::: transpose(delHeads(m))
	}
	//val m: Matrix = List(1 :: 2 :: Nil, 3 :: 4 :: Nil, 5 :: 6 :: Nil)
	//val mt: Matrix = Exercises.transpose(m)
	//m.map(_.mkString(" ")).foreach(println)
	// 1 2
	// 3 4
	// 5 6
	//mt.map(_.mkString(" ")).foreach(println)
	// 1 3 5
	// 2 4 6


	// 37 - Matrix multiplication
	val multiply: ((Matrix, Matrix) => Matrix) = (a: Matrix, b: Matrix) => {

		/* exactly same transpose as above, had to 
		use "lazy" else it doesn't compile */    	
		lazy val transpose: (Matrix => Matrix) = (m: Matrix) => {

			def getHeads(m: Matrix): Row = {
				if (m.isEmpty || m.head.isEmpty) Nil
		  		else m.head.head :: getHeads(m.tail)			
			}

			def delHeads(m: Matrix): Matrix = {
				if (m.isEmpty || m.head.isEmpty) Nil
		  		else m.head.tail :: delHeads(m.tail)			
			}

			if (m.isEmpty || m.head.isEmpty) Nil
			else List(getHeads(m)) ::: transpose(delHeads(m))
		}

		/* calculate all the elements of one row for the 
		result matrix */
		def findRow(m1: Matrix, m2: Matrix): Row = {

			/* calculate one element of the result 
			matrix: a1*b1+a2*b2... */
			def findElement(r1: Row, r2: Row): Int = {
				if (r1.isEmpty && r2.isEmpty) 0
		  		else if (r1.isEmpty || r2.isEmpty) {
		  			require(r1.length - r2.length == 0, "Matrices cannot be multiplied, wrong dimensions.")
		  			-1
		  		}
		  		else r1.head * r2.head + findElement(r1.tail, r2.tail) 
			}

			if (m2.isEmpty) Nil
		  	else findElement(m1.head, m2.head) :: findRow(m1, m2.tail)
		}

		if (a.isEmpty || b.isEmpty) Nil
		else List(findRow(a, transpose(b))) ::: multiply(a.tail, b)
	}
	//val a: Matrix = List(1 :: 2 :: 3 :: Nil, 4 :: 5 :: 6 :: Nil)
	//val b: Matrix = List(7 :: 8 :: Nil, 9 :: 10 :: Nil, 11 :: 12 :: Nil)
	//val ab: Matrix = Exercises.multiply(a, b)
	//ab.map(_.mkString(" ")).foreach(println)
	// 58 64
	// 139 154

}